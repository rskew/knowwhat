module GraphComponent where

import Prelude

import AppState (AppState, AppStateInner(..), UninitializedAppStateInner, DrawingEdge, DrawingEdgeId, GraphSpacePos(..), HoveredElementId(..), PageSpacePos(..), Shape, _drawingEdgePos, _drawingEdges, _graph, _graphNodePos, _synthState, _synthNodeState, _hoveredElementId, _graphOrigin, _boundingRect, _zoom, appStateVersion, drawingEdgeKey, edgeIdStr, toGraphSpace)
import AppState.Foreign (appStateFromJSON, appStateToJSON)
import Audio.WebAudio.BaseAudioContext (newAudioContext)
import Audio.WebAudio.GainNode (setGain) as WebAudio
import Audio.WebAudio.Types (AudioContext) as WebAudio
import ContentEditable.SVGComponent as SVGContentEditable
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT, lift)
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (for_, foldM)
import Data.Identity (Identity(..))
import Data.Int (toNumber)
import Data.Lens (preview, traversed, (.~), (?~), (^.), (^?))
import Data.Lens.At (at)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.String (joinWith)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Undoable (initUndoable, dooM, undoM, redoM, _current, _history, _undone)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Console (log)
import Effect.Now (now)
import Foreign (renderForeignError)
import Foreign as Foreign
import Halogen as H
import Halogen.Component.Utils.Drag as Drag
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import Math as Math
import Point2D (Point2D)
import Svg.Attributes as SA
import Svg.Elements as SE
import Svg.Elements.Keyed as SK
import Web.Event.Event as WE
import Web.Event.EventTarget as ET
import Web.File.File as File
import Web.File.FileList as FileList
import Web.File.FileReader as FileReader
import Web.HTML as WH
import Web.HTML.Event.EventTypes as WHET
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as WHE
import Web.HTML.HTMLInputElement as WHIE
import Web.HTML.Window as WHW
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET
import Web.UIEvent.MouseEvent as ME
import Web.UIEvent.WheelEvent as WhE
import Workflow.Core (EdgeId, NodeId, _edgeId, _nodeId, _nodes, _source, _subgraph, _target, allEdges, lookupChildren, lookupNode, lookupEdge, lookupParents)
import Workflow.Synth (SynthState, SynthNodeState(..), SynthNodeParams(..), interpretSynth, deleteSynthNode, tryCreateSynthNode, _synthNodeGain, _gainNode)
import Workflow.UIGraph (freshUIEdge, freshUINode, graphTitle)
import Workflow.UIGraph.Types (UIGraph, UINode, UIEdge, Focus(..), _pos, _nodeText, _edgeText, _focus)
import Workflow.UIGraph.UIGraphOp (UIGraphOp(..), UIGraphOpF(..), Free(..), deleteEdgeOp, deleteNodeOp, insertEdgeOp, insertNodeOp, moveNodeOp, updateEdgeTextOp, updateNodeTextOp)


foreign import loadFile :: Effect Unit
foreign import saveJSON :: String -> String -> Effect Unit


nodeRadius :: Number
nodeRadius = 7.0

groupNodeRadius :: Number
groupNodeRadius = 12.0

nodeBorderRadius :: Number
nodeBorderRadius = 28.0

haloRadius :: Number
haloRadius = 40.0

nodeTextBoxOffset :: Point2D
nodeTextBoxOffset = { x : 20.0, y : - 10.0 }

amplifierTextBoxOffset :: Point2D
amplifierTextBoxOffset = { x : -30.0, y : -50.0 }

amplifierBoxSize :: Number
amplifierBoxSize = 50.0

amplifierHaloWidth :: Number
amplifierHaloWidth = 15.0

edgeTextBoxOffset :: Point2D
edgeTextBoxOffset = { x : 10.0, y : - 20.0 }

zoomScaling :: Number
zoomScaling = 0.01

defaultTextFieldShape :: Shape
defaultTextFieldShape = { width : 100.0, height : 50.0 }

maxTextFieldShape :: Shape
maxTextFieldShape = { width : 700.0, height : 500.0 }

type Input = { boundingRect :: WHE.DOMRect
             , graph :: UIGraph
             , audioContext :: WebAudio.AudioContext
             }

initialState :: Input -> AppState
initialState inputs = initUndoable
  $ AppStateInner
    { graph : inputs.graph
    , drawingEdges : Map.empty
    , hoveredElementId : Nothing
    , boundingRect : inputs.boundingRect
    , graphOrigin : PageSpacePos { x : 0.0, y : 0.0 }
    , zoom : 1.0
    , synthState : { audioContext : inputs.audioContext
                   , synthNodes : Map.empty
                   }
    }

data Action
  = PreventDefault WE.Event Action
  | StopPropagation WE.Event Action
  | Init
  | UpdateContentEditableText
  | BackgroundDragStart PageSpacePos ME.MouseEvent
  | BackgroundDragMove Drag.DragEvent PageSpacePos H.SubscriptionId
  | NodeDragStart NodeId GraphSpacePos ME.MouseEvent
  | NodeDragMove Drag.DragEvent NodeId GraphSpacePos H.SubscriptionId
  | EdgeDrawStart DrawingEdgeId ME.MouseEvent
  | EdgeDrawMove Drag.DragEvent DrawingEdgeId H.SubscriptionId
  | NodeTextInput NodeId SVGContentEditable.Message
  | EdgeTextInput EdgeId SVGContentEditable.Message
  | AppCreateNode ME.MouseEvent
  | AppDeleteNode UINode
  | AppCreateEdge UIEdge
  | AppDeleteEdge UIEdge
  | FocusOn Focus
  | DeleteFocus
  | Hover (Maybe HoveredElementId)
  | Zoom WhE.WheelEvent
  | CenterGraphOrigin
  | FetchLocalFile WE.Event
  | LoadLocalFile FileReader.FileReader H.SubscriptionId WE.Event
  | SaveLocalFile
  | Keypress KE.KeyboardEvent
  | DoNothing
  | GainDragStart NodeId Number ME.MouseEvent
  | GainDragMove Drag.DragEvent NodeId Number H.SubscriptionId

data Query a = UpdateBoundingRect a

data Message
  = Focused Focus
  | DrawEdge DrawingEdge
  | MappingMode

-- Slot type for parent components using a child GraphComponent
type Slot = H.Slot Query Message

type Slots =
  ( nodeTextField :: SVGContentEditable.Slot NodeId
  , edgeTextField :: SVGContentEditable.Slot EdgeId
  )

_nodeTextField :: SProxy "nodeTextField"
_nodeTextField = SProxy

_edgeTextField :: SProxy "edgeTextField"
_edgeTextField = SProxy

graph :: H.Component HH.HTML Query Input Message Aff
graph =
  H.mkComponent
    { initialState : initialState
    , render : render
    , eval: H.mkEval (H.defaultEval { handleAction = handleAction
                                    , handleQuery = handleQuery
                                    , initialize   = Just Init
                                    })
    }
  where

  svgDefs :: H.ComponentHTML Action Slots Aff
  svgDefs = SE.defs []
            [ SE.marker
              [ SA.id "drawing-arrow"
              , SA.markerWidth 10.0
              , SA.markerHeight 10.0
              , SA.refX 3.0
              , SA.refY 5.0
              , SA.orient SA.AutoOrient
              , SA.markerUnits SA.UserSpaceOnUse
              ]
              [ SE.path [ SA.d [ SA.Abs (SA.M 0.0 0.0)
                               , SA.Abs (SA.L 10.0 5.0)
                               , SA.Abs (SA.L 0.0 10.0)
                               , SA.Abs SA.Z
                               ]
                        ]
              ]
            , SE.marker
              [ SA.id "arrow"
              , SA.markerWidth 10.0
              , SA.markerHeight 10.0
              , SA.refX 15.0
              , SA.refY 5.0
              , SA.orient SA.AutoOrient
              , SA.markerUnits SA.UserSpaceOnUse
              ]
              [ SE.path [ SA.d [ SA.Abs (SA.M 0.0 0.0)
                               , SA.Abs (SA.L 0.0 10.0)
                               , SA.Abs (SA.L 8.0 5.0)
                               , SA.Abs SA.Z
                               ]
                        ]
              ]
            , SE.marker
              [ SA.id "arrow-to-group"
              , SA.markerWidth 10.0
              , SA.markerHeight 10.0
              , SA.refX 19.0
              , SA.refY 5.0
              , SA.orient SA.AutoOrient
              , SA.markerUnits SA.UserSpaceOnUse
              ]
              [ SE.path [ SA.d [ SA.Abs (SA.M 0.0 0.0)
                               , SA.Abs (SA.L 0.0 10.0)
                               , SA.Abs (SA.L 8.0 5.0)
                               , SA.Abs SA.Z
                               ]
                        ]
              ]
            ]

  renderGraphNode :: UINode -> AppState -> H.ComponentHTML Action Slots Aff
  renderGraphNode node state =
    let
      hoveredOverBorder = (state ^. _hoveredElementId) == (Just $ NodeBorderId $ node ^. _nodeId)
      hoveredOverHalo = (state ^. _hoveredElementId) == (Just $ NodeHaloId $ node ^. _nodeId)
      noDrawingEdgeFromNode =
        Map.isEmpty (Map.filter (\drawingEdge -> drawingEdge.source == node ^. _nodeId) (state ^. _drawingEdges))
      existsDrawingEdgeHoveredOverNode =
        (0 < Map.size (Map.filter ((flip drawingEdgeWithinNodeHalo) node) (state ^. _drawingEdges)))
      drawingEdgeOverNode =
        (hoveredOverBorder || hoveredOverHalo)
        &&
        noDrawingEdgeFromNode
        &&
        existsDrawingEdgeHoveredOverNode
      nodeClasses =
        joinWith " " $ Array.catMaybes
        [ Just "node"
        , if state ^. _graph <<< _focus == FocusNode (node ^. _nodeId)
          then Just "focused"
          else Nothing
        ]
      nodeBorderClasses =
          joinWith " " $ Array.catMaybes
          [ Just "nodeBorder"
          -- TODO: decide if grouping/ungrouping is useful for ologs
          --, if (Set.member (node ^. _nodeId) (state ^. _graph <<< _highlighted))
          --  then Just "highlighted"
          --  else Nothing
          , if hoveredOverBorder
               &&
               (not drawingEdgeOverNode)
            then Just "hovered"
            else Nothing
          ]
      haloClasses =
        joinWith " " $ Array.catMaybes
        [ Just "nodeHalo"
        , if state ^. _graph <<< _focus == FocusNode (node ^. _nodeId)
             &&
             (not hoveredOverBorder)
          then Just "focused"
          else Nothing
        , if drawingEdgeOverNode
          then Just "ready"
          else Nothing
        ]
      textBoxHTML textBoxOffset =
        [ SE.g
          [ SA.transform [ SA.Translate textBoxOffset.x textBoxOffset.y ]
          , HE.onMouseDown \e -> Just
                                 $ StopPropagation (ME.toEvent e)
                                 $ DoNothing
          ]
          [ HH.slot
            _nodeTextField
            (node ^. _nodeId)
            SVGContentEditable.svgContenteditable
            { shape : defaultTextFieldShape
            , initialText : (node ^. _nodeText)
            , maxShape : maxTextFieldShape
            , fitContentDynamic : true
            }
            (Just <<< NodeTextInput (node ^. _nodeId))
          ]
        ]
      graphNodeHTML =
        -- Node Halo, for creating edges from
        [ SE.circle
          [ SA.class_ haloClasses
          , SA.r haloRadius
          , HE.onMouseDown \e -> Just $ StopPropagation (ME.toEvent e)
                                 $ EdgeDrawStart (node ^. _nodeId) e
          , HE.onMouseEnter \_ -> Just $ Hover $ Just $ NodeHaloId $ node ^. _nodeId
          , HE.onMouseLeave \_ -> Just $ Hover Nothing
          ]
          -- Node center
        , SE.circle
          [ SA.class_ nodeClasses
          , SA.r $ if (not (Map.isEmpty (node ^. _subgraph <<< _nodes)))
                   then groupNodeRadius
                   else nodeRadius
          ]
          -- Node border, for grabbing
        , SE.circle
          [ SA.class_ nodeBorderClasses
          , SA.r nodeBorderRadius
          , HE.onMouseDown \e -> Just
                                 $ StopPropagation (ME.toEvent e)
                                 $ NodeDragStart (node ^. _nodeId) (GraphSpacePos (node ^. _pos)) e
          , HE.onDoubleClick \e -> Just $ StopPropagation (ME.toEvent e)
                                   $ AppDeleteNode node
          , HE.onMouseEnter \_ -> Just $ Hover $ Just $ NodeBorderId $ node ^. _nodeId
          , HE.onMouseLeave \_ -> Just $ Hover Nothing
          ]
        ] <> textBoxHTML nodeTextBoxOffset
      amplifierNodeHTML gain =
        -- Node Halo, for creating edges from
        [ SE.path [ let
                      b = amplifierHaloWidth + amplifierBoxSize / 2.0
                    in
                      SA.d [ SA.Abs (SA.M (-b) (-b))
                           , SA.Abs (SA.L b (-b))
                           , SA.Abs (SA.L b b)
                           , SA.Abs (SA.L (-b) b)
                           , SA.Abs SA.Z
                           ]
                  , SA.class_ haloClasses
                  , HE.onMouseDown \e -> Just $ StopPropagation (ME.toEvent e)
                                         $ EdgeDrawStart (node ^. _nodeId) e
                  , HE.onMouseEnter \_ -> Just $ Hover $ Just $ NodeHaloId $ node ^. _nodeId
                  , HE.onMouseLeave \_ -> Just $ Hover Nothing
                  ]
          -- Draggable amplifier control
        , SE.path [ SA.class_ nodeClasses
                  , let
                      b = amplifierBoxSize / 2.0
                      bHorn = 2.0 * b
                      bHornGain = 2.0 * b * gain
                    in
                      SA.d [ SA.Abs (SA.M (-b) (-b))
                           , SA.Abs (SA.L b (-b))
                           , SA.Abs (SA.L bHorn (-bHornGain))
                           , SA.Abs (SA.L bHorn bHornGain)
                           , SA.Abs (SA.L b b)
                           , SA.Abs (SA.L (-b) b)
                           , SA.Abs SA.Z
                           ]
                  , HE.onMouseDown \e -> Just
                                         $ StopPropagation (ME.toEvent e)
                                         $ GainDragStart (node ^. _nodeId) gain e
                  ]
          -- Node border, for grabbing
        , SE.path [ let
                       b = amplifierBoxSize / 2.0
                    in
                       SA.d [ SA.Abs (SA.M (-b) (-b))
                            , SA.Abs (SA.L b (-b))
                            , SA.Abs (SA.L b b)
                            , SA.Abs (SA.L (-b) b)
                            , SA.Abs SA.Z
                            ]
                  , SA.class_ nodeBorderClasses
                  , HE.onMouseDown \e -> Just
                                         $ StopPropagation (ME.toEvent e)
                                         $ NodeDragStart (node ^. _nodeId) (GraphSpacePos (node ^. _pos)) e
                  , HE.onDoubleClick \e -> Just $ StopPropagation (ME.toEvent e)
                                           $ AppDeleteNode node
                  , HE.onMouseEnter \_ -> Just $ Hover $ Just $ NodeBorderId $ node ^. _nodeId
                  , HE.onMouseLeave \_ -> Just $ Hover Nothing
                  ]
        ] <> textBoxHTML amplifierTextBoxOffset
      nodeHTML = case Map.lookup (node ^. _nodeId) (state ^. _synthState).synthNodes of
        Nothing -> graphNodeHTML
        Just (SynthNodeState synthNodeState) -> case synthNodeState.synthNodeParams of
          AmplifierParams gain -> amplifierNodeHTML gain
          _ -> graphNodeHTML
    in
      SE.g
      [ SA.transform [ SA.Translate (node ^. _pos).x (node ^. _pos).y ] ]
      nodeHTML

  renderEdge :: UIEdge -> AppState -> Maybe (H.ComponentHTML Action Slots Aff)
  renderEdge edge state = do
    sourceNode <- Map.lookup (edge ^. _source) $ state ^. _graph <<< _nodes
    targetNode <- Map.lookup (edge ^. _target) $ state ^. _graph <<< _nodes
    let
      focused = state ^. _graph <<< _focus == FocusEdge (edge ^. _edgeId) []
      edgeClasses =
        joinWith " " $ Array.catMaybes
        [ Just "edge"
        , if focused
          then Just "focused"
          else Nothing
        ]
      edgeMidPos =
        { x : ((sourceNode ^. _pos).x + (targetNode ^. _pos).x) / 2.0
        , y : ((sourceNode ^. _pos).y + (targetNode ^. _pos).y) / 2.0
        }
    pure $
      SE.g [] $
      -- Edge
      [ SE.line
        [ SA.class_ edgeClasses
        , SA.x1 (sourceNode ^. _pos).x
        , SA.y1 (sourceNode ^. _pos).y
        , SA.x2 (targetNode ^. _pos).x
        , SA.y2 (targetNode ^. _pos).y
        , SA.markerEnd $ if (Map.isEmpty (targetNode ^. _subgraph <<< _nodes))
                         then "url(#arrow)"
                         else "url(#arrow-to-group)"
        ]
      ] <> if not focused && ((edge ^. _edgeText) == "") then [] else
      -- Edge Textbox
      [ SE.g
        [ SA.transform [ SA.Translate
                         (edgeMidPos.x + edgeTextBoxOffset.x)
                         (edgeMidPos.y + edgeTextBoxOffset.y)
                       ]
        , HE.onMouseDown \e -> Just
                               $ StopPropagation (ME.toEvent e)
                               $ DoNothing
        ]
        [ HH.slot
          _edgeTextField
          (edge ^. _edgeId)
          SVGContentEditable.svgContenteditable
          { shape : defaultTextFieldShape
          , initialText : (edge ^. _edgeText)
          , maxShape : maxTextFieldShape
          , fitContentDynamic : true
          }
          (Just <<< EdgeTextInput (edge ^. _edgeId))
        ]
      ]

  renderEdgeBorder :: UIEdge -> AppState -> Maybe (H.ComponentHTML Action Slots Aff)
  renderEdgeBorder edge state = do
    sourceNode <- Map.lookup (edge ^. _source) $ state ^. _graph <<< _nodes
    targetNode <- Map.lookup (edge ^. _target) $ state ^. _graph <<< _nodes
    let
      edgeBorderClasses =
        joinWith " " $ Array.catMaybes
        [ Just "edgeBorder"
        , if (state ^. _hoveredElementId) == (Just $ EdgeBorderId $ edge ^. _edgeId)
          then Just "hover"
          else Nothing
        ]
    pure $
      SE.line
      [ SA.class_ edgeBorderClasses
      , SA.x1 (sourceNode ^. _pos).x
      , SA.y1 (sourceNode ^. _pos).y
      , SA.x2 (targetNode ^. _pos).x
      , SA.y2 (targetNode ^. _pos).y
      , SA.strokeLinecap SA.Butt
      , HE.onClick \_ -> Just $ FocusOn $ FocusEdge (edge ^. _edgeId) []
      , HE.onMouseEnter \_ -> Just $ Hover $ Just $ EdgeBorderId $ edge ^. _edgeId
      , HE.onMouseLeave \_ -> Just $ Hover Nothing
      , HE.onDoubleClick \e -> Just $ StopPropagation (ME.toEvent e)
                               $ AppDeleteEdge $ edge
      ]

  renderDrawingEdge :: DrawingEdge -> AppState -> Maybe (H.ComponentHTML Action Slots Aff)
  renderDrawingEdge drawingEdgeState state = do
    sourcePos <- preview (_graphNodePos drawingEdgeState.source) state
    let
      GraphSpacePos sourceGraphPos = sourcePos
      GraphSpacePos drawingEdgeGraphPos = drawingEdgeState.pos
      drawingEdgeClasses =
        joinWith " " $ Array.catMaybes
        [ Just "edge" ]
    pure $
      SE.line
      [ SA.class_ drawingEdgeClasses
      , SA.x1 sourceGraphPos.x
      , SA.y1 sourceGraphPos.y
      , SA.x2 drawingEdgeGraphPos.x
      , SA.y2 drawingEdgeGraphPos.y
      , SA.markerEnd "url(#drawing-arrow)"
      ]

  render :: AppState -> H.ComponentHTML Action Slots Aff
  render state =
    let
      keyedNodes = map
                   (\node ->
                     Tuple (show (node ^. _nodeId)) $ renderGraphNode node state
                   )
                   $ Array.fromFoldable $ Map.values $ state ^. _graph <<< _nodes
      keyedEdges = Array.mapMaybe
                   (\edge -> do
                       edgeHTML <- renderEdge edge state
                       pure $ Tuple (edgeIdStr edge) edgeHTML
                   )
                   $ Array.fromFoldable $ allEdges $ state ^. _graph
      keyedEdgeBorders = Array.mapMaybe
                     (\edge -> do
                         edgeHTML <- renderEdgeBorder edge state
                         pure $ Tuple ((edgeIdStr edge) <> "_border") edgeHTML
                     )
                     $ Array.fromFoldable $ allEdges $ state ^. _graph
      keyedDrawingEdges = Array.mapMaybe
                     (\drawingEdgeState -> do
                         drawingEdgeHTML <- renderDrawingEdge drawingEdgeState state
                         pure $ Tuple (drawingEdgeKey drawingEdgeState.source) drawingEdgeHTML
                     )
                     $ Array.fromFoldable $ Map.values (state ^. _drawingEdges)
      PageSpacePos graphOrigin = (state ^. _graphOrigin)
    in
      HH.div
      [ HP.ref (H.RefLabel "svg") ]
      [ SE.svg
        [ SA.viewBox
          -- scale then shift
          ( - graphOrigin.x * (state ^. _zoom) )
          ( - graphOrigin.y * (state ^. _zoom) )
          ( (state ^. _boundingRect).width * (state ^. _zoom) )
          ( (state ^. _boundingRect).height * (state ^. _zoom) )
        , HE.onMouseDown \e -> Just
                               $ StopPropagation (ME.toEvent e)
                               $ BackgroundDragStart (state ^. _graphOrigin) e
        , HE.onDoubleClick \e -> Just
                                 $ StopPropagation (ME.toEvent e)
                                 $ AppCreateNode e
        , HE.onWheel $ Just <<< Zoom
        ]
        [ svgDefs
        , SK.g
          []
          (
           keyedEdges
           <> keyedEdgeBorders
           <> keyedDrawingEdges
           <> keyedNodes
          )
        ]
      , HH.input
        [ HP.type_ InputFile
        , HE.onChange $ Just <<< FetchLocalFile
        ]
      ]

  handleAction :: Action -> H.HalogenM AppState Action Slots Message Aff Unit
  handleAction = case _ of
    PreventDefault e next -> do
      H.liftEffect $ WE.preventDefault e
      handleAction next

    StopPropagation e next -> do
      H.liftEffect $ WE.stopPropagation e
      handleAction next

    Init -> do
      _ <- handleQuery $ UpdateBoundingRect \_ -> unit
      -- Add keyboard event listener to body
      document <- H.liftEffect $ WHW.document =<< WH.window
      _ <- H.subscribe $ ES.eventListenerEventSource KET.keydown (HTMLDocument.toEventTarget document) (map Keypress <<< KE.fromEvent)
      pure unit

    UpdateContentEditableText -> do
      state <- H.get
      for_ (state ^. _graph <<< _nodes) \node -> do
        H.query _nodeTextField (node ^. _nodeId) $ H.tell $ SVGContentEditable.SetText $ node ^. _nodeText
      for_ (allEdges (state ^. _graph)) \edge -> do
        H.query _edgeTextField (edge ^. _edgeId) $ H.tell $ SVGContentEditable.SetText $ edge ^. _edgeText

    NodeTextInput nodeId (SVGContentEditable.TextUpdate text) -> do
      state <- H.get
      case lookupNode (state ^. _graph) nodeId of
        Nothing -> pure unit
        Just node -> do
          modifyM $ dooM $ updateNodeTextOp node text

    EdgeTextInput edgeId (SVGContentEditable.TextUpdate text) -> do
      state <- H.get
      case lookupEdge (state ^. _graph) edgeId of
        Nothing -> pure unit
        Just edge -> do
          modifyM $ dooM $ updateEdgeTextOp edge text

    BackgroundDragStart initialGraphOrigin mouseEvent -> do
      H.modify_ $ _graph <<< _focus .~ NoFocus
      H.subscribe' \subscriptionId ->
        Drag.dragEventSource mouseEvent
          \e -> BackgroundDragMove e initialGraphOrigin subscriptionId

    BackgroundDragMove (Drag.Move _ dragData) (PageSpacePos initialGraphOrigin) _ -> do
      state <- H.get
      let newGraphOrigin = PageSpacePos { x : initialGraphOrigin.x + dragData.offsetX
                                        , y : initialGraphOrigin.y + dragData.offsetY
                                        }
      H.modify_ $ _graphOrigin .~ newGraphOrigin

    BackgroundDragMove (Drag.Done _) _ subscriptionId ->
      H.unsubscribe subscriptionId

    NodeDragStart nodeId initialNodePos mouseEvent -> do
      H.subscribe' \subscriptionId ->
        Drag.dragEventSource mouseEvent
          \e -> NodeDragMove e nodeId initialNodePos subscriptionId
      H.modify_ $ _graph <<< _focus .~ FocusNode nodeId

    NodeDragMove (Drag.Move _ dragData) nodeId (GraphSpacePos initialNodePos) _ -> do
      state <- H.get
      let
        dragOffsetGraphSpace = { x : dragData.offsetX * (state ^. _zoom)
                               , y : dragData.offsetY * (state ^. _zoom)
                               }
        newNodePos = { x : initialNodePos.x + dragOffsetGraphSpace.x
                     , y : initialNodePos.y + dragOffsetGraphSpace.y
                     }
      case lookupNode (state ^. _graph) nodeId of
        Nothing -> pure unit
        Just node -> do
          modifyM $ dooM $ moveNodeOp node newNodePos

    NodeDragMove (Drag.Done _) _ _ subscriptionId ->
      H.unsubscribe subscriptionId

    EdgeDrawStart drawingEdgeId mouseEvent -> do
      state <- H.get
      let
        mouseGraphPos = mouseEventPosition mouseEvent
                        # toGraphSpace (state ^. _boundingRect) (state ^. _graphOrigin) (state ^. _zoom)
        drawingEdgeSourceId = drawingEdgeId
      H.modify_ $ (_drawingEdges <<< at drawingEdgeId ?~ { pos : mouseGraphPos
                                                         , source : drawingEdgeSourceId
                                                         })
                >>> (_graph <<< _focus .~ FocusNode drawingEdgeId)
      H.subscribe' \subscriptionId ->
        Drag.dragEventSource mouseEvent $ \e -> EdgeDrawMove e drawingEdgeId subscriptionId

    EdgeDrawMove (Drag.Move _ dragData) drawingEdgeId _ -> do
      state <- H.get
      let dragGraphPos = PageSpacePos { x : dragData.x, y : dragData.y }
                         # toGraphSpace (state ^. _boundingRect) (state ^. _graphOrigin) (state ^. _zoom)
      H.modify_ $ _drawingEdgePos drawingEdgeId .~ dragGraphPos

    EdgeDrawMove (Drag.Done _) drawingEdgeId subscriptionId -> do
      -- Create a new edge if the edge is drawn within the halo of another node
      state <- H.get
      let
        maybeNewEdgeTarget = do
          drawingEdgeState <- Map.lookup drawingEdgeId $ state ^. _drawingEdges
          Map.values (state ^. _graph <<< _nodes)
           # (List.filter (drawingEdgeWithinNodeHalo drawingEdgeState))
           # List.head
        drawingEdgeSourceId = drawingEdgeId
      case maybeNewEdgeTarget of
        Just newEdgeTarget ->
          let
            newEdge = freshUIEdge { source : drawingEdgeSourceId, target : newEdgeTarget ^. _nodeId }
          in
          handleAction $ AppCreateEdge newEdge
        Nothing -> pure unit
      -- Remove the drawing edge
      H.modify_ $ _drawingEdges <<< at drawingEdgeId .~ Nothing
      H.unsubscribe subscriptionId

    AppCreateNode mouseEvent -> do
      newNode <- H.liftEffect freshUINode
      state <- H.get
      let
        GraphSpacePos newPos = mouseEventPosition mouseEvent
                               # toGraphSpace (state ^. _boundingRect) (state ^. _graphOrigin) (state ^. _zoom)
        newNode' = newNode # _pos .~ newPos
                           # _nodeText .~ "new node hey"
      modifyM $ dooM $ insertNodeOp newNode'
      handleAction $ FocusOn (FocusNode $ newNode' ^. _nodeId)

    AppDeleteNode node -> do
      modifyM $ dooM $ deleteNodeOp node
      state <- H.get
      case Set.findMin ((lookupParents (state ^. _graph) node) <> (lookupChildren (state ^. _graph) node)) of
        Just neighbor -> handleAction $ FocusOn (FocusNode (neighbor ^. _nodeId))
        Nothing -> pure unit

    AppCreateEdge edge -> do
      modifyM $ dooM $ insertEdgeOp edge
      handleAction $ FocusOn (FocusEdge (edge ^. _edgeId) [])

    AppDeleteEdge edge -> do
      modifyM $ dooM $ deleteEdgeOp edge
      handleAction $ FocusOn (FocusNode (edge ^. _edgeId).source)

    FocusOn newFocus -> do
      H.modify_ $ _graph <<< _focus .~ newFocus

    DeleteFocus -> do
      state <- H.get
      case state ^. _graph <<< _focus of
        NoFocus -> pure unit
        FocusNode nodeId -> case lookupNode (state ^. _graph) nodeId of
          Nothing -> pure unit
          Just node -> handleAction $ AppDeleteNode node
        FocusEdge edgeId _ -> case lookupEdge (state ^. _graph) edgeId of
          Nothing -> pure unit
          Just edge -> handleAction $ AppDeleteEdge edge

    Hover maybeElementId -> do
      H.modify_ $ _hoveredElementId .~ maybeElementId

    -- | Zoom in/out holding the mouse position invariant in graph space
    -- |
    -- | p = mousePagePos
    -- | prevMouseGraphPos = (p - oldGraphOrigin) * oldZoom
    -- | newMouseGraphPos = (p - newGraphOrigin) * newZoom
    -- | let newMouseGraphPos = prevMouseGraphPos
    -- | => newGraphOrigin = p - ((p - oldGraphOrigin) * (oldZoom / newZoom))
    -- | So if newZoom --> inf, newGraphOrigin --> mousePos, as expected.
    Zoom wheelEvent -> do
      state <- H.get
      let
        scaledZoom = (WhE.deltaY wheelEvent) * zoomScaling
        PageSpacePos mousePagePos = mouseEventPosition $ WhE.toMouseEvent wheelEvent
        PageSpacePos graphOrigin = state ^. _graphOrigin
        newZoom = Math.exp (scaledZoom + (Math.log (state ^. _zoom)))
        -- keep graph-space mouse pos invariant
        newGraphOriginDim mousePos oldGraphOrigin =
          mousePos - ((mousePos - oldGraphOrigin) * ((state ^. _zoom) / newZoom))
        newGraphOrigin = PageSpacePos
                         { x : newGraphOriginDim mousePagePos.x graphOrigin.x
                         , y : newGraphOriginDim mousePagePos.y graphOrigin.y
                         }
      H.modify_ $ (_zoom .~ newZoom)
                >>> (_graphOrigin .~ newGraphOrigin)

    CenterGraphOrigin -> do
      H.modify_ $ _graphOrigin .~ PageSpacePos { x : 0.0, y : 0.0 }

    FetchLocalFile changeEvent -> unit <$ runMaybeT do
      target <- MaybeT $ pure $ WE.target changeEvent
      inputElement <- MaybeT $ pure $ WHIE.fromEventTarget target
      files <- MaybeT $ H.liftEffect $ WHIE.files inputElement
      file <- MaybeT $ pure $ FileList.item 0 files
      fileReader <- lift $ H.liftEffect $ FileReader.fileReader
      lift $ H.liftEffect $ FileReader.readAsText (File.toBlob file) fileReader
      lift $ H.subscribe' \subscriptionId ->
        ES.effectEventSource \emitter -> do
          listener <- ET.eventListener \event -> do
            ES.emit emitter $ LoadLocalFile fileReader subscriptionId event
          ET.addEventListener WHET.load listener false $ FileReader.toEventTarget fileReader
          pure mempty

    LoadLocalFile fileReader subscriptionId event -> do
      exceptTJSON <- H.liftEffect $ FileReader.result fileReader
      let Identity eitherJSON = runExceptT $ Foreign.readString exceptTJSON
      case (eitherJSON
            # lmap (show <<< map renderForeignError))
           >>= appStateFromJSON of
        Left errors -> H.liftEffect $ log errors
        Right appStateWithMeta -> do
          appState <- H.get
          bareSynthState <- H.liftEffect $ tearDownSynthState appState
          let uninitializedAppStateInner = appStateWithMeta.uninitializedAppState ^. _current
          newAppStateCurrent <- H.liftEffect $ initializeSynthState uninitializedAppStateInner bareSynthState
          H.put $ initUndoable newAppStateCurrent
          -- Keep text fields in sync
          handleAction $ UpdateContentEditableText
      H.unsubscribe subscriptionId

    SaveLocalFile -> do
      state <- H.get
      timestamp <- H.liftEffect now
      let
        stateJSON = appStateToJSON state { version : appStateVersion
                                         , timestamp : timestamp
                                         }
        title = fromMaybe "untitled" $ (graphTitle (state ^. _graph))
      H.liftEffect $ saveJSON stateJSON $ title <> ".graph.json"

    Keypress keyboardEvent -> do
      H.liftEffect $ log $ show $ KE.key keyboardEvent
      case KE.key keyboardEvent of
        " " -> if not (KE.ctrlKey keyboardEvent) then pure unit else do
          H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
          H.liftEffect $ WE.stopPropagation $ KE.toEvent keyboardEvent
          H.liftEffect do
            log "beeping:"
            audioContext <- newAudioContext
            let synthState = { audioContext : audioContext, synthNodes : Map.empty }
            destinationNode <- freshUINode
            let destinationOp = UIGraphOp $ Free $ UpdateNodeText destinationNode "" "destination"
                                                 $ Pure unit
            log $ show $ ((Map.values synthState.synthNodes) <#> \(SynthNodeState sns) -> sns.synthNodeType)
            synthState' <- interpretSynth destinationOp synthState
            log $ show $ ((Map.values synthState'.synthNodes) <#> \(SynthNodeState sns) -> sns.synthNodeType)
            oscillatorNode <- freshUINode
            let oscillatorDestinationEdge = freshUIEdge { source : oscillatorNode ^. _nodeId, target : destinationNode ^. _nodeId }
            let oscillatorOp = UIGraphOp $ Free $ UpdateNodeText oscillatorNode "" "oscillator"
                                                $ Free $ InsertEdge oscillatorDestinationEdge
                                                       $ Pure unit
            synthState'' <- interpretSynth oscillatorOp synthState'
            log $ show $ ((Map.values synthState''.synthNodes) <#> \(SynthNodeState sns) -> sns.synthNodeType)
            log "beeping!"
        "z" -> if not (KE.ctrlKey keyboardEvent) then pure unit else do
          H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
          H.liftEffect $ WE.stopPropagation $ KE.toEvent keyboardEvent
          state <- H.get
          H.liftEffect $ log $ "Undoable history: " <> show (state ^. _history)
          modifyM undoM
          -- Keep text state in sync
          handleAction $ UpdateContentEditableText
        "y" -> if not (KE.ctrlKey keyboardEvent) then pure unit else do
          H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
          H.liftEffect $ WE.stopPropagation $ KE.toEvent keyboardEvent
          state <- H.get
          H.liftEffect $ log $ "Undoable undone: " <> show (state ^. _undone)
          modifyM redoM
          -- Keep text state in sync
          handleAction $ UpdateContentEditableText
        "l" -> if not (KE.ctrlKey keyboardEvent) then pure unit else do
          H.liftEffect $ loadFile
        "s" -> if not (KE.ctrlKey keyboardEvent) then pure unit else do
          -- Save app state as a local json file
          H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
          H.liftEffect $ WE.stopPropagation $ KE.toEvent keyboardEvent
          handleAction $ SaveLocalFile
        "o" -> if not (KE.ctrlKey keyboardEvent) then pure unit else do
          H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
          H.liftEffect $ WE.stopPropagation $ KE.toEvent keyboardEvent
          handleAction CenterGraphOrigin
        "Delete" ->
          handleAction $ DeleteFocus
        "Escape" ->
          handleAction $ FocusOn NoFocus
        "m" -> if not (KE.ctrlKey keyboardEvent) then pure unit else do
          H.raise MappingMode
        -- TODO: decide if grouping/ungrouping is useful for ologs
        --"s" -> H.modify_ $ _graph %~ toggleHighlightFocus
        --" " -> H.modify_ $ _graph %~ toggleGroupExpand
        _ -> pure unit

    DoNothing ->
      pure unit

    GainDragStart nodeId initialGain mouseEvent -> do
      H.subscribe' \subscriptionId ->
        Drag.dragEventSource mouseEvent
          \e -> GainDragMove e nodeId initialGain subscriptionId
      H.modify_ $ _graph <<< _focus .~ FocusNode nodeId

    GainDragMove (Drag.Move _ dragData) nodeId initialGain _ -> do
      state <- H.get
      let
        gainOffset = - dragData.offsetY * (state ^. _zoom)
        newGain = Math.max 0.0 $ initialGain + (1.0 / amplifierBoxSize) * gainOffset
      case Map.lookup nodeId (state ^. _synthState).synthNodes of
        Nothing -> pure unit
        Just synthNodeState -> do
          H.modify_ $ _synthNodeState nodeId <<< traversed <<< _synthNodeGain .~ newGain
          case synthNodeState ^? _gainNode of
            Nothing -> pure unit
            Just gainNode -> H.liftEffect $ WebAudio.setGain newGain gainNode

    GainDragMove (Drag.Done _) _ _ subscriptionId ->
      H.unsubscribe subscriptionId


  handleQuery :: forall a. Query a -> H.HalogenM AppState Action Slots Message Aff (Maybe a)
  handleQuery = case _ of
    UpdateBoundingRect a -> map (const a) <$> runMaybeT do
      svgElement <- MaybeT $ H.getHTMLElementRef (H.RefLabel "svg")
      svgRect <- lift $ H.liftEffect $ WHE.getBoundingClientRect svgElement
      lift $ H.modify_ $ _boundingRect .~ svgRect
      lift $ H.liftEffect $ log "updating bounding rect"


mouseEventPosition :: ME.MouseEvent -> PageSpacePos
mouseEventPosition e = PageSpacePos { x : toNumber $ ME.pageX e
                                    , y : toNumber $ ME.pageY e
                                    }

euclideanDistance :: GraphSpacePos -> GraphSpacePos -> Number
euclideanDistance (GraphSpacePos pos1) (GraphSpacePos pos2) =
  Math.sqrt
  $ (Math.pow (pos1.x - pos2.x) 2.0)
  + (Math.pow (pos1.y - pos2.y) 2.0)

drawingEdgeWithinNodeHalo :: DrawingEdge -> UINode -> Boolean
drawingEdgeWithinNodeHalo drawingEdgeState' node =
  haloRadius > euclideanDistance (GraphSpacePos (node ^. _pos)) drawingEdgeState'.pos


tearDownSynthState :: AppState -> Effect SynthState
tearDownSynthState appState =
  foldM
  (flip deleteSynthNode)
  (appState ^. _synthState)
  $ appState ^. _graph <<< _nodes

initializeSynthState :: UninitializedAppStateInner -> SynthState -> Effect AppStateInner
initializeSynthState uninitializedAppState bareSynthState = do
  newSynthState <- foldM
                   (flip tryCreateSynthNode)
                   bareSynthState
                   (uninitializedAppState.graph ^. _nodes)
  pure $ AppStateInner
         { synthState : newSynthState
         , graph : uninitializedAppState.graph
         , drawingEdges : uninitializedAppState.drawingEdges
         , hoveredElementId : uninitializedAppState.hoveredElementId
         , boundingRect : uninitializedAppState.boundingRect
         , graphOrigin : uninitializedAppState.graphOrigin
         , zoom : uninitializedAppState.zoom
         }

modifyM :: (AppState -> Effect AppState) -> H.HalogenM AppState Action Slots Message Aff Unit
modifyM op = do
  state <- H.get
  updatedState <- H.liftEffect $ op state
  H.put updatedState
