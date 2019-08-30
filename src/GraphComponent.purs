module GraphComponent where

import Prelude

import AppState (AppState, DrawingEdge, DrawingEdgeId, GraphSpacePos(..), HoveredElementId(..), PageSpacePos(..), Shape, _drawingEdgePos, _drawingEdges, _graph, _undoableGraph, _graphNodePos, _zoom, appStateVersion, drawingEdgeKey, edgeIdStr, toGraphSpace)
import AppState.Foreign (appStateFromJSON, appStateToJSON)
import ContentEditable.SVGComponent as SVGContentEditable
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT, lift)
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (for_, traverse_)
import Data.Identity (Identity(..))
import Data.Int (toNumber)
import Data.Lens (preview, (%~), (.~), (?~), (^.))
import Data.Lens.At (at)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Data.Undoable (initUndoable, doo, undo, redo, _history, _undone)
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
import Web.HTML.HTMLDocument (HTMLDocument)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as WHE
import Web.HTML.HTMLInputElement as WHIE
import Web.HTML.Window as WHW
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET
import Web.UIEvent.MouseEvent as ME
import Web.UIEvent.WheelEvent as WhE
import Workflow.Core (EdgeId, NodeId, _edgeId, _nodeId, _nodes, _source, _subgraph, _target, allEdges, lookupChildren, lookupNode, lookupEdge, lookupParents)
import Workflow.UIGraph (freshUIEdge, freshUINode, graphTitle)
import Workflow.UIGraph.Types (UIGraph, UINode, UIEdge, Focus(..), _pos, _nodeText, _edgeText, _focus)
import Workflow.UIGraph.UIGraphOp (deleteEdgeOp, deleteNodeOp, insertEdgeOp, insertNodeOp, moveNodeOp, updateEdgeTextOp, updateNodeTextOp)


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

edgeTextBoxOffset :: Point2D
edgeTextBoxOffset = { x : 10.0, y : - 20.0 }

zoomScaling :: Number
zoomScaling = 0.01

defaultTextFieldShape :: Shape
defaultTextFieldShape = { width : 100.0, height : 50.0 }

maxTextFieldShape :: Shape
maxTextFieldShape = { width : 700.0, height : 500.0 }

initialState :: { boundingRect :: WHE.DOMRect, graph :: UIGraph } -> AppState
initialState inputs =
  { graph : initUndoable inputs.graph
  , drawingEdges : Map.empty
  , hoveredElementId : Nothing
  , boundingRect : inputs.boundingRect
  , graphOrigin : PageSpacePos { x : 0.0, y : 0.0 }
  , zoom : 1.0
  }

data Query a
  = PreventDefault WE.Event (Query a)
  | StopPropagation WE.Event (Query a)
  | Init a
  | UpdateContentEditableText a
  | BackgroundDragStart PageSpacePos ME.MouseEvent a
  | BackgroundDragMove Drag.DragEvent PageSpacePos a
  | NodeDragStart NodeId GraphSpacePos ME.MouseEvent a
  | NodeDragMove Drag.DragEvent NodeId GraphSpacePos a
  | EdgeDrawStart DrawingEdgeId ME.MouseEvent a
  | EdgeDrawMove Drag.DragEvent DrawingEdgeId a
  | NodeTextInput NodeId SVGContentEditable.Message a
  | EdgeTextInput EdgeId SVGContentEditable.Message a
  | AppCreateNode ME.MouseEvent a
  | AppDeleteNode UINode a
  | AppCreateEdge UIEdge a
  | AppDeleteEdge UIEdge a
  | FocusOn Focus a
  | DeleteFocus a
  | Hover (Maybe HoveredElementId) a
  | Zoom WhE.WheelEvent a
  | CenterGraphOrigin a
  | FetchLocalFile WE.Event a
  | LoadLocalFile FileReader.FileReader WE.Event (H.SubscribeStatus -> a)
  | SaveLocalFile a
  | UpdateBoundingRect a
  | Keypress KE.KeyboardEvent (H.SubscribeStatus -> a)
  | DoNothing a

data Message
  = Focused Focus
  | DrawEdge DrawingEdge
  | MappingMode

type Input = { boundingRect :: WHE.DOMRect
             , graph :: UIGraph
             }

data Slot
  = NodeTextField NodeId
  | EdgeTextField EdgeId
derive instance eqTextFieldSlot :: Eq Slot
derive instance ordTextFieldSlot :: Ord Slot

graph :: H.Component HH.HTML Query Input Message Aff
graph =
  H.lifecycleParentComponent
    { initialState : initialState
    , render : render
    , eval : eval
    , receiver : const Nothing
    , initializer : Just $ H.action Init
    , finalizer : Nothing
    }
  where

  svgDefs :: H.ParentHTML Query SVGContentEditable.Query Slot Aff
  svgDefs = SE.defs
            [ SE.marker
              [ SA.id "drawing-arrow"
              , SA.markerWidth 10.0
              , SA.markerHeight 10.0
              , SA.refX $ SA.Offset 3.0
              , SA.refY $ SA.Offset 5.0
              , SA.orient SA.OrientAuto
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
              , SA.refX $ SA.Offset 15.0
              , SA.refY $ SA.Offset 5.0
              , SA.orient SA.OrientAuto
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
              , SA.refX $ SA.Offset 19.0
              , SA.refY $ SA.Offset 5.0
              , SA.orient SA.OrientAuto
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

  renderGraphNode :: UINode -> AppState -> H.ParentHTML Query SVGContentEditable.Query Slot Aff
  renderGraphNode node state =
    let
      hoveredOverBorder = state.hoveredElementId == (Just $ NodeBorderId $ node ^. _nodeId)
      hoveredOverHalo = state.hoveredElementId == (Just $ NodeHaloId $ node ^. _nodeId)
      noDrawingEdgeFromNode =
        Map.isEmpty (Map.filter (\drawingEdge -> drawingEdge.source == node ^. _nodeId) state.drawingEdges)
      existsDrawingEdgeHoveredOverNode =
        (0 < Map.size (Map.filter ((flip drawingEdgeWithinNodeHalo) node) state.drawingEdges))
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
    in
      SE.g
      [ SA.transform [ SA.Translate (node ^. _pos).x (node ^. _pos).y ] ]
      [ SE.circle
        -- Node Halo, for creating edges from
        [ SA.class_ haloClasses
        , SA.r haloRadius
        , HE.onMouseDown \e -> Just $ StopPropagation (ME.toEvent e)
                               $ H.action $ EdgeDrawStart (node ^. _nodeId) e
        , HE.onMouseEnter $ HE.input_ $ Hover $ Just $ NodeHaloId $ node ^. _nodeId
        , HE.onMouseLeave $ HE.input_ $ Hover Nothing
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
                               $ H.action $ NodeDragStart (node ^. _nodeId) (GraphSpacePos (node ^. _pos)) e
        , HE.onDoubleClick \e -> Just $ StopPropagation (ME.toEvent e)
                                 $ H.action $ AppDeleteNode node
        , HE.onMouseEnter $ HE.input_ $ Hover $ Just $ NodeBorderId $ node ^. _nodeId
        , HE.onMouseLeave $ HE.input_ $ Hover Nothing
        ]
      -- Node Textbox
      , SE.g
        [ SA.transform [ SA.Translate nodeTextBoxOffset.x nodeTextBoxOffset.y ]
        , HE.onMouseDown \e -> Just
                               $ StopPropagation (ME.toEvent e)
                               $ H.action DoNothing
        ]
        [ HH.slot
          (NodeTextField (node ^. _nodeId))
          SVGContentEditable.svgContenteditable
          { shape : defaultTextFieldShape
          , initialText : (node ^. _nodeText)
          , maxShape : maxTextFieldShape
          , fitContentDynamic : true
          }
          (HE.input (NodeTextInput (node ^. _nodeId)))
        ]
      ]

  renderEdge :: UIEdge -> AppState -> Maybe (H.ParentHTML Query SVGContentEditable.Query Slot Aff)
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
                         then SA.URL "#arrow"
                         else SA.URL "#arrow-to-group"
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
                               $ H.action DoNothing
        ]
        [ HH.slot
          (EdgeTextField (edge ^. _edgeId))
          SVGContentEditable.svgContenteditable
          { shape : defaultTextFieldShape
          , initialText : (edge ^. _edgeText)
          , maxShape : maxTextFieldShape
          , fitContentDynamic : true
          }
          (HE.input (EdgeTextInput (edge ^. _edgeId)))
        ]
      ]

  renderEdgeBorder :: UIEdge -> AppState -> Maybe (H.ParentHTML Query SVGContentEditable.Query Slot Aff)
  renderEdgeBorder edge state = do
    sourceNode <- Map.lookup (edge ^. _source) $ state ^. _graph <<< _nodes
    targetNode <- Map.lookup (edge ^. _target) $ state ^. _graph <<< _nodes
    let
      edgeBorderClasses =
        joinWith " " $ Array.catMaybes
        [ Just "edgeBorder"
        , if state.hoveredElementId == (Just $ EdgeBorderId $ edge ^. _edgeId)
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
      , HE.onClick $ HE.input_ $ FocusOn $ FocusEdge (edge ^. _edgeId) []
      , HE.onMouseEnter $ HE.input_ $ Hover $ Just $ EdgeBorderId $ edge ^. _edgeId
      , HE.onMouseLeave $ HE.input_ $ Hover Nothing
      , HE.onDoubleClick \e -> Just $ StopPropagation (ME.toEvent e)
                               $ H.action $ AppDeleteEdge $ edge
      ]

  renderDrawingEdge :: DrawingEdge -> AppState -> Maybe (H.ParentHTML Query SVGContentEditable.Query Slot Aff)
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
      , SA.markerEnd $ SA.URL "#drawing-arrow"
      ]

  render :: AppState -> H.ParentHTML Query SVGContentEditable.Query Slot Aff
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
                     $ Array.fromFoldable $ Map.values state.drawingEdges
      PageSpacePos graphOrigin = state.graphOrigin
    in
      HH.div
      [ HP.ref (H.RefLabel "svg") ]
      [ SE.svg
        [ SA.viewBox
          -- scale then shift
          ( - graphOrigin.x * state.zoom )
          ( - graphOrigin.y * state.zoom )
          ( state.boundingRect.width * state.zoom )
          ( state.boundingRect.height * state.zoom )
        , HE.onMouseDown \e -> Just
                               $ StopPropagation (ME.toEvent e)
                               $ H.action $ BackgroundDragStart state.graphOrigin e
        , HE.onDoubleClick \e -> Just
                                 $ StopPropagation (ME.toEvent e)
                                 $ H.action $ AppCreateNode e
        , HE.onWheel $ HE.input Zoom
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
        , HE.onChange $ HE.input FetchLocalFile
        ]
      ]

  eval :: Query ~> H.ParentDSL AppState Query SVGContentEditable.Query Slot Message Aff
  eval = case _ of
    PreventDefault e q -> do
      H.liftEffect $ WE.preventDefault e
      eval q

    StopPropagation e q -> do
      H.liftEffect $ WE.stopPropagation e
      eval q

    Init next -> next <$ do
      eval $ H.action UpdateBoundingRect
      -- Add keyboard event listener to body
      document <- H.liftEffect $ WHW.document =<< WH.window
      H.subscribe $ ES.eventSource (attachKeydownListener document) (Just <<< H.request <<< Keypress)

    UpdateContentEditableText next -> next <$ do
      state <- H.get
      for_ (state ^. _graph <<< _nodes) \node -> do
        H.query (NodeTextField (node ^. _nodeId)) $ H.action $ SVGContentEditable.SetText $ node ^. _nodeText
      for_ (allEdges (state ^. _graph)) \edge -> do
        H.query (EdgeTextField (edge ^. _edgeId)) $ H.action $ SVGContentEditable.SetText $ edge ^. _edgeText

    NodeTextInput nodeId (SVGContentEditable.TextUpdate text) next -> next <$ do
      state <- H.get
      case lookupNode (state ^. _graph) nodeId of
        Nothing -> pure unit
        Just node ->
          H.modify_ $ (_undoableGraph %~ (doo $ updateNodeTextOp node text))

    EdgeTextInput edgeId (SVGContentEditable.TextUpdate text) next -> next <$ do
      state <- H.get
      case lookupEdge (state ^. _graph) edgeId of
        Nothing -> pure unit
        Just edge ->
      H.modify_ $ (_undoableGraph %~ (doo $ updateEdgeTextOp edge text))

    BackgroundDragStart initialGraphOrigin mouseEvent next -> next <$ do
      H.modify_ $ _graph <<< _focus .~ NoFocus
      H.subscribe $ Drag.dragEventSource mouseEvent
        $ \e -> Just $ BackgroundDragMove e initialGraphOrigin H.Listening

    BackgroundDragMove (Drag.Move _ dragData) (PageSpacePos initialGraphOrigin) next -> next <$ do
      state <- H.get
      let newGraphOrigin = PageSpacePos { x : initialGraphOrigin.x + dragData.offsetX
                                        , y : initialGraphOrigin.y + dragData.offsetY
                                        }
      H.modify_ _{ graphOrigin = newGraphOrigin }

    BackgroundDragMove (Drag.Done _) _ next -> pure next

    NodeDragStart nodeId initialNodePos mouseEvent next -> next <$ do
      H.subscribe $ Drag.dragEventSource mouseEvent
        $ \e -> Just $ NodeDragMove e nodeId initialNodePos H.Listening
      H.modify_ $ _graph <<< _focus .~ FocusNode nodeId

    NodeDragMove (Drag.Move _ dragData) nodeId (GraphSpacePos initialNodePos) next -> next <$ do
      state <- H.get
      let
        dragOffsetGraphSpace = { x : dragData.offsetX * state.zoom
                               , y : dragData.offsetY * state.zoom
                               }
        newNodePos = { x : initialNodePos.x + dragOffsetGraphSpace.x
                     , y : initialNodePos.y + dragOffsetGraphSpace.y
                     }
      case lookupNode (state ^. _graph) nodeId of
        Nothing -> pure unit
        Just node -> H.modify_ $ _undoableGraph %~ (doo $ moveNodeOp node newNodePos)

    NodeDragMove (Drag.Done _) _ _ next -> pure next

    EdgeDrawStart drawingEdgeId mouseEvent next -> next <$ do
      state <- H.get
      let
        mouseGraphPos = mouseEventPosition mouseEvent
                        # toGraphSpace state.boundingRect state.graphOrigin state.zoom
        drawingEdgeSourceId = drawingEdgeId
      H.modify_ $ (_drawingEdges <<< at drawingEdgeId ?~ { pos : mouseGraphPos
                                                         , source : drawingEdgeSourceId
                                                         })
                >>> (_graph <<< _focus .~ FocusNode drawingEdgeId)
      H.subscribe $ Drag.dragEventSource mouseEvent $ \e -> Just $ EdgeDrawMove e drawingEdgeId H.Listening

    EdgeDrawMove (Drag.Move _ dragData) drawingEdgeId next -> next <$ do
      state <- H.get
      let dragGraphPos = PageSpacePos { x : dragData.x, y : dragData.y }
                         # toGraphSpace state.boundingRect state.graphOrigin state.zoom
      H.modify_ $ _drawingEdgePos drawingEdgeId .~ dragGraphPos

    EdgeDrawMove (Drag.Done _) drawingEdgeId next -> next <$ do
      -- Create a new edge if the edge is drawn within the halo of another node
      state <- H.get
      let
        maybeNewEdgeTarget = do
          drawingEdgeState <- Map.lookup drawingEdgeId state.drawingEdges
          Map.values (state ^. _graph <<< _nodes)
           # (List.filter (drawingEdgeWithinNodeHalo drawingEdgeState))
           # List.head
        drawingEdgeSourceId = drawingEdgeId
      case maybeNewEdgeTarget of
        Just newEdgeTarget ->
          let
            newEdge = freshUIEdge { source : drawingEdgeSourceId, target : newEdgeTarget ^. _nodeId }
          in
          eval $ AppCreateEdge newEdge unit
        Nothing -> pure unit
      -- Remove the drawing edge
      H.modify_ $ _drawingEdges <<< at drawingEdgeId .~ Nothing

    AppCreateNode mouseEvent next -> next <$ do
      newNode <- H.liftEffect freshUINode
      state <- H.get
      let
        GraphSpacePos newPos = mouseEventPosition mouseEvent # toGraphSpace state.boundingRect state.graphOrigin state.zoom
        newNode' = newNode # _pos .~ newPos
                           # _nodeText .~ "new node hey"
      H.modify_ $ (_undoableGraph %~ (doo $ insertNodeOp newNode'))
      eval $ FocusOn (FocusNode $ newNode' ^. _nodeId) unit

    AppDeleteNode node next -> next <$ do
      state <- H.get
      H.modify_ $ (_undoableGraph %~ (doo $ deleteNodeOp node))
      case Set.findMin ((lookupParents (state ^. _graph) node) <> (lookupChildren (state ^. _graph) node)) of
        Just neighbor -> eval $ FocusOn (FocusNode (neighbor ^. _nodeId)) unit
        Nothing -> pure unit

    AppCreateEdge edge next -> next <$ do
      H.modify_ $ (_undoableGraph %~ (doo $ insertEdgeOp edge))
      eval $ FocusOn (FocusEdge (edge ^. _edgeId) []) unit

    AppDeleteEdge edge next -> next <$ do
      H.modify_ $ (_undoableGraph %~ (doo $ deleteEdgeOp edge))
      eval $ FocusOn (FocusNode (edge ^. _edgeId).source) unit

    FocusOn newFocus next -> next <$ do
      H.modify_ $ _graph <<< _focus .~ newFocus

    DeleteFocus next -> next <$ do
      state <- H.get
      case state ^. _graph <<< _focus of
        NoFocus -> pure unit
        FocusNode nodeId -> case lookupNode (state ^. _graph) nodeId of
          Nothing -> pure unit
          Just node -> eval $ AppDeleteNode node unit
        FocusEdge edgeId _ -> case lookupEdge (state ^. _graph) edgeId of
          Nothing -> pure unit
          Just edge -> eval $ AppDeleteEdge edge unit

    Hover maybeElementId next -> next <$ do
      H.modify_ _{ hoveredElementId = maybeElementId }

    -- | Zoom in/out holding the mouse position invariant in graph space
    -- |
    -- | p = mousePagePos
    -- | prevMouseGraphPos = (p - oldGraphOrigin) * oldZoom
    -- | newMouseGraphPos = (p - newGraphOrigin) * newZoom
    -- | let newMouseGraphPos = prevMouseGraphPos
    -- | => newGraphOrigin = p - ((p - oldGraphOrigin) * (oldZoom / newZoom))
    -- | So if newZoom --> inf, newGraphOrigin --> mousePos, as expected.
    Zoom wheelEvent next -> next <$ do
      state <- H.get
      let
        scaledZoom = (WhE.deltaY wheelEvent) * zoomScaling
        PageSpacePos mousePagePos = mouseEventPosition $ WhE.toMouseEvent wheelEvent
        PageSpacePos graphOrigin = state.graphOrigin
        newZoom = Math.exp (scaledZoom + (Math.log state.zoom))
        -- keep graph-space mouse pos invariant
        newGraphOriginDim mousePos oldGraphOrigin =
          mousePos - ((mousePos - oldGraphOrigin) * (state.zoom / newZoom))
        newGraphOrigin = PageSpacePos
                         { x : newGraphOriginDim mousePagePos.x graphOrigin.x
                         , y : newGraphOriginDim mousePagePos.y graphOrigin.y
                         }
      H.modify_
        $ (_zoom .~ newZoom)
        >>> _{ graphOrigin = newGraphOrigin }

    CenterGraphOrigin next -> next <$ do
      H.modify _{ graphOrigin = PageSpacePos { x : 0.0, y : 0.0 } }

    FetchLocalFile changeEvent next -> next <$ runMaybeT do
      target <- MaybeT $ pure $ WE.target changeEvent
      inputElement <- MaybeT $ pure $ WHIE.fromEventTarget target
      files <- MaybeT $ H.liftEffect $ WHIE.files inputElement
      file <- MaybeT $ pure $ FileList.item 0 files
      fileReader <- lift $ H.liftEffect $ FileReader.fileReader
      lift $ H.liftEffect $ FileReader.readAsText (File.toBlob file) fileReader
      let attachLoadListener = \fn -> do
            listener <- ET.eventListener fn
            ET.addEventListener WHET.load listener false $ FileReader.toEventTarget fileReader
      lift $ H.subscribe $ ES.eventSource attachLoadListener (Just <<< H.request <<< LoadLocalFile fileReader)

    LoadLocalFile fileReader event reply -> do
      exceptTJSON <- H.liftEffect $ FileReader.result fileReader
      let Identity eitherJSON = runExceptT $ Foreign.readString exceptTJSON
      case (eitherJSON
            # lmap (show <<< map renderForeignError))
           >>= appStateFromJSON of
        Left errors -> H.liftEffect $ log errors
        Right appStateWithMeta -> do
          H.put appStateWithMeta.appState
          -- Update foreignObject wrapper sizes for the initial text
          eval $ H.action UpdateContentEditableText
      pure $ reply H.Done

    SaveLocalFile next -> next <$ do
      state <- H.get
      timestamp <- H.liftEffect now
      let
        stateJSON = appStateToJSON state { version : appStateVersion
                                         , timestamp : timestamp
                                         }
        title = fromMaybe "untitled" $ (graphTitle (state ^. _graph))
      H.liftEffect $ saveJSON stateJSON $ title <> ".graph.json"

    UpdateBoundingRect next -> next <$ runMaybeT do
      svgElement <- MaybeT $ H.getHTMLElementRef (H.RefLabel "svg")
      svgRect <- lift $ H.liftEffect $ WHE.getBoundingClientRect svgElement
      lift $ H.modify_ _{ boundingRect = svgRect }
      lift $ H.liftEffect $ log "updating bounding rect"

    Keypress keyboardEvent reply -> do
      H.liftEffect $ log $ show $ KE.key keyboardEvent
      case KE.key keyboardEvent of
        "z" -> if not (KE.ctrlKey keyboardEvent) then pure unit else do
          H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
          H.liftEffect $ WE.stopPropagation $ KE.toEvent keyboardEvent
          state <- H.get
          H.liftEffect $ log $ "Undoable history: " <> show (state.graph ^. _history)
          H.modify_ $ _undoableGraph %~ undo
          -- Keep text state in sync
          eval $ H.action $ UpdateContentEditableText
        "y" -> if not (KE.ctrlKey keyboardEvent) then pure unit else do
          H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
          H.liftEffect $ WE.stopPropagation $ KE.toEvent keyboardEvent
          state <- H.get
          H.liftEffect $ log $ "Undoable undone: " <> show (state.graph ^. _undone)
          H.modify_ $ _undoableGraph %~ redo
          -- Keep text state in sync
          eval $ H.action $ UpdateContentEditableText
        "l" -> if not (KE.ctrlKey keyboardEvent) then pure unit else do
          H.liftEffect $ loadFile
        "s" -> if not (KE.ctrlKey keyboardEvent) then pure unit else do
          -- Save app state as a local json file
          H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
          H.liftEffect $ WE.stopPropagation $ KE.toEvent keyboardEvent
          eval $ SaveLocalFile unit
        "o" -> if not (KE.ctrlKey keyboardEvent) then pure unit else do
          H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
          H.liftEffect $ WE.stopPropagation $ KE.toEvent keyboardEvent
          eval $ CenterGraphOrigin unit
        "Delete" ->
          eval $ DeleteFocus unit
        "Escape" ->
          eval $ FocusOn NoFocus unit
        "m" -> if not (KE.ctrlKey keyboardEvent) then pure unit else do
          H.raise MappingMode
        -- TODO: decide if grouping/ungrouping is useful for ologs
        --"s" -> H.modify_ $ _graph %~ toggleHighlightFocus
        --" " -> H.modify_ $ _graph %~ toggleGroupExpand
        _ -> pure unit
      pure $ reply H.Listening

    DoNothing next -> pure next

mouseEventPosition :: ME.MouseEvent -> PageSpacePos
mouseEventPosition e = PageSpacePos { x : toNumber $ ME.pageX e
                                    , y : toNumber $ ME.pageY e
                                    }

euclideanDistance :: GraphSpacePos -> GraphSpacePos -> Number
euclideanDistance (GraphSpacePos pos1) (GraphSpacePos pos2) =
  Math.sqrt
  $ (Math.pow (pos1.x - pos2.x) 2.0)
  + (Math.pow (pos1.y - pos2.y) 2.0)

attachKeydownListener :: HTMLDocument -> (KE.KeyboardEvent -> Effect Unit) -> Effect Unit
attachKeydownListener document fn = do
  let target = HTMLDocument.toEventTarget document
  listener <- ET.eventListener (traverse_ fn <<< KE.fromEvent)
  ET.addEventListener KET.keydown listener false target

drawingEdgeWithinNodeHalo :: DrawingEdge -> UINode -> Boolean
drawingEdgeWithinNodeHalo drawingEdgeState' node =
  haloRadius > euclideanDistance (GraphSpacePos (node ^. _pos)) drawingEdgeState'.pos
