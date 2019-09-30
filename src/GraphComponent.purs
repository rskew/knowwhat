module GraphComponent where

import Prelude

import AnalyserComponent as AnalyserComponent
import AppOperation (AppOperation(..), appOperationVersion)
import AppOperation.GraphOp (insertNode, deleteNode, insertEdge, deleteEdge, moveNode, updateNodeText, updateEdgeText)
import AppOperation.Interpreter (doAppOperation, interpretAppOperation, removeGraphData)
import AppOperation.UIOp (moveGraphOrigin, rescalePane, rescaleWindow, removePane)
import AppOperation.UndoOp (undo, redo)
import AppState (DrawingEdge, DrawingEdgeId, HoveredElementId(..), UninitializedAppState, AppState, _drawingEdgePosition, _drawingEdgeTargetGraph, _drawingEdges, _focus, _graphData, edgeIdStr)
import AppState.Foreign (graphDataFromJSON, graphDataToJSON)
import Audio.WebAudio.BaseAudioContext (newAudioContext, close) as WebAudio
import Audio.WebAudio.Types (AudioContext) as WebAudio
import CSS as CSS
import ContentEditable.SVGComponent as SVGContentEditable
import Control.Alt ((<|>))
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT, lift)
import Core (Edge, EdgeId, Focus(..), GraphData, GraphId, GraphSpacePoint2D(..), GraphView, Node, NodeId, PageSpacePoint2D(..), _origin, _pane, _zoom, allEdgesTouchingNode, edgeArray, emptyGraphData, freshNode, graphTitle, separateGraphs, toGraphSpace, toPageSpace, allEdgesBetweenGraphs)
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (for_, foldl)
import Data.Int (toNumber)
import Data.Lens ((%~), (.~), (^.), (^?))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust, fromMaybe)
import Data.Newtype (unwrap)
import Data.String (joinWith)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Data.UUID (genUUID)
import DemoGraph (demo)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Console (log)
import Effect.Now (now)
import Foreign (renderForeignError)
import Foreign as Foreign
import Halogen as H
import Halogen.Component.Utils.Drag as Drag
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Elements.Keyed as HK
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import Math as Math
import Run as Run
import Svg.Attributes as SA
import Svg.Elements as SE
import Svg.Elements.Keyed as SK
import Svg.Types as SVGT
import Synth (FilterState_, Synth, SynthNodeParams(..), SynthNodeState(..), SynthParameter(..), freshSynthNodeParams, parseSynthNodeType)
import Synth.SynthOp (createSynthNode, deleteSynthNode, connectSynthNodes, updateSynthParam, interpretSynthOp)
import UI.Constants (amplifierBoxSize, amplifierGainToControl, amplifierHaloOffset, amplifierTextBoxOffset, controlToAmplifierGain, defaultTextFieldShape, delayPeriodToGraphSpace, delayRectHaloOffset, delayRectHeight, delayTextBoxOffset, edgeTextBoxOffset, filterShape, filterTextBoxOffset, groupNodeRadius, haloRadius, maxTextFieldShape, nodeBorderRadius, nodeRadius, nodeTextBoxOffset, pageSpaceToDelayPeriod, zoomScaling)
import UI.Panes (zoomAtPoint, paneContainingPoint)
import UI.SvgDefs (svgDefs)
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


foreign import loadFile :: Effect Unit
foreign import saveJSON :: String -> String -> Effect Unit


data Action
  = PreventDefault WE.Event Action
  | StopPropagation WE.Event Action
  | EvalQuery (Query Unit)
  | Init
  | UpdateContentEditableText
  | BackgroundDragStart GraphId PageSpacePoint2D ME.MouseEvent
  | BackgroundDragMove Drag.DragEvent GraphId PageSpacePoint2D H.SubscriptionId
  | NodeDragStart GraphId NodeId GraphSpacePoint2D ME.MouseEvent
  | NodeDragMove Drag.DragEvent GraphId NodeId GraphSpacePoint2D H.SubscriptionId
  | EdgeDrawStart GraphView DrawingEdgeId ME.MouseEvent
  | EdgeDrawMove Drag.DragEvent GraphId DrawingEdgeId H.SubscriptionId
  | NodeTextInput NodeId SVGContentEditable.Message
  | EdgeTextInput GraphId EdgeId SVGContentEditable.Message
  | AppCreateNode GraphView ME.MouseEvent
  | AppDeleteNode Node
  | AppCreateEdge GraphId EdgeId
  | AppDeleteEdge GraphId Edge
  | FocusOn GraphId (Maybe Focus)
  | DeleteFocus GraphId
  | Hover (Maybe HoveredElementId)
  | Zoom GraphId WhE.WheelEvent
  | CenterGraphOriginAndZoom
  | FetchLocalFile WE.Event
  | LoadLocalFile FileReader.FileReader H.SubscriptionId WE.Event
  | SaveLocalFile
  | Keypress KE.KeyboardEvent
  | DoNothing
  | GainDragStart GraphId NodeId Number ME.MouseEvent
  | GainDragMove Drag.DragEvent GraphId NodeId Number H.SubscriptionId
  | DelayDragStart GraphId NodeId Number ME.MouseEvent
  | DelayDragMove Drag.DragEvent GraphId NodeId Number H.SubscriptionId

type Input = { windowBoundingRect :: WHE.DOMRect
             , audioContext       :: WebAudio.AudioContext
             }

initialState :: Input -> AppState
initialState inputs =
  { graphData             : emptyGraphData
  , history               : Map.empty
  , undone                : Map.empty
  , windowBoundingRect    : inputs.windowBoundingRect
  , synth : { synthState  : { audioContext    : inputs.audioContext
                            , synthNodeStates : Map.empty
                            }
            , synthParams : Map.empty
            }
  , drawingEdges          : Map.empty
  , hoveredElementId      : Nothing
  , focusedPane           : Nothing
  }

data Query a = UpdateBoundingRect a

data Message
  = Focused
  | MappingMode

-- Slot type for parent components using a child GraphComponent
type Slot = H.Slot Query Message

type Slots =
  ( nodeTextField :: SVGContentEditable.Slot NodeId
  , edgeTextField :: SVGContentEditable.Slot EdgeId
  , analyser :: AnalyserComponent.Slot NodeId
  )

_nodeTextField :: SProxy "nodeTextField"
_nodeTextField = SProxy

_edgeTextField :: SProxy "edgeTextField"
_edgeTextField = SProxy

_analyser :: SProxy "analyser"
_analyser = SProxy

graphComponent :: H.Component HH.HTML Query Input Message Aff
graphComponent =
  H.mkComponent
    { initialState : initialState
    , render : render
    , eval: H.mkEval (H.defaultEval { handleAction = handleAction
                                    , handleQuery = handleQuery
                                    , initialize   = Just Init
                                    })
    }
  where

  renderGraphNode :: AppState -> GraphView -> Node -> Tuple String (H.ComponentHTML Action Slots Aff)
  renderGraphNode state pane node =
    let
      hoveredOverBorder = state.hoveredElementId == Just (NodeBorderId node.id)
      hoveredOverHalo   = state.hoveredElementId == Just (NodeHaloId node.id)
      noDrawingEdgeFromNode =
        Map.isEmpty (Map.filter (\drawingEdge -> drawingEdge.source == node.id)
                                (state ^. _drawingEdges))
      existsDrawingEdgeHoveredOverNode =
        (0 < Map.size (Map.filter (\drawingEdge -> drawingEdgeWithinNodeHalo drawingEdge pane node) state.drawingEdges))
      drawingEdgeOverNode =
        (hoveredOverBorder || hoveredOverHalo)
        &&
        noDrawingEdgeFromNode
        &&
        existsDrawingEdgeHoveredOverNode
      nodeClasses =
        joinWith " " $ Array.catMaybes
        [ Just "node"
        , if pane.focus == Just (FocusNode node.id)
          then Just "focused"
          else Nothing
        ]
      nodeBorderClasses =
          joinWith " " $ Array.catMaybes
          [ Just "nodeBorder"
          -- TODO: use highlighting?
          --, if (Set.member node.id state.graphData.highlighted)
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
        , if pane.focus == Just (FocusNode node.id)
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
          [ SA.transform [ SVGT.Translate textBoxOffset.x textBoxOffset.y ]
          , HE.onMouseDown \e -> Just
                                 $ StopPropagation (ME.toEvent e)
                                 $ DoNothing
          ]
          [ HH.slot
            _nodeTextField
            node.id
            SVGContentEditable.svgContenteditable
            { shape : defaultTextFieldShape
            , initialText : node.text
            , maxShape : maxTextFieldShape
            , fitContentDynamic : true
            }
            (Just <<< NodeTextInput node.id)
          ]
        ]
      graphNodeHTML =
        -- Node Halo, for creating edges from
        [ SE.circle
          [ SA.class_ haloClasses
          , SA.r $ show haloRadius
          , HE.onMouseDown \e -> Just $ StopPropagation (ME.toEvent e)
                                 $ EdgeDrawStart pane node.id e
          , HE.onMouseEnter \_ -> Just $ Hover $ Just $ NodeHaloId node.id
          , HE.onMouseLeave \_ -> Just $ Hover Nothing
          ]
        -- Node center
        , SE.circle
          [ SA.class_ nodeClasses
          , SA.r $ show $ if isJust node.subgraph
                          then groupNodeRadius
                          else nodeRadius
          ]
        -- Node border, for grabbing
        , SE.circle
          [ SA.class_ nodeBorderClasses
          , SA.r $ show nodeBorderRadius
          , HE.onMouseDown \e -> Just
                                 $ StopPropagation (ME.toEvent e)
                                 $ NodeDragStart node.graphId node.id node.position e
          , HE.onDoubleClick \e -> Just $ StopPropagation (ME.toEvent e)
                                   $ AppDeleteNode node
          , HE.onMouseEnter \_ -> Just $ Hover $ Just $ NodeBorderId node.id
          , HE.onMouseLeave \_ -> Just $ Hover Nothing
          ]
        ] <> textBoxHTML nodeTextBoxOffset
      -- Amplifier node with draggable gain control
      --
      -- Drag the back of the speaker to move it around
      -- Drag the horn of the speaker up and down to change the gain
      --
      ---          /|
      ---      ___/ |
      ---      |    |
      ---      |__  |
      ---         \ |
      ---          \|

      amplifierNodeHTML gain =
        -- Node Halo, for creating edges from
        [ SE.path [ let
                      b = amplifierHaloOffset + amplifierBoxSize
                    in
                      SA.d [ SVGT.Abs (SVGT.M (-b) (-b))
                           , SVGT.Abs (SVGT.L   b  (-b))
                           , SVGT.Abs (SVGT.L   b    b)
                           , SVGT.Abs (SVGT.L (-b)   b)
                           , SVGT.Abs SVGT.Z
                           ]
                  , SA.class_ haloClasses
                  , HE.onMouseDown \e -> Just $ StopPropagation (ME.toEvent e)
                                         $ EdgeDrawStart pane node.id e
                  , HE.onMouseEnter \_ -> Just $ Hover $ Just $ NodeHaloId node.id
                  , HE.onMouseLeave \_ -> Just $ Hover Nothing
                  ]
        -- Draggable amplifier control
        , SE.path [ SA.class_ $ nodeClasses <> if gain < 1.0
                                               then " amplifier-below-unity"
                                               else " amplifier-above-unity"
                  , let
                      b = amplifierBoxSize
                      hornSize = 3.0 * amplifierBoxSize * (amplifierGainToControl gain)
                    in
                      SA.d [ SVGT.Abs (SVGT.M (-b)      (-b))
                           , SVGT.Abs (SVGT.L   b       (-b))
                           , SVGT.Abs (SVGT.L (3.0 * b) (-hornSize))
                           , SVGT.Abs (SVGT.L (3.0 * b)   hornSize)
                           , SVGT.Abs (SVGT.L   b         b)
                           , SVGT.Abs (SVGT.L (-b)        b)
                           , SVGT.Abs SVGT.Z
                           ]
                  , HE.onMouseDown \e -> Just
                                         $ StopPropagation (ME.toEvent e)
                                         $ GainDragStart node.graphId node.id gain e
                  ]
        -- Node border, for grabbing
        , SE.path [ let
                       b = amplifierBoxSize
                    in
                       SA.d [ SVGT.Abs (SVGT.M (-b) (-b))
                            , SVGT.Abs (SVGT.L   b  (-b))
                            , SVGT.Abs (SVGT.L   b    b)
                            , SVGT.Abs (SVGT.L (-b)   b)
                            , SVGT.Abs SVGT.Z
                            ]
                  , SA.class_ nodeBorderClasses
                  , HE.onMouseDown \e -> Just
                                         $ StopPropagation (ME.toEvent e)
                                         $ NodeDragStart node.graphId node.id node.position e
                  , HE.onDoubleClick \e -> Just $ StopPropagation (ME.toEvent e)
                                           $ AppDeleteNode node
                  , HE.onMouseEnter \_ -> Just $ Hover $ Just $ NodeBorderId node.id
                  , HE.onMouseLeave \_ -> Just $ Hover Nothing
                  ]
        ] <> textBoxHTML amplifierTextBoxOffset
      -- Delay node with draggable delay-period control
      --
      -- Drag left box handle to move around
      --
      -- Drag delay tube left/right to change the delay period
      --
      ---   ____________________
      ---   |_-_-_-_-_-_-_-_-_-|
      ---   --------------------
      ---
      delayNodeHTML period =
        -- Node Halo, for creating edges from
        [ SE.rect [ SA.height $ show $ delayRectHeight + 2.0 * delayRectHaloOffset
                  , SA.width $ show $ (delayPeriodToGraphSpace period) + 2.0 * delayRectHaloOffset
                  , SA.x $ show $ - delayRectHaloOffset
                  , SA.y $ show $ - delayRectHaloOffset - delayRectHeight / 2.0
                  , SA.class_ $ haloClasses
                  , HE.onMouseDown \e -> Just $ StopPropagation (ME.toEvent e)
                                         $ EdgeDrawStart pane node.id e
                  , HE.onMouseEnter \_ -> Just $ Hover $ Just $ NodeHaloId $ node.id
                  , HE.onMouseLeave \_ -> Just $ Hover Nothing
                  ]
        -- Inner border shadow for delay period control
        , SE.rect [ SA.class_ $ nodeClasses <> " delay border-shadow"
                  , SA.height $ show $ delayRectHeight - 4.0
                  , SA.width $ show $ (delayPeriodToGraphSpace period) - 4.0
                  , SA.x $ show 2.0
                  , SA.y $ show $ 2.0 - delayRectHeight / 2.0
                  ]
        -- Draggable delay period control
        , SE.rect [ SA.class_ $ nodeClasses <> " delay"
                  , SA.height $ show delayRectHeight
                  , SA.width $ show $ delayPeriodToGraphSpace period
                  , SA.y $ show $ - delayRectHeight / 2.0
                  , HE.onMouseDown \e -> Just
                                         $ StopPropagation (ME.toEvent e)
                                         $ DelayDragStart node.graphId node.id period e
                  ]
        -- Node border, for grabbing
        , SE.rect [ SA.height $ show $ delayRectHeight / 2.0
                  , SA.width $ show $ Math.min delayRectHeight (delayPeriodToGraphSpace period)
                  , SA.class_ $ nodeBorderClasses <> " delay"
                  , SA.y $ show $ - delayRectHeight / 2.0
                  , HE.onMouseDown \e -> Just
                                         $ StopPropagation (ME.toEvent e)
                                         $ NodeDragStart node.graphId node.id node.position e
                  , HE.onDoubleClick \e -> Just $ StopPropagation (ME.toEvent e)
                                           $ AppDeleteNode node
                  , HE.onMouseEnter \_ -> Just $ Hover $ Just $ NodeBorderId $ node.id
                  , HE.onMouseLeave \_ -> Just $ Hover Nothing
                  ]
        ] <> textBoxHTML delayTextBoxOffset
      -- Filter node with analyser spectrum display and draggable cutoff control
      --
      -- Drag the square brackets to move the cutoff frequency of the
      -- hipass and lopass filters.
      --
      ---   ____________________
      ---   |   /\  | [[ /\  ]]|
      ---   |../  \/ \[[/  \/]]|
      ---   --------------------
      ---
      filterNodeHTML :: Node -> FilterState_ -> Number -> Number -> Array (H.ComponentHTML Action Slots Aff)
      filterNodeHTML node' filterState lopassCutoff hipassCutoff =
        -- Spectrum display
        [ HH.slot
          _analyser
          node'.id
          AnalyserComponent.analyser
          { shape : filterShape
          , analyserNode : filterState.analyserNode
          , spectrumBuffer : filterState.analyserBuffer
          , drawLoopStopSignal : filterState.drawLoopStopSignal
          }
          (const Nothing)
        -- Node Halo, for creating edges from
        , SE.rect [ SA.height $ show $ filterShape.height + 2.0 * delayRectHaloOffset
                  , SA.width $ show $ filterShape.width + 2.0 * delayRectHaloOffset
                  , SA.x $ show $ - delayRectHaloOffset
                  , SA.y $ show $ - delayRectHaloOffset
                  , SA.class_ $ haloClasses
                  , HE.onMouseDown \e -> Just $ StopPropagation (ME.toEvent e)
                                         $ EdgeDrawStart pane node'.id e
                  , HE.onMouseEnter \_ -> Just $ Hover $ Just $ NodeHaloId $ node'.id
                  , HE.onMouseLeave \_ -> Just $ Hover Nothing
                  ]
        -- Thin border
        , SE.rect
          [ SA.class_ "analyser_border"
          , SA.fill $ SVGT.PaintColor $ SVGT.RGBA 0 0 0 0.0
            -- 'stroke: #dddc;';
          , SA.stroke $ SVGT.PaintColor $ SVGT.RGBA (13 * 16) (13 * 16) (13 * 16) (12.0 / 16.0)
          , SA.height $ show filterShape.height
          , SA.width $ show filterShape.width
          , HE.onMouseDown \e -> Just
                                 $ StopPropagation (ME.toEvent e)
                                 $ NodeDragStart node'.graphId node'.id node'.position e
          , HE.onDoubleClick \e -> Just $ StopPropagation (ME.toEvent e)
                                   $ AppDeleteNode node'
          , HE.onMouseEnter \_ -> Just $ Hover $ Just $ NodeBorderId $ node'.id
          , HE.onMouseLeave \_ -> Just $ Hover Nothing
          ]
        ] <> textBoxHTML filterTextBoxOffset

      whichNodeHTML = case Map.lookup node.id state.synth.synthParams of
        Nothing -> graphNodeHTML
        Just synthNodeParams -> case synthNodeParams of
          OscillatorParams freq -> graphNodeHTML
          AmplifierParams gain -> amplifierNodeHTML gain
          DelayParams delayPeriod -> delayNodeHTML delayPeriod
          FilterParams filterParams ->
            case Map.lookup node.id state.synth.synthState.synthNodeStates of
              Nothing -> graphNodeHTML
              Just synthNodeState -> case synthNodeState of
                FilterState filterState ->
                  filterNodeHTML node filterState 0.0 0.0
                _ -> graphNodeHTML
          _ -> graphNodeHTML
      GraphSpacePoint2D nodePos = node.position
      nodeHTML = SE.g
                 [ SA.transform [ SVGT.Translate nodePos.x nodePos.y ] ]
                 whichNodeHTML
    in
      Tuple (show node.id) nodeHTML

  renderGhostNode :: AppState -> GraphView -> Node -> Maybe (Tuple String (H.ComponentHTML Action Slots Aff))
  renderGhostNode state renderPane node =
    case Map.lookup node.graphId state.graphData.panes of
      Nothing -> Nothing
      Just nativePane -> Just
        let
          graphNodeHTML =
            SE.circle
            [ SA.class_ "node ghost"
            , SA.r $ show $ if isJust node.subgraph
                            then groupNodeRadius
                            else nodeRadius
            ]
          GraphSpacePoint2D nodePos = node.position # toPageSpace nativePane # toGraphSpace renderPane
          nodeHTML = SE.g
                     [ SA.transform [ SVGT.Translate nodePos.x nodePos.y ] ]
                     [ graphNodeHTML ]
        in
          Tuple (show node.id <> "_ghost") nodeHTML

  renderEdge :: AppState -> GraphView -> Edge -> Maybe (Tuple String (H.ComponentHTML Action Slots Aff))
  renderEdge state renderPane edge = do
    targetNode <- Map.lookup edge.id.target state.graphData.nodes
    GraphSpacePoint2D targetPos <- edgeTargetPosition state edge renderPane
    GraphSpacePoint2D sourcePos <- edgeSourcePosition state edge renderPane
    let
      focused = state ^? _focus renderPane.graphId == (Just $ Just $ FocusEdge edge.id [])
      edgeClasses =
        joinWith " " $ Array.catMaybes
        [ Just "edge"
        , if focused
          then Just "focused"
          else Nothing
        ]
      markerRef = if (isJust targetNode.subgraph)
                  then "url(#arrow)"
                  else "url(#arrow-to-group)"
      markerRef' = if isJust $ Map.lookup edge.id.target state.synth.synthParams
                   then "url(#arrow-to-synth-node)"
                   else markerRef
    pure $ Tuple (show edge.id <> "_edge_" <> show renderPane.graphId) $
      SE.line
      [ SA.class_ edgeClasses
      , SA.x1 $ show sourcePos.x
      , SA.y1 $ show sourcePos.y
      , SA.x2 $ show targetPos.x
      , SA.y2 $ show targetPos.y
      , SA.markerEnd markerRef'
      ]

  renderEdgeTextField :: AppState -> GraphView -> Edge -> Maybe (Tuple String (H.ComponentHTML Action Slots Aff))
  renderEdgeTextField state renderPane edge = do
    targetNode <- Map.lookup edge.id.target state.graphData.nodes
    sourceNode <- Map.lookup edge.id.source state.graphData.nodes
    GraphSpacePoint2D edgeMidPos <- edgeMidPosition state edge renderPane
    let
      focused = state ^? _focus renderPane.graphId == (Just $ Just $ FocusEdge edge.id [])
    if not focused && (edge.text == "")
    then Nothing
    else Just $ Tuple (show edge.id <> "_textField_" <> show renderPane.graphId) $
      SE.g
      [ SA.transform [ SVGT.Translate
                       (edgeMidPos.x + edgeTextBoxOffset.x)
                       (edgeMidPos.y + edgeTextBoxOffset.y)
                     ]
      , HE.onMouseDown \e -> Just
                             $ StopPropagation (ME.toEvent e)
                             $ DoNothing
      ]
      [ HH.slot
        _edgeTextField
        edge.id
        SVGContentEditable.svgContenteditable
        { shape : defaultTextFieldShape
        , initialText : edge.text
        , maxShape : maxTextFieldShape
        , fitContentDynamic : true
        }
        (Just <<< EdgeTextInput sourceNode.graphId edge.id)
      ]

  renderEdgeBorder :: AppState -> GraphView -> Edge -> Maybe (Tuple String (H.ComponentHTML Action Slots Aff))
  renderEdgeBorder state renderPane edge = do
    targetNode <- Map.lookup edge.id.target state.graphData.nodes
    GraphSpacePoint2D sourcePos <- edgeSourcePosition state edge renderPane
    GraphSpacePoint2D targetPos <- edgeTargetPosition state edge renderPane
    let
      edgeBorderClasses =
        joinWith " " $ Array.catMaybes
        [ Just "edgeBorder"
        , if state.hoveredElementId == (Just $ EdgeBorderId edge.id)
          then Just "hover"
          else Nothing
        ]
    pure $ Tuple (show edge.id <> "_border_" <> show renderPane.graphId) $
      SE.line
      [ SA.class_ edgeBorderClasses
      , SA.x1 $ show sourcePos.x
      , SA.y1 $ show sourcePos.y
      , SA.x2 $ show targetPos.x
      , SA.y2 $ show targetPos.y
      , SA.strokeLinecap SVGT.Butt
      , HE.onClick \_ -> Just $ FocusOn renderPane.graphId $ Just $ FocusEdge edge.id []
      , HE.onMouseEnter \_ -> Just $ Hover $ Just $ EdgeBorderId edge.id
      , HE.onMouseLeave \_ -> Just $ Hover Nothing
      , HE.onDoubleClick \e -> Just $ StopPropagation (ME.toEvent e)
                                    $ AppDeleteEdge renderPane.graphId edge
      ]

  renderDrawingEdge :: AppState -> GraphView -> DrawingEdge -> Maybe (Tuple String (H.ComponentHTML Action Slots Aff))
  renderDrawingEdge state renderPane drawingEdgeState = do
    let
      GraphSpacePoint2D sourceGraphPos =
        drawingEdgeState.sourcePosition
        # toGraphSpace renderPane
      GraphSpacePoint2D pointGraphPos =
        drawingEdgeState.pointPosition
        # toGraphSpace renderPane
      isAcrossGraphs = drawingEdgeState.sourceGraph /= drawingEdgeState.targetGraph
    pure $ Tuple (show drawingEdgeState.source <> "_drawingEdge_" <> show renderPane.graphId) $
      SE.line
      [ SA.class_ $ "edge" <> if isAcrossGraphs then " mappingEdge" else ""
      , SA.x1 $ show sourceGraphPos.x
      , SA.y1 $ show sourceGraphPos.y
      , SA.x2 $ show pointGraphPos.x
      , SA.y2 $ show pointGraphPos.y
      , SA.markerEnd "url(#drawing-arrow)"
      ]

  renderSingleGraph :: AppState -> GraphView -> GraphData -> H.ComponentHTML Action Slots Aff
  renderSingleGraph state renderPane singleGraph =
    let
      nodes = singleGraph.nodes # Map.values # Array.fromFoldable

      keyedNodes = nodes <#> renderGraphNode state renderPane

      allNodesInOtherGraphs = state.graphData.nodes
                              # Map.filter (\node -> node.graphId /= renderPane.graphId)
                              # Map.values # Array.fromFoldable

      ghostNodes = Array.catMaybes $ allNodesInOtherGraphs <#> renderGhostNode state renderPane

      edges = Array.nub $ edgeArray singleGraph <> allEdgesBetweenGraphs state.graphData

      keyedEdges = Array.mapMaybe (renderEdge state renderPane) edges

      keyedEdgeTextFields = Array.mapMaybe (renderEdgeTextField state renderPane) edges

      keyedEdgeBorders = Array.mapMaybe (renderEdgeBorder state renderPane) edges

      drawingEdges = state.drawingEdges # Map.values # Array.fromFoldable

      keyedDrawingEdges = Array.mapMaybe (renderDrawingEdge state renderPane) drawingEdges

      zoom                    = renderPane.zoom
      PageSpacePoint2D origin = renderPane.origin
      boundingRect            = renderPane.boundingRect
      focused                 = state.focusedPane == Just renderPane.graphId
    in
      HH.div
      [ HCSS.style do
          CSS.left    $ CSS.px boundingRect.left
          CSS.top     $ CSS.px boundingRect.top
          CSS.width   $ CSS.px boundingRect.width
          CSS.height  $ CSS.px boundingRect.height
      , HP.classes $ [ HH.ClassName "pane" ] <> if focused then [ HH.ClassName "focused" ] else []
      ]
      [ SE.svg
        [ SA.viewBox
          -- scale then shift
          ( - origin.x * zoom )
          ( - origin.y * zoom )
          ( boundingRect.width  * zoom )
          ( boundingRect.height * zoom )
        , HE.onMouseDown \e -> Just
                               $ StopPropagation (ME.toEvent e)
                               $ BackgroundDragStart renderPane.graphId (PageSpacePoint2D origin) e
        , HE.onDoubleClick \e -> Just
                                 $ StopPropagation (ME.toEvent e)
                                 $ AppCreateNode renderPane e
        , HE.onWheel $ Just <<< Zoom renderPane.graphId
        ]
        [ svgDefs
        , SK.g
          []
          (ghostNodes
           <>keyedEdges
           <> keyedEdgeBorders
           <> keyedDrawingEdges
           <> keyedNodes
           <> keyedEdgeTextFields)
        ]
      ]

  render :: AppState -> H.ComponentHTML Action Slots Aff
  render state =
    let
      graphs = separateGraphs state.graphData
      renderedGraphs =
        graphs
        # Map.toUnfoldable
        <#> (\(Tuple graphId singleGraph) -> do
               pane <- Map.lookup graphId singleGraph.panes
               pure $ Tuple (show graphId) $ renderSingleGraph state pane singleGraph)
        # Array.catMaybes
    in
      HK.div
      [ HP.ref (H.RefLabel "panes")
      , HP.classes [ HH.ClassName "panes" ]
      ]
      (renderedGraphs
       <> [ Tuple "input" $
                  HH.input
                  [ HP.type_ InputFile
                  , HE.onChange $ Just <<< FetchLocalFile
                  ]
          ]
      )

  handleAction :: Action -> H.HalogenM AppState Action Slots Message Aff Unit
  handleAction = case _ of
    PreventDefault e next -> do
      H.liftEffect $ WE.preventDefault e
      handleAction next

    StopPropagation e next -> do
      H.liftEffect $ WE.stopPropagation e
      handleAction next

    EvalQuery query -> do
      _ <- handleQuery query
      pure unit

    Init -> do
      -- Initialize bounding rectangle
      _ <- handleQuery $ UpdateBoundingRect (const unit)
      state_ <- H.get
      H.liftEffect $ log $ "new window rect: " <> show state_.windowBoundingRect
      -- Subscribe to updates in bounding rectangle
      window <- H.liftEffect $ WH.window
      _ <- H.subscribe $ ES.eventListenerEventSource
                         (WE.EventType "resize")
                         (WHW.toEventTarget window)
                         \event -> Just $ EvalQuery $ UpdateBoundingRect unit
      -- Add keyboard event listener to body
      document <- H.liftEffect $ WHW.document =<< WH.window
      _ <- H.subscribe $ ES.eventListenerEventSource KET.keydown (HTMLDocument.toEventTarget document) (map Keypress <<< KE.fromEvent)
      -- Load demo graph into pane
      Tuple graphId demoGraphOp <- H.liftEffect demo
      bareState <- H.get
      let initState = interpretAppOperation $ AppOperation do
            (unwrap demoGraphOp)
            rescalePane graphId bareState.windowBoundingRect
      initialisedState <- H.liftEffect $ initState bareState
      H.put $ initialisedState # _{ history = Map.singleton graphId []
                                  , undone  = Map.singleton graphId []
                                  }
      state' <- H.get
      H.liftEffect $ log $ "pane bounding rects: " <> (show $ (_.boundingRect >>> show) <$> Map.values state'.graphData.panes)

    UpdateContentEditableText -> do
      state <- H.get
      for_ state.graphData.nodes \node -> do
        H.query _nodeTextField node.id $ H.tell $ SVGContentEditable.SetText $ node.text
      for_ (edgeArray state.graphData) \edge -> do
        H.query _edgeTextField edge.id $ H.tell $ SVGContentEditable.SetText $ edge.text

    NodeTextInput nodeId (SVGContentEditable.TextUpdate text) -> do
      state <- H.get
      case Map.lookup nodeId state.graphData.nodes of
        Nothing -> pure unit
        Just node -> do
          modifyM $ doAppOperation node.graphId $ AppOperation $ updateNodeText node text
          handleAction $ FocusOn node.graphId $ Just $ FocusNode node.id

    EdgeTextInput graphId edgeId (SVGContentEditable.TextUpdate text) -> do
      state <- H.get
      case Map.lookup edgeId.source state.graphData.edges.sourceTarget >>= Map.lookup edgeId.target of
        Nothing -> pure unit
        Just edge -> do
          modifyM $ doAppOperation graphId $ AppOperation $ updateEdgeText edge text
          handleAction $ FocusOn graphId $ Just $ FocusEdge edgeId []

    BackgroundDragStart graphId initialGraphOrigin mouseEvent -> do
      handleAction $ FocusOn graphId Nothing
      H.subscribe' \subscriptionId ->
        Drag.dragEventSource mouseEvent
          \e -> BackgroundDragMove e graphId initialGraphOrigin subscriptionId

    BackgroundDragMove (Drag.Move _ dragData) graphId (PageSpacePoint2D initialGraphOrigin) _ -> do
      state <- H.get
      let newGraphOrigin =
            PageSpacePoint2D
              { x : initialGraphOrigin.x + dragData.offsetX
              , y : initialGraphOrigin.y + dragData.offsetY
              }
      modifyM $ doAppOperation graphId $ AppOperation
        $ moveGraphOrigin graphId newGraphOrigin


    BackgroundDragMove (Drag.Done _) _ _ subscriptionId ->
      H.unsubscribe subscriptionId

    NodeDragStart graphId nodeId initialNodePos mouseEvent -> do
      H.subscribe' \subscriptionId ->
        Drag.dragEventSource mouseEvent
          \e -> NodeDragMove e graphId nodeId initialNodePos subscriptionId
      handleAction $ FocusOn graphId $ Just $ FocusNode nodeId

    NodeDragMove (Drag.Move _ dragData) graphId nodeId (GraphSpacePoint2D initialNodePos) _ -> do
      state <- H.get
      case Map.lookup graphId state.graphData.panes of
        Nothing -> pure unit
        Just pane -> do
          let
            dragOffsetGraphSpace = { x : dragData.offsetX * pane.zoom
                                   , y : dragData.offsetY * pane.zoom
                                   }
            newNodePos =
              GraphSpacePoint2D
                { x : initialNodePos.x + dragOffsetGraphSpace.x
                , y : initialNodePos.y + dragOffsetGraphSpace.y
                }
          case Map.lookup nodeId state.graphData.nodes of
            Nothing -> pure unit
            Just node -> do
              H.liftEffect $ log $ "new pos: " <> show newNodePos
              H.liftEffect $ log $ "node pos: " <> show node.position
              modifyM $ doAppOperation node.graphId $ AppOperation $ moveNode node newNodePos

    NodeDragMove (Drag.Done _) _ _ _ subscriptionId ->
      H.unsubscribe subscriptionId

    EdgeDrawStart pane drawingEdgeId mouseEvent -> do
      state <- H.get
      case Map.lookup drawingEdgeId state.graphData.nodes of
        Nothing -> pure unit
        Just source -> do
          let
            mousePosition = mouseEventPosition mouseEvent
            sourcePosition = source.position # toPageSpace pane
          H.modify_ $ (_drawingEdges %~ Map.insert drawingEdgeId { sourcePosition : sourcePosition
                                                                 , source         : drawingEdgeId
                                                                 , sourceGraph    : pane.graphId
                                                                 , pointPosition  : mousePosition
                                                                 , targetGraph    : pane.graphId
                                                                 })
          handleAction $ FocusOn pane.graphId $ Just $ FocusNode drawingEdgeId
          H.subscribe' \subscriptionId ->
            Drag.dragEventSource mouseEvent $ \e -> EdgeDrawMove e pane.graphId drawingEdgeId subscriptionId

    -- | Check which pane the point of the drawing edge is in and update
    -- | the drawingEdgeState so it can be drawn properly in both panes
    EdgeDrawMove (Drag.Move _ dragData) graphId drawingEdgeId _ -> do
      state <- H.get
      let
        panes = Array.fromFoldable $ Map.values state.graphData.panes
        edgePageTargetPosition = PageSpacePoint2D { x : dragData.x, y : dragData.y }
      case paneContainingPoint panes edgePageTargetPosition <|> Map.lookup graphId state.graphData.panes of
        Nothing -> pure unit
        Just targetPane -> do
          H.modify_ $ (_drawingEdgePosition drawingEdgeId .~ edgePageTargetPosition)
                      >>> (_drawingEdgeTargetGraph drawingEdgeId .~ targetPane.graphId)

    EdgeDrawMove (Drag.Done _) graphId drawingEdgeId subscriptionId -> do
      -- Create a new edge if the edge is drawn within the halo of another node
      state <- H.get
      let
        maybeNewEdgeId = do
          drawingEdgeState <- Map.lookup drawingEdgeId state.drawingEdges
          targetPane <- Map.lookup drawingEdgeState.targetGraph state.graphData.panes
          drawingEdgeTarget <-
            Map.values state.graphData.nodes
              # List.filter (\node -> node.graphId == drawingEdgeState.targetGraph)
              # List.filter (drawingEdgeWithinNodeHalo drawingEdgeState targetPane)
              # List.head
          pure { source      : drawingEdgeState.source
               , sourceGraph : drawingEdgeState.sourceGraph
               , target      : drawingEdgeTarget.id
               , targetGraph : drawingEdgeTarget.graphId
               }
      case maybeNewEdgeId of
        Nothing -> pure unit
        Just newEdgeId -> do
          handleAction $ AppCreateEdge graphId newEdgeId
      -- Remove the drawing edge
      H.modify_ $ _{ drawingEdges = Map.delete drawingEdgeId state.drawingEdges }
      H.unsubscribe subscriptionId

    AppCreateNode pane mouseEvent -> do
      newNodeId <- H.liftEffect genUUID
      state <- H.get
      let
        newNode = freshNode pane.graphId newNodeId
        newNodePosition = mouseEventPosition mouseEvent
        newNodeGraphSpacePos =
          newNodePosition
          # toGraphSpace pane
      modifyM $ doAppOperation pane.graphId $ AppOperation
        $   insertNode pane.graphId newNodeId
        >>= (const $ moveNode newNode newNodeGraphSpacePos)
      handleAction $ FocusOn pane.graphId $ Just $ FocusNode newNodeId

    AppDeleteNode node -> do
      state <- H.get
      let allEdges = allEdgesTouchingNode node.id state.graphData
      case Array.head allEdges.incoming of
        Just incomingEdge -> handleAction $ FocusOn node.graphId $ Just $ FocusNode incomingEdge.id.source
        Nothing -> case Array.head allEdges.outgoing of
          Just outgoingEdge -> handleAction $ FocusOn node.graphId $ Just $ FocusNode outgoingEdge.id.target
          Nothing -> pure unit
      state' <- H.get
      modifyM $ doAppOperation node.graphId $ AppOperation $ deleteNode state'.graphData node

    AppCreateEdge graphId edgeId -> do
      modifyM $ doAppOperation graphId $ AppOperation $ insertEdge edgeId
      handleAction $ FocusOn edgeId.sourceGraph $ Just $ FocusEdge edgeId []

    AppDeleteEdge graphId edge -> do
      modifyM $ doAppOperation graphId $ AppOperation $ deleteEdge edge
      handleAction $ FocusOn edge.id.sourceGraph $ Just $ FocusNode edge.id.source

    FocusOn graphId newFocus -> do
      H.modify_ $
        (_focus graphId .~ newFocus)
        >>> _{ focusedPane = Just graphId }

    DeleteFocus graphId -> do
      state <- H.get
      case Map.lookup graphId state.graphData.panes >>= _.focus of
        Nothing -> pure unit
        Just (FocusNode nodeId) -> case Map.lookup nodeId state.graphData.nodes of
          Nothing -> pure unit
          Just node ->
            handleAction $ AppDeleteNode node
        Just (FocusEdge edgeId _) ->
          case Map.lookup edgeId.source state.graphData.edges.sourceTarget
               >>= Map.lookup edgeId.target of
            Nothing -> pure unit
            Just edge -> handleAction $ AppDeleteEdge graphId edge

    Hover maybeElementId -> do
      H.modify_ $ _{ hoveredElementId = maybeElementId }

    -- | Zoom in/out holding the mouse position invariant
    Zoom graphId wheelEvent -> do
      state <- H.get
      case Map.lookup graphId state.graphData.panes of
        Nothing -> pure unit
        Just pane ->
          let
            scaledZoom = (WhE.deltaY wheelEvent) * zoomScaling
            newZoom = (Math.exp scaledZoom) * pane.zoom
          in
            modifyM
            $ doAppOperation pane.graphId
            $ zoomAtPoint
                newZoom
                (mouseEventPosition $ WhE.toMouseEvent wheelEvent)
                pane

    CenterGraphOriginAndZoom -> do
      state <- H.get
      case state.focusedPane of
        Nothing -> pure unit
        Just graphId ->
          H.modify_ $     (_graphData <<< _pane graphId <<< _origin .~ PageSpacePoint2D { x : 0.0, y : 0.0 })
                      >>> (_graphData <<< _pane graphId <<< _zoom .~ 1.0)

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
      state <- H.get
      exceptTJSON <- H.liftEffect $ FileReader.result fileReader
      case
        lmap (show <<< map renderForeignError)
        $ unwrap $ runExceptT do
            json <- Foreign.readString exceptTJSON
            graphDataFromJSON state json
      of
        Left errors ->
          H.liftEffect $ log $ "Failed to parse JSON: " <> errors
        Right deserialisedGraphData -> do
          H.liftEffect $ log $ "Loading saved graph encoded with AppOperation version "
                               <> deserialisedGraphData.metadata.version
          --reducedAppState <- H.liftEffect $ removeGraphData graphId state
          reducedAppState <- H.get
          newAppState <- H.liftEffect $ interpretAppOperation deserialisedGraphData.appStateOp reducedAppState
          H.put $ newAppState
                    { history = Map.insert deserialisedGraphData.graphId deserialisedGraphData.history newAppState.history
                    , undone  = Map.insert deserialisedGraphData.graphId deserialisedGraphData.undone  newAppState.undone
                    }
          -- Keep text fields in sync
          handleAction $ UpdateContentEditableText
          state' <- H.get
          H.liftEffect $ log $ "put app state.graphData : " <> show state'.graphData
      H.unsubscribe subscriptionId

    SaveLocalFile -> do
      state <- H.get
      timestamp <- H.liftEffect now
      case do
        graphId <- state.focusedPane
        let metadata = { version : appOperationVersion , timestamp : timestamp }
        graphDataToJSON graphId state metadata
      of
        Nothing -> pure unit
        Just stateJSON ->
          let
            title = fromMaybe "untitled" $ graphTitle state.graphData
          in
            H.liftEffect $ saveJSON stateJSON $ title <> ".graph.json"

    Keypress keyboardEvent -> do
      H.liftEffect $ log $ show $ KE.key keyboardEvent
      case KE.key keyboardEvent of
        -- Kill audio
        " " -> if not (KE.ctrlKey keyboardEvent) then pure unit else do
          H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
          H.liftEffect $ WE.stopPropagation $ KE.toEvent keyboardEvent
          state <- H.get
          H.liftEffect $ WebAudio.close state.synth.synthState.audioContext
          H.liftEffect $ log "Closed audio context"

        -- Refresh synth nodes
        "1" -> if not (KE.ctrlKey keyboardEvent) then pure unit else do
          H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
          H.liftEffect $ WE.stopPropagation $ KE.toEvent keyboardEvent
          state <- H.get
          for_ state.graphData.nodes \node ->
            case parseSynthNodeType node.text of
              Nothing -> pure unit
              Just synthNodeType ->
                modifyM $ doAppOperation node.graphId $ AppOperation
                  $ createSynthNode node.id $ freshSynthNodeParams synthNodeType
          for_ (edgeArray state.graphData) \edge ->
            case Map.lookup edge.id.source state.graphData.nodes of
              Nothing -> pure unit
              Just source ->
                modifyM $ doAppOperation source.graphId $ AppOperation
                  $ connectSynthNodes edge.id

        -- Undo
        "z" -> if not (KE.ctrlKey keyboardEvent) then pure unit else do
          H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
          H.liftEffect $ WE.stopPropagation $ KE.toEvent keyboardEvent
          state <- H.get
          case state.focusedPane of
            Nothing -> pure unit
            Just graphId -> do
              modifyM $ doAppOperation graphId $ AppOperation $ undo graphId
          -- Keep text state in sync
          handleAction $ UpdateContentEditableText

        -- Redo
        "y" -> if not (KE.ctrlKey keyboardEvent) then pure unit else do
          H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
          H.liftEffect $ WE.stopPropagation $ KE.toEvent keyboardEvent
          state <- H.get
          case state.focusedPane of
            Nothing -> pure unit
            Just graphId -> do
              modifyM $ doAppOperation graphId $ AppOperation $ redo graphId
          -- Keep text state in sync
          handleAction $ UpdateContentEditableText

        -- Load saved graph from local JSON file
        "l" -> if not (KE.ctrlKey keyboardEvent) then pure unit else do
          H.liftEffect $ loadFile

        -- Save current graph to local JSON file
        "s" -> if not (KE.ctrlKey keyboardEvent) then pure unit else do
          -- Save app state as a local json file
          H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
          H.liftEffect $ WE.stopPropagation $ KE.toEvent keyboardEvent
          handleAction $ SaveLocalFile

        -- Return to graph origin and reset zoom
        "o" -> if not (KE.ctrlKey keyboardEvent) then pure unit else do
          H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
          H.liftEffect $ WE.stopPropagation $ KE.toEvent keyboardEvent
          handleAction CenterGraphOriginAndZoom

        -- Delete node/edge currently in focus
        "Delete" -> do
          state <- H.get
          case state.focusedPane of
            Nothing -> pure unit
            Just graphId -> handleAction $ DeleteFocus graphId

        -- Unfocus
        "Escape" -> do
          state <- H.get
          for_ (Map.keys state.graphData.panes) \graphId ->
            handleAction $ FocusOn graphId Nothing
          handleAction $ UpdateContentEditableText

        -- reMove pane
        "m" -> if not (KE.ctrlKey keyboardEvent) then pure unit else do
          H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
          state <- H.get
          case state.focusedPane of
            Nothing -> pure unit
            Just graphId -> do
              -- TODO: remove graph data in removePane when monadic ops are removed
              modifyM $ removeGraphData graphId
              modifyM $ interpretAppOperation $ AppOperation $ removePane graphId

        -- TODO: highlighting?
        ---- Highlight currently focused node/edge
        --"s" -> H.modify_ $ _graph %~ toggleHighlightFocus

        _ -> pure unit

    DoNothing ->
      pure unit

    GainDragStart graphId nodeId initialGain mouseEvent -> do
      H.subscribe' \subscriptionId ->
        Drag.dragEventSource mouseEvent
        \e -> GainDragMove e graphId nodeId initialGain subscriptionId
      handleAction $ FocusOn graphId $ Just $ FocusNode nodeId

    GainDragMove (Drag.Move _ dragData) graphId nodeId initialGain _ -> do
      state <- H.get
      case Map.lookup graphId state.graphData.panes of
        Nothing -> pure unit
        Just pane ->
          let
            gainOffsetPageSpace = - dragData.offsetY * pane.zoom
            initialGainPageSpace = amplifierBoxSize * (amplifierGainToControl initialGain)
            newGainPageSpace = Math.max 0.0 (initialGainPageSpace + gainOffsetPageSpace)
            newGain = Math.max 0.0 $ controlToAmplifierGain (newGainPageSpace / amplifierBoxSize)
          in
            case Tuple (Map.lookup nodeId state.synth.synthParams)
                       (Map.lookup nodeId state.graphData.nodes) of
              Tuple (Just (AmplifierParams oldGain)) (Just node) -> do
                modifyM $ doAppOperation graphId $ AppOperation
                  $ updateSynthParam node.id AmplifierGain oldGain newGain
              _ -> pure unit

    GainDragMove (Drag.Done _) _ _ _ subscriptionId ->
      H.unsubscribe subscriptionId

    DelayDragStart graphId nodeId initialPeriod mouseEvent -> do
      H.subscribe' \subscriptionId ->
        Drag.dragEventSource mouseEvent
          \e -> DelayDragMove e graphId nodeId initialPeriod subscriptionId
      handleAction $ FocusOn graphId $ Just $ FocusNode nodeId

    DelayDragMove (Drag.Move _ dragData) graphId nodeId initialPeriod _ -> do
      state <- H.get
      case Map.lookup graphId state.graphData.panes of
        Nothing -> pure unit
        Just pane ->
          let
            periodOffsetPageSpace = dragData.offsetX * pane.zoom
            initialPeriodPageSpace = delayPeriodToGraphSpace initialPeriod
            newPeriod = Math.max 0.0 $ pageSpaceToDelayPeriod (periodOffsetPageSpace + initialPeriodPageSpace)
          in
            case Tuple (Map.lookup nodeId state.synth.synthParams)
                       (Map.lookup nodeId state.graphData.nodes) of
              Tuple (Just (DelayParams oldPeriod)) (Just node) -> do
                modifyM $ doAppOperation graphId $ AppOperation
                  $ updateSynthParam node.id DelayPeriod oldPeriod newPeriod
              _ -> pure unit

    DelayDragMove (Drag.Done _) _ _ _ subscriptionId ->
      H.unsubscribe subscriptionId

  handleQuery :: forall a. Query a -> H.HalogenM AppState Action Slots Message Aff (Maybe a)
  handleQuery = case _ of
    UpdateBoundingRect a -> do
      map (const a) <$> runMaybeT do
        panesElement <- MaybeT $ H.getHTMLElementRef (H.RefLabel "panes")
        panesRect <- lift $ H.liftEffect $ WHE.getBoundingClientRect panesElement
        state <- H.get
        lift $ H.liftEffect $ log $ "updating bounding rect to: " <> show panesRect <> "from: " <> show state.windowBoundingRect
        lift $ modifyM $ interpretAppOperation $ AppOperation
          $ rescaleWindow panesRect
        lift $ H.liftEffect $ log $ "new pane bounding rects: " <> (show $ (_.boundingRect >>> show) <$> Map.values state.graphData.panes)


------
-- Utilities

mouseEventPosition :: ME.MouseEvent -> PageSpacePoint2D
mouseEventPosition e = PageSpacePoint2D { x : toNumber $ ME.pageX e
                                        , y : toNumber $ ME.pageY e
                                        }

euclideanDistance :: GraphSpacePoint2D -> GraphSpacePoint2D -> Number
euclideanDistance (GraphSpacePoint2D pos1) (GraphSpacePoint2D pos2) =
  Math.sqrt
  $ (Math.pow (pos1.x - pos2.x) 2.0)
  + (Math.pow (pos1.y - pos2.y) 2.0)

drawingEdgeWithinNodeHalo :: DrawingEdge -> GraphView -> Node -> Boolean
drawingEdgeWithinNodeHalo drawingEdgeState pane node =
  let
    pointPositionGraphSpace = drawingEdgeState.pointPosition # toGraphSpace pane
  in
    haloRadius > euclideanDistance node.position pointPositionGraphSpace

tearDownSynthState :: AppState -> Effect Synth
tearDownSynthState appState =
  let
    teardownOps = Array.catMaybes $ Array.fromFoldable $ appState.graphData.nodes <#> \node ->
      case Map.lookup node.id appState.synth.synthParams of
        Nothing -> Nothing
        Just synthNodeParams -> Just $ deleteSynthNode node.id synthNodeParams
    deleteNodeFuncs = (Tuple.fst <<< Run.extract <<< interpretSynthOp) <$> teardownOps
  in
    foldl bind (pure appState.synth) deleteNodeFuncs

initializeSynthNodes :: UninitializedAppState -> Synth -> Effect Synth
initializeSynthNodes uninitializedAppState uninitializedSynth =
  let
    synthParams = uninitializedSynth.synthParams
    graphData' = uninitializedAppState.graphData
    initializeOps = Array.catMaybes $ Array.fromFoldable $ graphData'.nodes <#> \node ->
      case Map.lookup node.id synthParams of
        Nothing -> Nothing
        Just synthNodeParams -> Just $ createSynthNode node.id synthNodeParams
    createNodeFuncs = (Tuple.fst <<< Run.extract <<< interpretSynthOp) <$> initializeOps
  in
    foldl bind (pure uninitializedSynth) createNodeFuncs

initializeSynthState :: UninitializedAppState -> Effect AppState
initializeSynthState uninitializedAppState = do
  audioContext <- WebAudio.newAudioContext
  let
    bareSynthState = { audioContext : audioContext, synthNodeStates : Map.empty }
    uninitializedSynth = { synthParams : uninitializedAppState.synth
                         , synthState : bareSynthState
                         }
  synth <- initializeSynthNodes uninitializedAppState uninitializedSynth
  pure $ uninitializedAppState { synth = synth }

toUninitializedAppState :: AppState -> Effect UninitializedAppState
toUninitializedAppState state = do
  WebAudio.close state.synth.synthState.audioContext
  pure $ state { synth = state.synth.synthParams }

modifyM :: (AppState -> Effect AppState) -> H.HalogenM AppState Action Slots Message Aff Unit
modifyM op = do
  state <- H.get
  updatedState <- H.liftEffect $ op state
  H.put updatedState

edgeTextFieldIdStr :: Edge -> String
edgeTextFieldIdStr edge = edgeIdStr edge <> "textField"

edgeSourcePosition :: AppState -> Edge -> GraphView -> Maybe GraphSpacePoint2D
edgeSourcePosition state edge renderPane = do
  sourceNode <- Map.lookup edge.id.source state.graphData.nodes
  sourcePane <- Map.lookup edge.id.sourceGraph state.graphData.panes
  let GraphSpacePoint2D sourcePos = sourceNode.position # toPageSpace sourcePane # toGraphSpace renderPane
  pure case Map.lookup edge.id.source state.synth.synthParams of
    -- Special case for delay nodes
    -- TODO: remove special case when synth code is removed
    Just (DelayParams delayPeriod) ->
      GraphSpacePoint2D
        { x : sourcePos.x
              + (delayPeriodToGraphSpace delayPeriod)
        , y : sourcePos.y
        }
    _  -> GraphSpacePoint2D sourcePos

edgeTargetPosition :: AppState -> Edge -> GraphView -> Maybe GraphSpacePoint2D
edgeTargetPosition state edge renderPane = do
  targetNode <- Map.lookup edge.id.target state.graphData.nodes
  targetPane <- Map.lookup edge.id.targetGraph state.graphData.panes
  pure $ targetNode.position # toPageSpace targetPane # toGraphSpace renderPane

edgeMidPosition :: AppState -> Edge -> GraphView -> Maybe GraphSpacePoint2D
edgeMidPosition state edge renderPane = do
  targetNode <- Map.lookup edge.id.target state.graphData.nodes
  GraphSpacePoint2D sourcePos <- edgeSourcePosition state edge renderPane
  GraphSpacePoint2D targetPos <- edgeTargetPosition state edge renderPane
  pure $ GraphSpacePoint2D
           { x : (sourcePos.x + targetPos.x) / 2.0
           , y : (sourcePos.y + targetPos.y) / 2.0
           }
