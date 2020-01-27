module GraphComponent where

import Prelude

import AppOperation (AppOperation(..), appOperationVersion, undo, redo)
import AppOperation.GraphOp (GraphOpF(..), _graphOp, connectSubgraph, deleteEdge, deleteNode, insertEdge, insertNode, moveNode, updateEdgeText, updateNodeText, updateTitle)
import AppOperation.Interpreter (doAppOperation, interpretAppOperation)
import AppOperation.QueryServerOp (connectSubgraphIfTitleExists, createGraph, openGraphsWithSubgraph)
import AppOperation.UIOp (insertPane, moveGraphOrigin, removePane)
import AppState (AppState, DrawingEdge, DrawingEdgeId, HoveredElementId(..), Shape, _drawingEdgePosition, _drawingEdgeTargetGraph, _drawingEdges, _focus, _graphData, edgeIdStr, emptyAppState)
import AppState.Foreign (graphDataFromJSON, graphDataToJSON)
import CSS as CSS
import ContentEditable.SVGComponent as SVGContentEditable
import Control.Alt ((<|>))
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT, lift)
import Core (Edge, EdgeId, Focus(..), GraphData, GraphId, GraphSpacePoint2D(..), GraphTitle, GraphView, Node, NodeId, PageSpacePoint2D(..), _origin, _pane, _zoom, allEdgesBetweenGraphs, allEdgesTouchingNode, edgeArray, freshNode, selectGraphData, separateGraphs, toGraphSpace, toPageSpace)
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Lens ((%~), (.~), (^.), (^?))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust, fromMaybe)
import Data.Newtype (unwrap)
import Data.String (joinWith)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.UUID (genUUID)
import Data.UUID as UUID
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Console as Console
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
import Server.Config (config)
import Svg.Attributes as SA
import Svg.Elements as SE
import Svg.Elements.Keyed as SK
import Svg.Types as SVGT
import UI.Constants (defaultTextFieldShape, edgeTextBoxOffset, groupNodeRadius, haloRadius, maxTextFieldShape, nodeBorderRadius, nodeRadius, nodeTextBoxOffset, zoomScaling, defaultTitleShape, maxTitleShape, invalidIndicatorOffset, invalidIndicatorSize)
import UI.Panes (zoomAtPoint, paneContainingPoint, rescaleWindow)
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
  | UpdateContentEditableText GraphId
  | UpdateNodeContentEditableText NodeId
  | BackgroundDragStart GraphId PageSpacePoint2D ME.MouseEvent
  | BackgroundDragMove Drag.DragEvent GraphId PageSpacePoint2D H.SubscriptionId
  | NodeDragStart GraphId NodeId GraphSpacePoint2D ME.MouseEvent
  | NodeDragMove Drag.DragEvent GraphId NodeId GraphSpacePoint2D H.SubscriptionId
  | EdgeDrawStart GraphView DrawingEdgeId ME.MouseEvent
  | EdgeDrawMove Drag.DragEvent GraphId DrawingEdgeId H.SubscriptionId
  | NodeTextInput NodeId SVGContentEditable.Message
  | EdgeTextInput GraphId EdgeId SVGContentEditable.Message
  | TitleTextInput GraphId SVGContentEditable.Message
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
  | Keyup KE.KeyboardEvent
  | DoNothing

type Input = Shape

initialState :: Input -> AppState
initialState windowShape =
  emptyAppState { windowBoundingRect =
                  { left : 0.0
                  , width : windowShape.width
                  , right : windowShape.height
                  , top : 0.0
                  , height : windowShape.height
                  , bottom : windowShape.width
                  }
                }

data Query a
  = UpdateBoundingRect a
  | ReceiveOperation (AppOperation Unit) a

data Message
  = SendOperation (AppOperation Unit)

-- Slot type for parent components using a child GraphComponent
type Slot = H.Slot Query Message

type Slots =
  ( nodeTextField  :: SVGContentEditable.Slot NodeId
  , edgeTextField  :: SVGContentEditable.Slot EdgeId
  , titleTextField :: SVGContentEditable.Slot GraphId
  )

_nodeTextField :: SProxy "nodeTextField"
_nodeTextField = SProxy

_edgeTextField :: SProxy "edgeTextField"
_edgeTextField = SProxy

_titleTextField :: SProxy "titleTextField"
_titleTextField = SProxy

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

renderGraphNode :: AppState -> GraphView -> Node -> Tuple String (H.ComponentHTML Action Slots Aff)
renderGraphNode state pane node =
  let
    nodeFocused = pane.focus == Just (FocusNode node.id)
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
      , if nodeFocused
        then Just "focused"
        else Nothing
      ]
    nodeBorderClasses =
        joinWith " " $ Array.catMaybes
        [ Just "nodeBorder"
        -- TODO: use highlighting
        --, if (Set.member node.id state.graphData.highlighted)
        --  then Just "highlighted"
        --  else Nothing
        , if hoveredOverBorder
             &&
             (not drawingEdgeOverNode)
             &&
             (not nodeFocused)
          then Just "hovered"
          else Nothing
        ]
    haloClasses =
      joinWith " " $ Array.catMaybes
      [ Just "nodeHalo"
      , if nodeFocused
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
    GraphSpacePoint2D nodePos = node.position
    shiftedNodeHTML = SE.g
               [ SA.transform [ SVGT.Translate nodePos.x nodePos.y ] ]
               graphNodeHTML
  in
    Tuple (show node.id) shiftedNodeHTML

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
                then "url(#arrow-to-group)"
                else "url(#arrow)"
  pure $ Tuple (show edge.id <> "_edge_" <> show renderPane.graphId) $
    SE.line
    [ SA.class_ edgeClasses
    , SA.x1 $ show sourcePos.x
    , SA.y1 $ show sourcePos.y
    , SA.x2 $ show targetPos.x
    , SA.y2 $ show targetPos.y
    , SA.markerEnd markerRef
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

renderTitle :: GraphId -> String -> Tuple String (H.ComponentHTML Action Slots Aff)
renderTitle graphId titleText =
  Tuple (show graphId <> "_title") $
    SE.g
    [ SA.class_ "title"
    , HE.onMouseDown \e -> Just
                           $ StopPropagation (ME.toEvent e)
                           $ DoNothing
    ]
    [ HH.slot
      _titleTextField
      graphId
      SVGContentEditable.svgContenteditable
      { shape : defaultTitleShape
      , initialText : titleText
      , maxShape : maxTitleShape
      , fitContentDynamic : true
      }
      (Just <<< TitleTextInput graphId)
    ]

renderTitleInvalidIndicator :: GraphId -> Tuple String (H.ComponentHTML Action Slots Aff)
renderTitleInvalidIndicator graphId =
  Tuple (show graphId <> "_titleInvalidIndicator") $
    SE.rect
    [ SA.class_ "invalidIndicator"
    , SA.width  $ show invalidIndicatorSize
    , SA.height $ show invalidIndicatorSize
    , SA.x $ show invalidIndicatorOffset.x
    , SA.y $ show invalidIndicatorOffset.y
    ]

renderSingleGraph :: AppState -> GraphView -> GraphData -> H.ComponentHTML Action Slots Aff
renderSingleGraph state renderPane singleGraph =
  let
    nodes = singleGraph.nodes # Map.values # Array.fromFoldable

    keyedNodes = nodes <#> renderGraphNode state renderPane

    allNodesInOtherGraphs = state.graphData.nodes
                            # Map.filter (\node -> node.graphId /= renderPane.graphId)
                            # Map.values # Array.fromFoldable

    ghostNodes          = Array.catMaybes $ allNodesInOtherGraphs <#> renderGhostNode state renderPane

    edges               = Array.nub $ edgeArray singleGraph <> allEdgesBetweenGraphs state.graphData

    keyedEdges          = Array.mapMaybe (renderEdge state renderPane) edges

    keyedEdgeTextFields = Array.mapMaybe (renderEdgeTextField state renderPane) edges

    keyedEdgeBorders    = Array.mapMaybe (renderEdgeBorder state renderPane) edges

    drawingEdges        = state.drawingEdges # Map.values # Array.fromFoldable

    keyedDrawingEdges   = Array.mapMaybe (renderDrawingEdge state renderPane) drawingEdges

    keyedTitle = case Map.lookup renderPane.graphId singleGraph.titles of
      Nothing -> []
      Just title -> [ renderTitle renderPane.graphId title.titleText ]

    keyedTitleInvalidIndicator = case Map.lookup renderPane.graphId singleGraph.titles of
      Nothing -> []
      Just title -> if title.isValid
                    then []
                    else [ renderTitleInvalidIndicator renderPane.graphId ]

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
         <> keyedEdges
         <> keyedEdgeBorders
         <> keyedDrawingEdges
         <> keyedNodes
         <> keyedEdgeTextFields
         <> keyedTitle
         <> keyedTitleInvalidIndicator
        )
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
    -- Subscribe to updates in bounding rectangle
    window <- H.liftEffect $ WH.window
    _ <- H.subscribe $ ES.eventListenerEventSource
                       (WE.EventType "resize")
                       (WHW.toEventTarget window)
                       \event -> Just $ EvalQuery $ UpdateBoundingRect unit
    -- Add keydown event listener to body
    document <- H.liftEffect $ WHW.document =<< WH.window
    _ <- H.subscribe $ ES.eventListenerEventSource KET.keydown (HTMLDocument.toEventTarget document) (map Keypress <<< KE.fromEvent)
    -- keyup listener
    _ <- H.subscribe $ ES.eventListenerEventSource KET.keyup   (HTMLDocument.toEventTarget document) (map Keyup <<< KE.fromEvent)

    pure unit

  UpdateContentEditableText graphId -> do
    state <- H.get
    case selectGraphData graphId state.graphData of
      Nothing -> pure unit
      Just singleGraph -> do
        for_ singleGraph.nodes \node -> do
          handleAction $ UpdateNodeContentEditableText node.id
        for_ (edgeArray singleGraph) \edge -> do
          H.query _edgeTextField edge.id $ H.tell $ SVGContentEditable.SetText edge.text
        let
          titles :: Array (Tuple GraphId GraphTitle)
          titles = Map.toUnfoldable singleGraph.titles
        for_ titles \(Tuple graphId' title) -> do
          H.query _titleTextField graphId' $ H.tell $ SVGContentEditable.SetText title.titleText

  UpdateNodeContentEditableText nodeId -> do
    state <- H.get
    case Map.lookup nodeId state.graphData.nodes of
      Nothing -> pure unit
      Just node -> do
        _ <- H.query _nodeTextField node.id $ H.tell $ SVGContentEditable.SetText node.text
        pure unit

  NodeTextInput nodeId (SVGContentEditable.TextUpdate text) -> do
    state <- H.get
    case Map.lookup nodeId state.graphData.nodes of
      Nothing -> pure unit
      Just node ->
        let
          op = AppOperation node.graphId $ updateNodeText node text
        in do
          interpretAndSend op
          handleAction $ FocusOn node.graphId $ Just $ FocusNode node.id

  EdgeTextInput graphId edgeId (SVGContentEditable.TextUpdate text) -> do
    state <- H.get
    case Map.lookup edgeId.source state.graphData.edges.sourceTarget >>= Map.lookup edgeId.target of
      Nothing -> pure unit
      Just edge ->
        let
          op = AppOperation graphId $ updateEdgeText edge text
        in do
          interpretAndSend op
          handleAction $ FocusOn graphId $ Just $ FocusEdge edgeId []

  TitleTextInput graphId (SVGContentEditable.TextUpdate newTitleText) -> do
    state <- H.get
    case Map.lookup graphId state.graphData.titles of
      Nothing -> pure unit
      Just oldTitle ->
        let
          op = AppOperation graphId $ updateTitle graphId oldTitle.titleText newTitleText
        in
          interpretAndSend op

  BackgroundDragStart graphId initialGraphOrigin mouseEvent -> do
    handleAction $ FocusOn graphId Nothing
    H.subscribe' \subscriptionId ->
      Drag.dragEventSource mouseEvent
        \e -> BackgroundDragMove e graphId initialGraphOrigin subscriptionId

  BackgroundDragMove (Drag.Move _ dragData) graphId (PageSpacePoint2D initialGraphOrigin) _ -> do
    state <- H.get
    let
      newGraphOrigin =
        PageSpacePoint2D
          { x : initialGraphOrigin.x + dragData.offsetX
          , y : initialGraphOrigin.y + dragData.offsetY
          }
      op = AppOperation graphId $ moveGraphOrigin graphId newGraphOrigin
    interpretAndSend op


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
          Just node ->
            let
              op = AppOperation node.graphId $ moveNode node newNodePos
            in
              interpretAndSend op

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
            # List.filter (\node -> node.id /= drawingEdgeId)
            # List.filter (drawingEdgeWithinNodeHalo drawingEdgeState targetPane)
            # List.head
        pure { source      : drawingEdgeState.source
             , sourceGraph : drawingEdgeState.sourceGraph
             , target      : drawingEdgeTarget.id
             , targetGraph : drawingEdgeTarget.graphId
             }
    -- Remove the drawing edge
    H.modify_ $ _{ drawingEdges = Map.delete drawingEdgeId state.drawingEdges }
    H.unsubscribe subscriptionId
    case maybeNewEdgeId of
      Nothing -> pure unit
      Just newEdgeId -> do
        handleAction $ AppCreateEdge graphId newEdgeId

  AppCreateNode pane mouseEvent -> do
    newNodeId <- H.liftEffect genUUID
    state <- H.get
    let
      newNode = freshNode pane.graphId newNodeId
      newNodePosition = mouseEventPosition mouseEvent
      newNodeGraphSpacePos = toGraphSpace pane newNodePosition
      op = AppOperation pane.graphId do
        insertNode pane.graphId newNodeId
        moveNode newNode newNodeGraphSpacePos
    interpretAndSend op
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
    let op = AppOperation node.graphId $ deleteNode state'.graphData node
    interpretAndSend op

  AppCreateEdge graphId edgeId ->
    let
      op = AppOperation graphId $ insertEdge edgeId
    in do
      interpretAndSend op
      handleAction $ FocusOn edgeId.sourceGraph $ Just $ FocusEdge edgeId []

  AppDeleteEdge graphId edge ->
    let
      op = AppOperation graphId $ deleteEdge edge
    in do
      interpretAndSend op
      handleAction $ FocusOn edge.id.sourceGraph $ Just $ FocusNode edge.id.source

  FocusOn graphId newFocus -> do
    H.modify_ $
      (_focus graphId .~ newFocus)
      >>> _{ focusedPane = Just graphId }
    _ <- case newFocus of
      Just (FocusNode nodeId)   -> H.query _nodeTextField nodeId $ H.tell SVGContentEditable.Focus
      Just (FocusEdge edgeId _) -> H.query _edgeTextField edgeId $ H.tell SVGContentEditable.Focus
      _                         -> pure $ Just unit
    pure unit

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
          op = zoomAtPoint
                 newZoom
                 (mouseEventPosition $ WhE.toMouseEvent wheelEvent)
                 pane
        in
          interpretAndSend op

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
        H.liftEffect $ Console.log $ "Failed to parse JSON: " <> errors
      Right deserialisedGraphData -> do
        H.liftEffect $ Console.log $ "Loading saved graph encoded with AppOperation version "
                                     <> deserialisedGraphData.metadata.version
        H.put $ interpretAppOperation deserialisedGraphData.appStateOp state
        -- Keep text fields in sync
        let AppOperation graphId _ = deserialisedGraphData.appStateOp
        handleAction $ UpdateContentEditableText graphId
    H.unsubscribe subscriptionId

  SaveLocalFile -> do
    state <- H.get
    timestamp <- H.liftEffect now
    case do
      graphId <- state.focusedPane
      focusedGraphData <- selectGraphData graphId state.graphData
      history <- Map.lookup graphId state.history
      undone  <- Map.lookup graphId state.undone
      let metadata = { version : appOperationVersion , timestamp : timestamp }
      let title = fromMaybe "untitled" $ _.titleText <$> Map.lookup graphId state.graphData.titles
      pure $ Tuple (graphDataToJSON graphId focusedGraphData history undone metadata)
                   title
    of
      Nothing -> pure unit
      Just (Tuple stateJSON title) ->
        H.liftEffect $ saveJSON stateJSON $ title <> ".graph.json"

  Keyup keyboardEvent -> handleKeyup keyboardEvent

  Keypress keyboardEvent -> handleKeypress keyboardEvent

  DoNothing ->
    pure unit

handleQuery :: forall a. Query a -> H.HalogenM AppState Action Slots Message Aff (Maybe a)
handleQuery = case _ of
  UpdateBoundingRect a -> do
    map (const a) <$> runMaybeT do
      panesElement <- MaybeT $ H.getHTMLElementRef (H.RefLabel "panes")
      panesRect <- lift $ H.liftEffect $ WHE.getBoundingClientRect panesElement
      lift $ H.modify_ $ rescaleWindow panesRect

  ReceiveOperation op a -> do
    state <- H.get
    H.modify_ $ interpretAppOperation op
    case opUpdatesNodeText op of
      Nothing -> pure unit
      Just nodeId -> handleAction $ UpdateNodeContentEditableText nodeId
    pure $ Just a


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

interpretAndSend :: AppOperation Unit -> H.HalogenM AppState Action Slots Message Aff Unit
interpretAndSend op = do
  H.raise $ SendOperation op
  H.modify_ $ doAppOperation op

edgeTextFieldIdStr :: Edge -> String
edgeTextFieldIdStr edge = edgeIdStr edge <> "textField"

edgeSourcePosition :: AppState -> Edge -> GraphView -> Maybe GraphSpacePoint2D
edgeSourcePosition state edge renderPane = do
  sourceNode <- Map.lookup edge.id.source state.graphData.nodes
  sourcePane <- Map.lookup edge.id.sourceGraph state.graphData.panes
  pure $ sourceNode.position # toPageSpace sourcePane # toGraphSpace renderPane

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

-- | Utility to indicate if an operation received from the server updates a node's
-- | text, in which case the contenteditable field needs to be refreshed.
opUpdatesNodeText :: AppOperation Unit -> Maybe NodeId
opUpdatesNodeText (AppOperation graphId runOp) =
  case Run.peel runOp of
    Left opV -> opV # (Run.on _graphOp (case _ of
        UpdateNodeText nodeId from to next ->
          Just nodeId
        _ -> Nothing)
     (Run.default Nothing))
    _ -> Nothing

focusNode :: AppState -> Maybe Node
focusNode state = do
  graphId    <- state.focusedPane
  pane       <- Map.lookup graphId state.graphData.panes
  focus      <- pane.focus
  nodeId     <- case focus of
    FocusNode nodeId -> Just nodeId
    _ -> Nothing
  Map.lookup nodeId state.graphData.nodes

focusNodeSubgraph :: AppState -> Maybe GraphId
focusNodeSubgraph state = do
  node <- focusNode state
  subgraphId <- node.subgraph
  pure subgraphId


------
-- Key Commands

handleKeyup :: KE.KeyboardEvent -> H.HalogenM AppState Action Slots Message Aff Unit
handleKeyup keyboardEvent =
      case KE.key keyboardEvent of
        " " -> do
          state <- H.get
          H.modify_ _{ keyHoldState = state.keyHoldState { spaceDown = false} }

        _ -> pure unit

handleKeypress :: KE.KeyboardEvent -> H.HalogenM AppState Action Slots Message Aff Unit
handleKeypress keyboardEvent = do
      H.liftEffect $ Console.log $ show $ KE.key keyboardEvent
      case KE.key keyboardEvent of
        " " -> if not (KE.ctrlKey keyboardEvent) then pure unit else do
          H.liftEffect $ WE.preventDefault  $ KE.toEvent keyboardEvent
          state <- H.get
          H.modify_ _{ keyHoldState = state.keyHoldState { spaceDown = true } }

        -- Undo
        "z" -> if not (KE.ctrlKey keyboardEvent) then pure unit else do
          H.liftEffect $ WE.preventDefault  $ KE.toEvent keyboardEvent
          state <- H.get
          case state.focusedPane of
            Nothing -> pure unit
            Just graphId ->
              let
                op = AppOperation graphId $ undo graphId
              in do
                interpretAndSend op
                -- Keep text state in sync
                handleAction $ UpdateContentEditableText graphId

        -- Redo
        "y" -> if not (KE.ctrlKey keyboardEvent) then pure unit else do
          H.liftEffect $ WE.preventDefault  $ KE.toEvent keyboardEvent
          state <- H.get
          case state.focusedPane of
            Nothing -> pure unit
            Just graphId ->
              let
                op = AppOperation graphId $ redo graphId
              in do
                interpretAndSend op
                -- Keep text state in sync
                handleAction $ UpdateContentEditableText graphId

        "l" -> if not (KE.ctrlKey keyboardEvent) then pure unit else do
          H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
          -- Load saved graph from local JSON file
          H.liftEffect loadFile

        "s" -> if not (KE.ctrlKey keyboardEvent) then pure unit else do
          H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
          state <- H.get
          if not state.keyHoldState.spaceDown
          then
            -- Save current graph to local JSON file
            handleAction $ SaveLocalFile
          else
            -- connect focused node to Subgraph with same title as node text
            case focusNode state of
              Nothing -> pure unit
              Just node ->
                let
                  op = AppOperation node.graphId $ connectSubgraphIfTitleExists node.id node.text
                in
                  H.raise $ SendOperation op

        "o" -> if not (KE.ctrlKey keyboardEvent) then pure unit else do
          H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
          state <- H.get
          if not state.keyHoldState.spaceDown
          then do
            -- Return to graph origin and reset zoom
            handleAction CenterGraphOriginAndZoom
          else
            -- Open subgraph in new pane
            if state.keyHoldState.spaceDown
            then unit <$ runMaybeT do
              subgraphId <- MaybeT $ pure $ focusNodeSubgraph state
              lift $ H.raise $ SendOperation $ AppOperation subgraphId $ insertPane subgraphId
            else
              pure unit

        -- Delete node/edge currently in focus
        "Delete" -> do
          state <- H.get
          case state.focusedPane of
            Nothing -> pure unit
            Just graphId -> handleAction $ DeleteFocus graphId

        -- Unfocus
        "Escape" -> do
          state <- H.get
          for_ (Map.keys state.graphData.panes) \graphId -> do
            handleAction $ FocusOn graphId Nothing
            handleAction $ UpdateContentEditableText graphId

        -- Close pane
        "c" -> if not (KE.ctrlKey keyboardEvent) then pure unit else do
          state <- H.get
          if not state.keyHoldState.spaceDown
          then
            pure unit
          else unit <$ runMaybeT do
            lift $ H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
            graphId <- MaybeT $ pure $ state.focusedPane
            lift $ interpretAndSend $ AppOperation graphId $ removePane graphId

        -- new Graph
        -- TODO: if current selection is non-empty, push it down into the new subgraph
        "g" -> if not (KE.ctrlKey keyboardEvent) then pure unit else do
          state <- H.get
          if not state.keyHoldState.spaceDown
          then
            pure unit
          else
            case Tuple (focusNode state) (focusNode state >>= _.subgraph) of
              Tuple (Just node) Nothing -> do
                -- create new graph as a subgraph of focused node
                H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
                newGraphId <- H.liftEffect genUUID
                -- perform these ops separately so that connectSubraph is undoable
                H.raise $ SendOperation $ AppOperation node.graphId $ createGraph newGraphId node.text
                H.raise $ SendOperation $ AppOperation node.graphId $ connectSubgraph node (Just newGraphId)
              _ -> do
                -- create new graph in new pane
                H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
                newGraphId <- H.liftEffect genUUID
                let op = AppOperation newGraphId do
                      insertPane newGraphId
                      updateTitle newGraphId "" $ UUID.toString newGraphId
                interpretAndSend op

        -- TODO: highlighting
        ---- Highlight currently focused node/edge
        --"h" -> H.modify_ $ _graph %~ toggleHighlightFocus

        -- jump Down into the subgraph of the focused node
        "d" -> if not (KE.ctrlKey keyboardEvent) then pure unit else do
          state <- H.get
          if not state.keyHoldState.spaceDown
          then
            pure unit
          else
            case Tuple state.focusedPane (focusNodeSubgraph state) of
              Tuple (Just focusedGraphId) (Just subgraphId) ->
                let
                  op = AppOperation focusedGraphId do
                    removePane focusedGraphId
                    insertPane subgraphId
                in do
                  H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
                  interpretAndSend op
              _ -> pure unit

        -- jump Up to the graphs that have nodes that point to the current graph
        -- as a subgraph
        "u" -> if not (KE.ctrlKey keyboardEvent) then pure unit else do
          state <- H.get
          if not state.keyHoldState.spaceDown
          then
            pure unit
          else do
            H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
            unit <$ runMaybeT do
              focusedGraphId <- MaybeT $ pure state.focusedPane
              lift $ interpretAndSend $ AppOperation focusedGraphId $ openGraphsWithSubgraph focusedGraphId

        -- load the Knowledge navigator
        "k" -> if not (KE.ctrlKey keyboardEvent) then pure unit else do
          state <- H.get
          if not state.keyHoldState.spaceDown
          then
            pure unit
          else do
            H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
            H.raise $ SendOperation $ AppOperation config.knowledgeNavigatorId $
              insertPane config.knowledgeNavigatorId

        _ -> pure unit
