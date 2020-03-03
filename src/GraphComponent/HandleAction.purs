module GraphComponent.HandleAction where

import Prelude

import AppOperation (AppOperation(..), HistoryUpdate(..), encodeMegagraphStateAsAppOperations)
import AppState (AppState, EdgeSourceElement(..), HoveredElementId(..), MegagraphElement(..), _controlDown, _drawingEdgePosition, _drawingEdgeTargetGraph, _drawingEdges, _graph, _graphAtId, _graphs, _hoveredElements, _keyHoldState, _mappingAtId, _mappings, _megagraph, _paneAtId, _spaceDown, lookupMapping)
import ContentEditable.SVGComponent as SVGContentEditable
import Control.Alt ((<|>))
import Control.Monad.Except (runExcept)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT, lift)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Lens ((%~), (.~), (^?), (^.), (^..), traversed)
import Data.Lens.At (at)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Set as Set
import Data.String (joinWith)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Data.UUID (genUUID)
import Data.UUID as UUID
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Console as Console
import Foreign (F, renderForeignError)
import Foreign as Foreign
import Foreign.Generic (encodeJSON, decodeJSON)
import GraphComponent.Types (Action(..), Message(..), Query(..), Slots, _edgeTextField, _nodeTextField, _titleTextField)
import GraphComponent.Utils (mouseEventPosition, updatedNodes)
import Halogen as H
import Halogen.Component.Utils.Drag as Drag
import Halogen.Query.EventSource as ES
import HasuraQuery (renderMutation, renderQuery)
import Interpreter (appOperationToQuery, interpretAppOperation, preprocessAppOperation)
import Math as Math
import Megagraph (Focus(..), GraphEdgeSpacePoint2D(..), GraphId, GraphSpacePoint2D(..), PageEdgeSpacePoint2D(..), PageSpacePoint2D(..), _edge, _edgeMappingEdges, _nodeMappingEdges, _nodes, _origin, _position, _text, _title, _zoom, edgeArray, edgeMidpoint, freshEdge, freshNode, graphEdgeSpaceToGraphSpace, graphSpaceToGraphEdgeSpace, graphSpaceToPageSpace, lookupEdgeById, nodePosition, pageEdgeSpaceToPageSpace, pageSpaceToGraphSpace, pageSpaceToPageEdgeSpace)
import MegagraphOperation (GraphOperation(..), MappingOperation(..), MegagraphOperation(..), MegagraphUpdate, invertMegagraphUpdate)
import Query (graphFetchQuery)
import UI.Constants (zoomScaling)
import UI.Panes (arrangePanes, insertPane, paneContainingPoint, rescaleWindow, zoomAtPoint)
import Utils (tupleApply)
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
import Web.UIEvent.WheelEvent as WhE
import WireData (WireDataRaw, decodeWireData, encodeWireData)

foreign import loadFile :: Effect Unit
foreign import saveJSON :: String -> String -> Effect Unit


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

    -- TODO remove when backend hooked up again
    initialGraphId <- H.liftEffect UUID.genUUID
    H.modify_ $ insertPane initialGraphId

    pure unit

  UpdateContentEditableText graphId -> do
    state <- H.get
    case Map.lookup graphId state.megagraph.graphs <#> _.graph of
      Nothing -> pure unit
      Just singleGraph -> do
        for_ singleGraph.nodes \node ->
          handleAction $ UpdateNodeContentEditableText node.graphId node.id
        for_ (edgeArray singleGraph) \edge ->
          H.query _edgeTextField edge.id $ H.tell $ SVGContentEditable.SetText edge.text
        _ <- H.query _titleTextField graphId $ H.tell $ SVGContentEditable.SetText singleGraph.title.text
        pure unit

  UpdateNodeContentEditableText graphId nodeId -> do
    state <- H.get
    case Map.lookup graphId state.megagraph.graphs >>= (_.graph.nodes >>> Map.lookup nodeId) of
      Nothing -> pure unit
      Just node -> do
        _ <- H.query _nodeTextField node.id $ H.tell $ SVGContentEditable.SetText node.text
        pure unit

  NodeTextInput graphId nodeId text -> do
    state <- H.get
    case state ^? _graphAtId graphId <<< _nodes <<< at nodeId <<< traversed of
      Nothing -> pure unit
      Just node ->
        let
          op = [GraphElementOperation graphId $ UpdateNodes [node] [(node {text = text})]]
          appOp = AppOperation
                  { target: GraphElement graphId
                  , op: op
                  , historyUpdate: NoOp
                  , undoneUpdate: NoOp
                  }
        in do
          interpretAndSend appOp
          handleAction $ UpdateFocus $ Just $ FocusNode node.graphId node.id

  EdgeTextInput graphId edgeId text -> do
    state <- H.get
    case lookupEdgeById edgeId =<< (state ^? _graphAtId graphId) of
      Nothing -> pure unit
      Just edge ->
        let
          op = [GraphElementOperation graphId $ UpdateEdges [edge] [(edge {text = text})]]
          appOp = AppOperation
                  { target: GraphElement graphId
                  , op: op
                  , historyUpdate: NoOp
                  , undoneUpdate: NoOp
                  }
        in do
          interpretAndSend appOp
          handleAction $ UpdateFocus $ Just $ FocusEdge graphId edgeId

  TitleTextInput graphId newTitleText -> do
    state <- H.get
    case state ^? _graphAtId graphId of
      Nothing -> pure unit
      Just graph ->
        let
          appOp = AppOperation
                  { target: GraphElement graphId
                  , op: [GraphElementOperation graphId $ UpdateTitle graph.title.text newTitleText]
                  , historyUpdate: NoOp
                  , undoneUpdate: NoOp
                  }
        in
          interpretAndSend appOp

  BackgroundDragStart graphId initialGraphOrigin mouseEvent -> do
    state <- H.get
    if state.keyHoldState.controlDown
    -- ctrl-click to select the mapping from the current focused pane
    -- to the clicked pane
    then do
      handleAction $ UpdateFocus Nothing
      let
        newMappingFocus sourceGraphId targetGraphId = do
          case lookupMapping sourceGraphId targetGraphId state of
            Nothing -> pure unit
            Just mapping ->
              handleAction $ UpdateFocusPane $ MappingElement mapping.id mapping.sourceGraph mapping.targetGraph
      case state.focusedPane of
        Just (GraphElement sourceGraphId) -> newMappingFocus sourceGraphId graphId
        Just (MappingElement mappingId sourceGraphId targetGraphId) -> newMappingFocus sourceGraphId graphId
        _ -> handleAction $ UpdateFocusPane $ GraphElement graphId
    else do
      handleAction $ UpdateFocus Nothing
      handleAction $ UpdateFocusPane $ GraphElement graphId
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
    H.modify_ $ _paneAtId graphId <<< _origin .~ newGraphOrigin

  BackgroundDragMove (Drag.Done _) _ _ subscriptionId ->
    H.unsubscribe subscriptionId

  NodeDragStart graphId nodeId initialNodePos mouseEvent -> do
    H.subscribe' \subscriptionId ->
      Drag.dragEventSource mouseEvent
        \e -> NodeDragMove e graphId nodeId initialNodePos subscriptionId
    handleAction $ UpdateFocus $ Just $ FocusNode graphId nodeId

  NodeDragMove (Drag.Move _ dragData) graphId nodeId (GraphSpacePoint2D initialNodePos) _ -> do
    state <- H.get
    case state ^? _paneAtId graphId of
      Nothing -> pure unit
      Just pane -> do
        let
          dragOffsetGraphSpace = { x : dragData.offsetX * pane.zoom
                                 , y : dragData.offsetY * pane.zoom
                                 }
          GraphSpacePoint2D newNodePos =
            GraphSpacePoint2D
              { x : initialNodePos.x + dragOffsetGraphSpace.x
              , y : initialNodePos.y + dragOffsetGraphSpace.y
              }
        case state ^? _graphAtId graphId <<< _nodes <<< at nodeId <<< traversed of
          Nothing -> pure unit
          Just node ->
            let
              op = [GraphElementOperation graphId
                    $ UpdateNodes [node] [(node {positionX = newNodePos.x, positionY = newNodePos.y})]]
              appOp = AppOperation
                      { target: GraphElement graphId
                      , op: op
                      , historyUpdate: NoOp
                      , undoneUpdate: NoOp
                      }
            in do
              interpretAndSend appOp

  NodeDragMove (Drag.Done _) graphId nodeId (GraphSpacePoint2D initialNodePos) subscriptionId -> do
    H.unsubscribe subscriptionId
    state <- H.get
    case state ^? _graphAtId graphId <<< _nodes <<< at nodeId <<< traversed of
      Nothing -> pure unit
      Just node ->
        let
          op = [GraphElementOperation graphId
                $ UpdateNodes [(node {positionX = initialNodePos.x, positionY = initialNodePos.y})] [node]]
          appOp = AppOperation
                  { target: GraphElement graphId
                  , op: []
                  , historyUpdate: Insert op
                  , undoneUpdate: NoOp
                  }
        in
          interpretAndSend appOp

  NodeMappingEdgeDragStart mappingId initialNodeMappingEdge mouseEvent -> do
    state <- H.get
    case Map.lookup mappingId state.megagraph.mappings <#> _.mapping of
      Nothing -> pure unit
      Just mapping ->
        H.subscribe' \subscriptionId -> Drag.dragEventSource mouseEvent \e ->
          NodeMappingEdgeDragMove e mapping initialNodeMappingEdge.id initialNodeMappingEdge.midpoint subscriptionId
    handleAction $ UpdateFocus $ Just $ FocusNodeMappingEdge mappingId initialNodeMappingEdge.id

  NodeMappingEdgeDragMove (Drag.Move _ dragData) mapping nodeMappingEdgeId initialMidpoint _ -> do
    state <- H.get
    case do
      currentNodeMappingEdge <- mapping ^. _nodeMappingEdges <<< at nodeMappingEdgeId
      sourcePos <- state ^? _graphAtId mapping.sourceGraph <<< _nodes <<< at currentNodeMappingEdge.sourceNode <<< traversed <<< _position
      sourcePane <- state ^? _paneAtId mapping.sourceGraph
      targetPos <- state ^? _graphAtId mapping.targetGraph <<< _nodes <<< at currentNodeMappingEdge.targetNode <<< traversed <<< _position
      targetPane <- state ^? _paneAtId mapping.targetGraph
      let
        sourcePosPageSpace = graphSpaceToPageSpace sourcePane sourcePos
        targetPosPageSpace = graphSpaceToPageSpace targetPane targetPos
        PageSpacePoint2D initialMidpointCartesian =
          pageEdgeSpaceToPageSpace sourcePosPageSpace targetPosPageSpace initialMidpoint
        newMidpointCartesian =
          PageSpacePoint2D { x : initialMidpointCartesian.x + dragData.offsetX
                           , y : initialMidpointCartesian.y + dragData.offsetY
                           }
        PageEdgeSpacePoint2D newMidpoint = pageSpaceToPageEdgeSpace sourcePosPageSpace targetPosPageSpace newMidpointCartesian
        updatedNodeMappingEdge = currentNodeMappingEdge {midpointAngle = newMidpoint.angle, midpointRadius = newMidpoint.radius}
        op = [ MappingElementOperation mapping.id
               $ UpdateNodeMappingEdges [currentNodeMappingEdge] [updatedNodeMappingEdge]
             ]
        appOp = AppOperation { target : MappingElement mapping.id mapping.sourceGraph mapping.targetGraph
                             , op : op
                             , historyUpdate : NoOp
                             , undoneUpdate : NoOp
                             }
      pure appOp
    of
      Nothing -> pure unit
      Just appOp -> interpretAndSend appOp

  NodeMappingEdgeDragMove (Drag.Done _) mapping nodeMappingEdgeId (PageEdgeSpacePoint2D initialMidpoint) subscriptionId -> do
    H.unsubscribe subscriptionId
    state <- H.get
    case state ^? _mappingAtId mapping.id <<< _nodeMappingEdges <<< at nodeMappingEdgeId <<< traversed of
      Nothing -> pure unit
      Just nodeMappingEdge ->
        let
          initialNodeMappingEdge = nodeMappingEdge {midpointAngle: initialMidpoint.angle, midpointRadius: initialMidpoint.radius}
          op = [ MappingElementOperation mapping.id
                 $ UpdateNodeMappingEdges [initialNodeMappingEdge] [nodeMappingEdge]
               ]
          appOp = AppOperation
                  { target: MappingElement mapping.id mapping.sourceGraph mapping.targetGraph
                  , op: []
                  , historyUpdate: Insert op
                  , undoneUpdate: NoOp
                  }
        in
          interpretAndSend appOp

  EdgeMappingEdgeDragStart mappingId initialEdgeMappingEdge mouseEvent -> do
    state <- H.get
    case Map.lookup mappingId state.megagraph.mappings <#> _.mapping of
      Nothing -> pure unit
      Just mapping ->
        H.subscribe' \subscriptionId -> Drag.dragEventSource mouseEvent \e ->
          EdgeMappingEdgeDragMove e mapping initialEdgeMappingEdge.id initialEdgeMappingEdge.midpoint subscriptionId
    handleAction $ UpdateFocus $ Just $ FocusEdgeMappingEdge mappingId initialEdgeMappingEdge.id

  EdgeMappingEdgeDragMove (Drag.Move _ dragData) mapping edgeMappingEdgeId initialMidpoint _ -> do
    state <- H.get
    case do
      currentEdgeMappingEdge <- mapping ^. _edgeMappingEdges <<< at edgeMappingEdgeId
      sourceGraph <- state ^? _graphAtId mapping.sourceGraph
      sourceEdge <- lookupEdgeById currentEdgeMappingEdge.sourceEdge sourceGraph
      sourcePane <- state ^? _paneAtId mapping.sourceGraph
      sourceEdgeSourcePos <- sourceGraph ^? _nodes <<< at sourceEdge.source <<< traversed <<< _position
      sourceEdgeTargetPos <- sourceGraph ^? _nodes <<< at sourceEdge.target <<< traversed <<< _position
      targetGraph <- state ^? _graphAtId mapping.targetGraph
      targetEdge <- lookupEdgeById currentEdgeMappingEdge.targetEdge targetGraph
      targetPane <- state ^? _paneAtId mapping.targetGraph
      targetEdgeSourcePos <- targetGraph ^? _nodes <<< at targetEdge.source <<< traversed <<< _position
      targetEdgeTargetPos <- targetGraph ^? _nodes <<< at targetEdge.target <<< traversed <<< _position
      let
        sourcePos = graphEdgeSpaceToGraphSpace sourceEdgeSourcePos sourceEdgeTargetPos (edgeMidpoint sourceEdge)
        targetPos = graphEdgeSpaceToGraphSpace targetEdgeSourcePos targetEdgeTargetPos (edgeMidpoint targetEdge)
        sourcePosPageSpace = graphSpaceToPageSpace sourcePane sourcePos
        targetPosPageSpace = graphSpaceToPageSpace targetPane targetPos
        PageSpacePoint2D initialMidpointCartesian =
          pageEdgeSpaceToPageSpace sourcePosPageSpace targetPosPageSpace initialMidpoint
        newMidpointCartesian =
          PageSpacePoint2D { x : initialMidpointCartesian.x + dragData.offsetX
                           , y : initialMidpointCartesian.y + dragData.offsetY
                           }
        PageEdgeSpacePoint2D newMidpoint = pageSpaceToPageEdgeSpace sourcePosPageSpace targetPosPageSpace newMidpointCartesian
        newEdgeMappingEdge = currentEdgeMappingEdge {midpointAngle: newMidpoint.angle, midpointRadius: newMidpoint.radius}
        op = [ MappingElementOperation mapping.id
               $ UpdateEdgeMappingEdges [currentEdgeMappingEdge] [newEdgeMappingEdge]
             ]
        appOp = AppOperation { target : MappingElement mapping.id mapping.sourceGraph mapping.targetGraph
                             , op : op
                             , historyUpdate : NoOp
                             , undoneUpdate : NoOp
                             }
      pure appOp
    of
      Nothing -> pure unit
      Just appOp -> interpretAndSend appOp

  EdgeMappingEdgeDragMove (Drag.Done _) mapping edgeMappingEdgeId (PageEdgeSpacePoint2D initialMidpoint) subscriptionId -> do
    H.unsubscribe subscriptionId
    state <- H.get
    case state ^? _mappingAtId mapping.id <<< _edgeMappingEdges <<< at edgeMappingEdgeId <<< traversed of
      Nothing -> pure unit
      Just edgeMappingEdge ->
        let
          initialEdgeMappingEdge = edgeMappingEdge {midpointAngle: initialMidpoint.angle, midpointRadius: initialMidpoint.radius}
          op = [ MappingElementOperation mapping.id
                 $ UpdateEdgeMappingEdges [initialEdgeMappingEdge] [edgeMappingEdge]
               ]
          appOp = AppOperation
                  { target: MappingElement mapping.id mapping.sourceGraph mapping.targetGraph
                  , op: []
                  , historyUpdate: Insert op
                  , undoneUpdate: NoOp
                  }
        in
          interpretAndSend appOp

  EdgeDragStart graphId edgeId initialMidpoint mouseEvent -> do
    H.subscribe' \subscriptionId ->
      Drag.dragEventSource mouseEvent
      \e -> EdgeDragMove e graphId edgeId initialMidpoint subscriptionId
    handleAction $ UpdateFocus $ Just $ FocusEdge graphId edgeId

  EdgeDragMove (Drag.Move _ dragData) graphId edgeId initialMidpoint _ -> do
    state <- H.get
    case do
      graph <- state ^? _graphAtId graphId
      edge <- lookupEdgeById edgeId graph
      sourcePos <- graph ^? _nodes <<< at edge.source <<< traversed <<< _position
      targetPos <- graph ^? _nodes <<< at edge.target <<< traversed <<< _position
      pane <- state ^? _paneAtId graphId
      let
        dragOffsetGraphSpace = { x : dragData.offsetX * pane.zoom
                               , y : dragData.offsetY * pane.zoom
                               }
        GraphSpacePoint2D initialMidpointGraphSpace = graphEdgeSpaceToGraphSpace sourcePos targetPos initialMidpoint
        newMidpointGraphSpace =
          GraphSpacePoint2D
            { x : initialMidpointGraphSpace.x + dragOffsetGraphSpace.x
            , y : initialMidpointGraphSpace.y + dragOffsetGraphSpace.y
            }
        GraphEdgeSpacePoint2D newMidpoint = graphSpaceToGraphEdgeSpace sourcePos targetPos newMidpointGraphSpace
        op = [ GraphElementOperation graphId
               $ UpdateEdges [edge] [(edge {midpointAngle = newMidpoint.angle, midpointRadius = newMidpoint.radius})]
             ]
        appOp = AppOperation
                { target: GraphElement graphId
                , op: op
                , historyUpdate: NoOp
                , undoneUpdate: NoOp
                }
      pure $ interpretAndSend appOp
    of
      Nothing -> pure unit
      Just thingsToDo -> thingsToDo

  EdgeDragMove (Drag.Done _) graphId edgeId (GraphEdgeSpacePoint2D initialMidpoint) subscriptionId -> do
    H.unsubscribe subscriptionId
    state <- H.get
    case state ^? _graphAtId graphId <<< _edge edgeId <<< traversed of
      Nothing -> pure unit
      Just edge ->
        let
          op = [ GraphElementOperation graphId
                 $ UpdateEdges [(edge {midpointAngle = initialMidpoint.angle, midpointRadius = initialMidpoint.radius})] [edge]
               ]
          appOp = AppOperation
                  { target: GraphElement graphId
                  , op: []
                  , historyUpdate: Insert op
                  , undoneUpdate: NoOp
                  }
        in
          interpretAndSend appOp

  EdgeDrawStart pane sourceElement mouseEvent -> do
    state <- H.get
    case
      case sourceElement of
        NodeSource nodeId -> do
          node <- state ^? _graphAtId pane.graphId <<< _nodes <<< at nodeId <<< traversed
          pure (nodePosition node)
        EdgeSource edgeId -> do
          graph <- state ^? _graphAtId pane.graphId
          edge <- lookupEdgeById edgeId graph
          sourceNode <- Map.lookup edge.source graph.nodes
          targetNode <- Map.lookup edge.target graph.nodes
          pure $ graphEdgeSpaceToGraphSpace (nodePosition sourceNode) (nodePosition targetNode) (edgeMidpoint edge)
    of
      Nothing -> pure unit
      Just sourcePosition ->
        let
          mousePosition = mouseEventPosition mouseEvent
          sourcePositionPageSpace = graphSpaceToPageSpace pane sourcePosition
          drawingEdge = { source         : sourceElement
                        , sourcePosition : sourcePositionPageSpace
                        , sourceGraph    : pane.graphId
                        , pointPosition  : mousePosition
                        , targetGraph    : pane.graphId
                        }
        in do
          drawingEdgeId <- H.liftEffect UUID.genUUID
          H.modify_ $ _drawingEdges %~ Map.insert drawingEdgeId drawingEdge
          handleAction $ UpdateFocus $ Just case sourceElement of
            NodeSource nodeId -> FocusNode pane.graphId nodeId
            EdgeSource edgeId -> FocusEdge pane.graphId edgeId
          H.subscribe' \subscriptionId ->
            Drag.dragEventSource mouseEvent
            $ \e -> EdgeDrawMove e pane.graphId drawingEdgeId subscriptionId

  -- | Check which pane the point of the drawing edge is in and update
  -- | the drawingEdgeState so it can be drawn properly in both panes
  EdgeDrawMove (Drag.Move _ dragData) graphId drawingEdgeId _ -> do
    state <- H.get
    let
      panes = _.view <$> Array.fromFoldable (Map.values state.megagraph.graphs)
      edgePageTargetPosition = PageSpacePoint2D { x : dragData.x, y : dragData.y }
    case paneContainingPoint panes edgePageTargetPosition <|> state ^? _paneAtId graphId of
      Nothing -> pure unit
      Just targetPane -> do
        H.modify_ $ (_drawingEdgePosition drawingEdgeId .~ edgePageTargetPosition)
                    >>> (_drawingEdgeTargetGraph drawingEdgeId .~ targetPane.graphId)

  -- Create:
  -- - a new edge if the source is a node and the drawn edge is dropped
  --   within the halo of another node of the same graph,
  -- - a new nodeMappingEdge of the source is a node and the drawn edge
  --   is dropped within the halo of a node from a different graph,
  -- - a new edgeMappingEdge if the source is an edge and the drawn edge
  --   is dropped within the halo of an edge from a different graph
  EdgeDrawMove (Drag.Done _) graphId drawingEdgeId subscriptionId -> do
    newEdgeId <- H.liftEffect UUID.genUUID
    let
      createEdgeBetweenNodes sourceNodeId sourceGraphId targetNodeId targetGraphId =
        if sourceGraphId == targetGraphId
        then handleAction $ AppCreateEdge {id: newEdgeId, graphId : sourceGraphId, source: sourceNodeId, target: targetNodeId}
        else handleAction $ AppCreateNodeMappingEdge newEdgeId sourceNodeId sourceGraphId targetNodeId targetGraphId
      createEdgeBetweenEdges sourceEdgeId sourceGraphId targetEdgeId targetGraphId =
        if sourceGraphId == targetGraphId
        then pure unit
        else handleAction $ AppCreateEdgeMappingEdge newEdgeId sourceEdgeId sourceGraphId targetEdgeId targetGraphId
      createEdgeIfSourceTargetAreCompatible drawingEdge hoveredElementId =
        case drawingEdge.source, hoveredElementId of
          NodeSource sourceNodeId, NodeHaloId targetGraphId targetNodeId ->
            createEdgeBetweenNodes sourceNodeId drawingEdge.sourceGraph targetNodeId targetGraphId
          NodeSource sourceNodeId, NodeBorderId targetGraphId targetNodeId ->
            createEdgeBetweenNodes sourceNodeId drawingEdge.sourceGraph targetNodeId targetGraphId
          EdgeSource sourceEdgeId, EdgeHaloId (GraphElement targetGraphId) targetEdgeId ->
            createEdgeBetweenEdges sourceEdgeId drawingEdge.sourceGraph targetEdgeId targetGraphId
          EdgeSource sourceEdgeId, EdgeBorderId (GraphElement targetGraphId) targetEdgeId ->
            createEdgeBetweenEdges sourceEdgeId drawingEdge.sourceGraph targetEdgeId targetGraphId
          _, _ -> pure unit
    state <- H.get
    case Map.lookup drawingEdgeId state.drawingEdges of
      Nothing -> pure unit
      Just drawingEdge ->
        for_ state.hoveredElements \hoveredElementId -> createEdgeIfSourceTargetAreCompatible drawingEdge hoveredElementId
    -- Remove the drawing edge
    H.modify_ $ _{ drawingEdges = Map.delete drawingEdgeId state.drawingEdges }
    H.unsubscribe subscriptionId

  AppCreateNode pane mouseEvent -> do
    newNodeId <- H.liftEffect genUUID
    state <- H.get
    let
      GraphSpacePoint2D newNodePosition = pageSpaceToGraphSpace pane $ mouseEventPosition mouseEvent
      newNode = (freshNode pane.graphId newNodeId) {positionX = newNodePosition.x, positionY = newNodePosition.y}
      op = [GraphElementOperation pane.graphId $ InsertNodes [newNode]]
      appOp = AppOperation { target : GraphElement pane.graphId
                           , op : op
                           , historyUpdate : Insert op
                           , undoneUpdate : NoOp
                           }
    interpretAndSend appOp
    handleAction $ UpdateFocus $ Just $ FocusNode pane.graphId newNodeId

  AppDeleteNode node -> do
    state <- H.get
    case state ^? _graphAtId node.graphId of
      Nothing -> pure unit
      Just graph ->
        let
          allEdges = edgeArray graph
                     # Array.filter (\edge ->
                         edge.source == node.id || edge.target == node.id)
          focus = case Array.head allEdges of
            Just edge ->
              case edge.source == node.id, edge.target == node.id of
                true, false -> Just $ FocusNode edge.graphId edge.target
                false, true -> Just $ FocusNode edge.graphId edge.source
                _, _ -> Nothing
            Nothing -> Nothing
        in do
          handleAction $ UpdateFocus focus
          state' <- H.get
          let
            removeEdgesOp = Array.concatMap (removeEdgeMappingEdgesOp state') allEdges
                         <> Array.concatMap removeEdgeOp allEdges
            op = removeEdgesOp <> removeNodeMappingEdgesOp state node <> removeNodeOp node
            appOp = AppOperation { target : GraphElement node.graphId
                                 , op : op
                                 , historyUpdate : Insert op
                                 , undoneUpdate : NoOp
                                 }
          interpretAndSend appOp

  AppCreateEdge edgeMetadata ->
    if edgeMetadata.source == edgeMetadata.target
    then pure unit
    else
      let
        newEdge = freshEdge edgeMetadata
        appOp = undoableGraphOp edgeMetadata.graphId
                $ InsertEdge newEdge
      in do
        interpretAndSend appOp
        handleAction $ UpdateFocus $ Just $ FocusEdge edgeMetadata.graphId edgeMetadata.id

  AppDeleteEdge edge -> do
    state <- H.get
    let
      op = removeEdgeMappingEdgesOp state edge <> removeEdgeOp edge
      appOp = AppOperation { target : GraphElement edge.graphId
                           , op : op
                           , historyUpdate : Insert op
                           , undoneUpdate : NoOp
                           }
    interpretAndSend appOp
    handleAction $ UpdateFocus $ Just $ FocusNode edge.graphId edge.source

  AppCreateNodeMappingEdge edgeId sourceNodeId sourceGraphId targetNodeId targetGraphId -> do
    state <- H.get
    mappingId <- case lookupMapping sourceGraphId targetGraphId state of
      Nothing -> H.liftEffect UUID.genUUID
      Just mapping -> pure mapping.id
    let
      nodeMappingEdge = { id : edgeId
                        , mappingId : mappingId
                        , sourceNode : sourceNodeId
                        , targetNode : targetNodeId
                        , midpoint : PageEdgeSpacePoint2D {angle: 0.0, radius: 0.0}
                        }
      op = [ MappingElementOperation mappingId $ InsertNodeMappingEdge nodeMappingEdge ]
      appOp = AppOperation { target: MappingElement mappingId sourceGraphId targetGraphId
                           , op : op
                           , historyUpdate : Insert op
                           , undoneUpdate : NoOp
                           }
    interpretAndSend appOp

  AppDeleteNodeMappingEdge mappingId edgeId -> do
    state <- H.get
    H.modify_ $ _mappingAtId mappingId <<< _nodeMappingEdges <<< at edgeId .~ Nothing

  AppCreateEdgeMappingEdge edgeId sourceEdgeId sourceGraphId targetEdgeId targetGraphId -> do
    state <- H.get
    mappingId <- case lookupMapping sourceGraphId targetGraphId state of
      Nothing -> H.liftEffect UUID.genUUID
      Just mapping -> pure $ mapping.id
    let
      edgeMappingEdge = { id : edgeId
                        , mappingId : mappingId
                        , sourceEdge : sourceEdgeId
                        , targetEdge : targetEdgeId
                        , midpoint : PageEdgeSpacePoint2D {angle: 0.0, radius: 0.0}
                        }
      op = [ MappingElementOperation mappingId $ InsertEdgeMappingEdge edgeMappingEdge ]
      appOp = AppOperation { target: MappingElement mappingId sourceGraphId targetGraphId
                           , op : op
                           , historyUpdate : Insert op
                           , undoneUpdate : NoOp
                           }
    interpretAndSend appOp

  AppDeleteEdgeMappingEdge mappingId edgeId -> do
    state <- H.get
    H.modify_ $ _mappingAtId mappingId <<< _edgeMappingEdges <<< at edgeId .~ Nothing

  UpdateFocus newFocus -> do
    H.modify_ _{ focus = newFocus }
    _   <- case newFocus of
      Just (FocusNode _ nodeId)   -> H.query _nodeTextField nodeId $ H.tell SVGContentEditable.Focus
      Just (FocusEdge _ edgeId) -> H.query _edgeTextField edgeId $ H.tell SVGContentEditable.Focus
      _                         -> pure $ Just unit
    state <- H.get
    case newFocus of
      Just (FocusNode graphId _) -> handleAction $ UpdateFocusPane $ GraphElement graphId
      Just (FocusEdge graphId _) -> handleAction $ UpdateFocusPane $ GraphElement graphId
      Just (FocusNodeMappingEdge mappingId _) ->
        case state ^? _mappingAtId mappingId of
          Nothing -> pure unit
          Just mapping -> handleAction $ UpdateFocusPane $ MappingElement mapping.id mapping.sourceGraph mapping.targetGraph
      Just (FocusEdgeMappingEdge mappingId _) ->
        case state ^? _mappingAtId mappingId of
          Nothing -> pure unit
          Just mapping -> handleAction $ UpdateFocusPane $ MappingElement mapping.id mapping.sourceGraph mapping.targetGraph
      _ -> pure unit

  UpdateFocusPane megagraphElement ->
    H.modify_ _{ focusedPane = Just megagraphElement }

  DeleteFocus -> do
    state <- H.get
    let maybeAction = case state.focus of
          Just (FocusNode graphId nodeId) -> do
            node <- state ^? _graphAtId graphId <<< _nodes <<< at nodeId <<< traversed
            pure $ AppDeleteNode node
          Just (FocusEdge graphId edgeId) -> do
            edge <- state ^? _graphAtId graphId >>= lookupEdgeById edgeId
            pure $ AppDeleteEdge edge
          Just (FocusNodeMappingEdge mappingId nodeMappingEdgeId) ->
            pure $ AppDeleteNodeMappingEdge mappingId nodeMappingEdgeId
          Just (FocusEdgeMappingEdge mappingId edgeMappingEdgeId) ->
            pure $ AppDeleteEdgeMappingEdge mappingId edgeMappingEdgeId
          Nothing -> Nothing
    case maybeAction of
      Nothing -> pure unit
      Just action -> handleAction action

  Hover elementId -> do
    H.modify_ $ _hoveredElements %~ Set.insert elementId

  UnHover hoveredElementId -> do
    H.modify_ $ _hoveredElements %~ Set.delete hoveredElementId

  FocusText giveStrForMegagraphUpdate ->
    H.modify_ _{ textFocused = Just giveStrForMegagraphUpdate }

  BlurText text -> do
    state <- H.get
    case do -- Maybe
      giveStrForMegagraphUpdate <- state.textFocused
      Tuple op target <- giveStrForMegagraphUpdate text
      pure $ AppOperation { target : target
                          , op : []
                          , historyUpdate : Insert op
                          , undoneUpdate : NoOp
                          }
    of
      Nothing -> pure unit
      Just appOp -> do
        interpretAndSend appOp
    H.modify_ _{ textFocused = Nothing }

  -- | Zoom in/out holding the mouse position invariant
  Zoom graphId wheelEvent -> do
    state <- H.get
    case state ^? _paneAtId graphId of
      Nothing -> pure unit
      Just pane ->
        let
          scaledZoom = (WhE.deltaY wheelEvent) * zoomScaling
          newZoom = (Math.exp scaledZoom) * pane.zoom
          newPane = zoomAtPoint
                      newZoom
                      (mouseEventPosition $ WhE.toMouseEvent wheelEvent)
                      pane
        in
          H.modify_ $ _paneAtId graphId .~ newPane

  CenterGraphOriginAndZoom -> do
    state <- H.get
    case state.focusedPane of
      Just (GraphElement graphId) ->
        H.modify_ $ _paneAtId graphId %~
          ((_origin .~ PageSpacePoint2D { x : 0.0, y : 0.0 })
           >>>
           (_zoom .~ 1.0))
      _ -> pure unit

  Undo (GraphElement graphId) -> do
    state <- H.get
    case Map.lookup graphId state.megagraph.graphs
         >>= (_.history >>> Array.uncons) of
      Nothing -> pure unit
      Just {head, tail} -> do
        undoOp head (GraphElement graphId)
        handleAction $ UpdateContentEditableText graphId

  Undo (MappingElement mappingId sourceGraph targetGraph) -> do
    state <- H.get
    case Map.lookup mappingId state.megagraph.mappings
         >>= (_.history >>> Array.uncons) of
      Nothing -> pure unit
      Just {head, tail} -> undoOp head (MappingElement mappingId sourceGraph targetGraph)

  Redo (GraphElement graphId) -> do
    state <- H.get
    case Map.lookup graphId state.megagraph.graphs
         >>= (_.undone >>> Array.uncons) of
      Nothing -> pure unit
      Just {head, tail} -> do
        redoOp head (GraphElement graphId)
        handleAction $ UpdateContentEditableText graphId

  Redo (MappingElement mappingId sourceGraph targetGraph) -> do
    state <- H.get
    case Map.lookup mappingId state.megagraph.mappings
         >>= (_.undone >>> Array.uncons) of
      Nothing -> pure unit
      Just {head, tail} -> redoOp head (MappingElement mappingId sourceGraph targetGraph)

  RemovePane graphId -> do
    H.modify_ $ _megagraph %~
      (_graphs %~ Map.delete graphId)
      >>>
      (_mappings %~ Map.filter (_.mapping >>> not mappingTouchesGraph))
    H.modify_ arrangePanes
      where
        mappingTouchesGraph mapping =
          mapping.sourceGraph == graphId
          ||
          mapping.targetGraph == graphId

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
    -- state <- H.get
    foreignJSON <- H.liftEffect $ FileReader.result fileReader
    case
      lmap (show <<< map renderForeignError)
      $ runExcept $ (Foreign.readString foreignJSON
                     >>= decodeJSON :: F (Array WireDataRaw))
                     >>= (sequence <<< map decodeWireData)
    of
      Left errors ->
        H.liftEffect $ Console.log $ "Failed to parse JSON: " <> errors
      Right wireDatas -> for_ wireDatas \wireData -> do
        H.liftEffect $ Console.log $ "Loading saved graph encoded with version "
                                     <> wireData.metadata.version
        H.modify_ $ _megagraph %~ interpretAppOperation wireData.op
        H.modify_ arrangePanes
        -- Keep text fields in sync
        let AppOperation appOp = wireData.op
        case appOp.target of
          GraphElement graphId -> handleAction $ UpdateContentEditableText graphId
          _ -> pure unit
    H.unsubscribe subscriptionId

  SaveLocalFile -> do
    state <- H.get
    rawWireDatas <- H.liftEffect $ sequence $ encodeWireData <$> encodeMegagraphStateAsAppOperations state.megagraph
    let
      titles = state ^.. _megagraph <<< _graphs <<< traversed <<< _graph <<< _title <<< _text
      title = joinWith "_" $ Array.fromFoldable titles
    H.liftEffect $ saveJSON (encodeJSON rawWireDatas) $ title <> ".graph.json"

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
    H.modify_ $ _megagraph %~ interpretAppOperation (preprocessAppOperation op state.megagraph)
    for_ (updatedNodes op) (handleAction <<< tupleApply UpdateNodeContentEditableText)
    H.modify_ arrangePanes
    pure $ Just a

undoableGraphOp :: GraphId -> GraphOperation -> AppOperation
undoableGraphOp graphId graphOp =
  let
    op = [GraphElementOperation graphId graphOp]
  in
    AppOperation { target : GraphElement graphId
                 , op : op
                 , historyUpdate : Insert op
                 , undoneUpdate : NoOp
                 }

undoOp :: MegagraphUpdate -> MegagraphElement -> H.HalogenM AppState Action Slots Message Aff Unit
undoOp op target =
  let
    reversedLastOp = invertMegagraphUpdate op
    appOp = AppOperation { target : target
                         , op : reversedLastOp
                         , historyUpdate : Pop
                         , undoneUpdate : Insert op
                         }
  in do
    interpretAndSend appOp

redoOp :: MegagraphUpdate -> MegagraphElement -> H.HalogenM AppState Action Slots Message Aff Unit
redoOp op target =
  let
    appOp = AppOperation { target : target
                         , op : op
                         , historyUpdate : Insert op
                         , undoneUpdate : Pop
                         }
  in do
    interpretAndSend appOp

interpretAndSend :: AppOperation -> H.HalogenM AppState Action Slots Message Aff Unit
interpretAndSend op = do
  state <- H.get
  let
    preprocessedOp = preprocessAppOperation op state.megagraph
    query = renderMutation $ appOperationToQuery preprocessedOp
  H.liftEffect $ Console.log $ query
  H.raise $ SendOperation $ query
  H.modify_ $ _megagraph %~ interpretAppOperation preprocessedOp


------
-- Key Commands

handleKeyup :: KE.KeyboardEvent -> H.HalogenM AppState Action Slots Message Aff Unit
handleKeyup keyboardEvent =
      case KE.key keyboardEvent of
        "Control" -> do
          state <- H.get
          H.modify_ $ _keyHoldState <<< _controlDown .~ false

        " " -> do
          state <- H.get
          H.modify_ $ _keyHoldState <<< _spaceDown .~ false

        _ -> pure unit

handleKeypress :: KE.KeyboardEvent -> H.HalogenM AppState Action Slots Message Aff Unit
handleKeypress keyboardEvent = do
      H.liftEffect $ Console.log $ show $ KE.key keyboardEvent
      case KE.key keyboardEvent of
        "Control" -> do
          H.liftEffect $ WE.preventDefault  $ KE.toEvent keyboardEvent
          state <- H.get
          H.modify_ $ _keyHoldState <<< _controlDown .~ true

        " " -> do
          state <- H.get
          if not state.keyHoldState.controlDown then pure unit else do
            H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
            H.modify_ $ _keyHoldState <<< _spaceDown .~ true

        -- Undo
        "z" -> do
          state <- H.get
          if isJust state.textFocused || not state.keyHoldState.controlDown then pure unit else do
            H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
            case state.focusedPane of
              Nothing -> pure unit
              Just megagraphElement -> handleAction $ Undo megagraphElement

        -- Redo
        "y" -> do
          state <- H.get
          if not state.keyHoldState.controlDown then pure unit else do
            H.liftEffect $ WE.preventDefault  $ KE.toEvent keyboardEvent
            case state.focusedPane of
              Nothing -> pure unit
              Just megagraphElement -> handleAction $ Redo megagraphElement

        "l" -> do
          state <- H.get
          if not state.keyHoldState.controlDown then pure unit else do
            H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
            -- Load saved graph from local JSON file
            -- TODO revert experiment
            -- H.liftEffect loadFile
            case UUID.parseUUID "dcb54f3a-f3a5-46f5-a1b0-b933d0592235" of
              Nothing -> pure unit
              Just graphId ->
                H.raise $ SendOperation $ renderQuery $ graphFetchQuery graphId

        "s" -> do
          state <- H.get
          if state.keyHoldState.controlDown
          then do
            H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
            -- Save current graph to local JSON file
            handleAction $ SaveLocalFile
          else
              -- connect focused node to Subgraph with same title as node text
              -- TODO
              pure unit
              --case focusNode state of
              --  Nothing -> pure unit
              --  Just node ->
              --    let
              --      op = AppOperation node.graphId $ connectSubgraphIfTitleExists node.id node.text
              --    in
              --      H.raise $ SendOperation op

        "o" -> do
          state <- H.get
          if not state.keyHoldState.controlDown then pure unit else do
            H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
            if not state.keyHoldState.spaceDown
            then do
              -- Return to graph origin and reset zoom
              handleAction CenterGraphOriginAndZoom
            else
              -- Open subgraph in new pane
              -- TODO
              pure unit
              --if state.keyHoldState.spaceDown
              --then unit <$ runMaybeT do
              --  subgraphId <- MaybeT $ pure $ focusNodeSubgraph state
              --  lift $ H.raise $ SendOperation $ AppOperation subgraphId $ insertPane subgraphId
              --else
              --  pure unit

        -- Delete node/edge currently in focus
        "Delete" -> do
          state <- H.get
          case state.focusedPane of
            Nothing -> pure unit
            Just graphId -> handleAction DeleteFocus

        -- Unfocus
        "Escape" -> do
          state <- H.get
          for_ (Map.keys state.megagraph.graphs) \graphId -> do
            handleAction $ UpdateFocus Nothing
            handleAction $ UpdateContentEditableText graphId

        -- Close pane
        "c" -> do
          state <- H.get
          if not state.keyHoldState.controlDown || not state.keyHoldState.spaceDown then pure unit else
            unit <$ runMaybeT do
              lift $ H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
              focusedElement <- MaybeT $ pure $ state.focusedPane
              case focusedElement of
                GraphElement graphId -> lift $ handleAction $ RemovePane graphId
                _ -> pure unit

        -- new Graph
        -- TODO: if current selection is non-empty, push it down into the new subgraph
        "g" -> do
          state <- H.get
          if not state.keyHoldState.controlDown || not state.keyHoldState.spaceDown then pure unit else
            -- TODO
            pure unit
            --case Tuple (focusNode state) (focusNode state >>= _.subgraph) of
            --  Tuple (Just node) Nothing -> do
            --    -- create new graph as a subgraph of focused node
            --    H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
            --    newGraphId <- H.liftEffect genUUID
            --    -- perform these ops separately so that connectSubraph is undoable
            --    H.raise $ SendOperation $ AppOperation node.graphId $ createGraph newGraphId node.text
            --    H.raise $ SendOperation $ AppOperation node.graphId $ connectSubgraph node (Just newGraphId)
            --  _ -> do
            --    -- create new graph in new pane
            --    H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
            --    newGraphId <- H.liftEffect genUUID
            --    let op = AppOperation newGraphId do
            --          insertPane newGraphId
            --          updateTitle newGraphId "" $ UUID.toString newGraphId
            --    interpretAndSend op

        -- TODO: highlighting
        ---- Highlight currently focused node/edge
        --"h" -> H.modify_ $ _graphAtId %~ toggleHighlightFocus

        -- jump Down into the subgraph of the focused node
        "d" -> do
          state <- H.get
          if not state.keyHoldState.controlDown || not state.keyHoldState.spaceDown then pure unit else
            -- TODO
            pure unit
            --case Tuple state.focusedPane (focusNodeSubgraph state) of
            --  Tuple (Just focusedGraphId) (Just subgraphId) ->
            --    let
            --      op = AppOperation focusedGraphId do
            --        removePane focusedGraphId
            --        insertPane subgraphId
            --    in do
            --      H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
            --      interpretAndSend op
            --  _ -> pure unit

        -- jump Up to the graphs that have nodes that point to the current graph
        -- as a subgraph
        "u" -> do
          state <- H.get
          if not state.keyHoldState.controlDown || not state.keyHoldState.spaceDown then pure unit else
            -- TODO
            pure unit
            --H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
            --unit <$ runMaybeT do
            --  focusedGraphId <- MaybeT $ pure state.focusedPane
            --  lift $ interpretAndSend $ AppOperation focusedGraphId $ openGraphsWithSubgraph focusedGraphId

        -- load the Knowledge navigator
        "k" -> do
          state <- H.get
          if not state.keyHoldState.controlDown || not state.keyHoldState.spaceDown then pure unit else
            -- TODO
            pure unit
            --H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
            --H.raise $ SendOperation $ AppOperation config.knowledgeNavigatorId $
            --  insertPane config.knowledgeNavigatorId

        _ -> pure unit
