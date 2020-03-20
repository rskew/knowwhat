module GraphComponent.HandleAction where

import Prelude

import AppOperation.Utils (removeEdgesOp, removeNodesOp)
import AppState (Action(..), AppState, CallbackId, EdgeSourceElement(..), HistoryUpdate(..), HoveredElementId(..), Message(..), Query(..), Slots, TextFieldElement(..), _callbacks, _controlDown, _drawingEdgePosition, _drawingEdgeTargetGraph, _drawingEdges, _edgeTextField, _graphHistory, _history, _hoveredElements, _keyHoldState, _mappingHistory, _megagraph, _megagraphHistory, _nodeTextField, _pending, _spaceDown, _titleTextField, _undone, lookupMapping, updateHistory, updateUndone)
import Config as Config
import ContentEditable.SVGComponent as SVGContentEditable
import Control.Alt ((<|>))
import Control.Monad.Except (runExcept)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT, lift)
import Data.Argonaut.Core (Json, stringify)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Lens (traversed, (%~), (.~), (^.), (^..), (^?))
import Data.Lens.At (at)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Set as Set
import Data.String (joinWith, trim)
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Data.UUID (UUID, genUUID)
import Data.UUID as UUID
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Console as Console
import Foreign (F, renderForeignError)
import Foreign as Foreign
import Foreign.Generic (encodeJSON, decodeJSON)
import GraphComponent.Utils (mouseEventPosition, updatedNodes)
import Halogen as H
import Halogen.Component.Utils.Drag as Drag
import Halogen.Query.EventSource as ES
import HasuraQuery (renderMutation, renderQuery)
import Interpreter (applyMegagraphUpdate, megagraphUpdateToQuery)
import Math as Math
import Megagraph (GraphEdgeSpacePoint2D(..), GraphSpacePoint2D(..), MegagraphElement(..), PageEdgeSpacePoint2D(..), PageSpacePoint2D(..), _edge, _edgeMappingEdge, _edgeMappingEdges, _graph, _graphs, _isValid, _mapping, _mappings, _node, _nodeMappingEdge, _nodeMappingEdges, _origin, _pane, _panes, _position, _subgraph, _text, _title, _zoom, edgeArray, edgeMidpoint, freshEdge, freshEdgeMappingEdge, freshNode, freshNodeMappingEdge, graphEdgeSpaceToGraphSpace, graphSpaceToGraphEdgeSpace, graphSpaceToPageSpace, lookupEdgeById, mappingEdgeMidpoint, nodePosition, pageEdgeSpaceToPageSpace, pageSpaceToGraphSpace, pageSpaceToPageEdgeSpace)
import MegagraphOperation (CreateOperation(..), GraphOperation(..), MappingOperation(..), MegagraphComponent(..), MegagraphOperation(..), MegagraphUpdate, createTargetsIfNotExist, encodeGraphAsMegagraphUpdate, encodeMappingAsMegagraphUpdate, encodeMegagraphAsMegagraphUpdate, invertMegagraphUpdate, megagraphOperationTargets)
import Query (graphFetchQuery, graphIdWithTitleQuery, nodesWithSubgraphQuery, parseGraphFetchResponse, parseGraphIdWithTitleResponse, parseGraphUpsertResponse, parseMegagraphUpsertResponse, parseNodesWithSubgraphResponse, renderGraphUpsertQuery)
import UI.Constants (zoomScaling)
import UI.Panes (arrangePanes, paneContainingPoint, rescaleWindow, zoomAtPoint)
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
  DoMany actions ->
    for_ actions handleAction

  ApplyMegagraphUpdate op -> applyReceivedMegagraphUpdate op

  PreventDefault e -> do
    H.liftEffect $ WE.preventDefault e

  StopPropagation e -> do
    H.liftEffect $ WE.stopPropagation e

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

  LoadGraph graphId -> do
    callbackId <- H.liftEffect UUID.genUUID
    let
      callback = \msgJson -> do
        {graph, mappings} <- parseGraphFetchResponse msgJson
        let
          graphOp = encodeGraphAsMegagraphUpdate graph
          mappingUpdates = mappings <#> \mapping ->
            encodeMappingAsMegagraphUpdate mapping
        pure $ DoMany $ (ApplyMegagraphUpdate <$> Array.cons graphOp mappingUpdates)
                        <> [UpdateFocusPane (GraphComponent graph.id)]
    H.modify_ $ registerCallback callbackId callback [graphId]
    state <- H.get
    let
      currentGraphIds = state.megagraph.graphs
                        # Map.keys # Array.fromFoldable
    H.liftEffect $ Console.log $ "Loading graph " <> show graphId
    H.raise $ SendOperation $ renderQuery callbackId $ graphFetchQuery graphId currentGraphIds

  UpdateContentEditableText graphId -> do
    state <- H.get
    case Map.lookup graphId state.megagraph.graphs of
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
    case state.megagraph ^? _graph graphId <<< _node nodeId of
      Nothing -> pure unit
      Just node -> do
        _ <- H.query _nodeTextField node.id $ H.tell $ SVGContentEditable.SetText node.text
        pure unit

  NodeTextInput graphId nodeId text -> do
    state <- H.get
    case state.megagraph ^? _graph graphId <<< _node nodeId of
      Nothing -> pure unit
      Just node ->
        let
          updateNodeTextOp = [GraphComponentOperation graphId $ UpdateNodes [node] [(node {text = text})]]
        in do
          interpretAndSend updateNodeTextOp
          handleAction $ UpdateFocus $ Just $ NodeElement node.graphId node.id
          -- If node has a linked subgraph, keep the subgraph title in sync with the node text.
          case node.subgraph of
            Nothing -> pure unit
            Just subgraphId ->
              let
                updateTitleOp = [GraphComponentOperation subgraphId $ UpdateTitle node.text (trim text)]
              in do
                -- Check uniqueness of title in the database
                callbackId <- H.liftEffect UUID.genUUID
                let
                  callback = \msg ->
                    case parseGraphUpsertResponse msg of
                      Right 1 -> Right $ DoMany [ UpdateNodeValidity graphId nodeId true
                                                , ApplyMegagraphUpdate updateTitleOp
                                                , UpdateContentEditableText subgraphId
                                                ]
                      _ -> Right $ UpdateNodeValidity graphId nodeId false
                H.modify_ $ registerCallback callbackId callback [nodeId]
                H.raise $ SendOperation
                          $ renderMutation callbackId
                            $ renderGraphUpsertQuery [{id: subgraphId, title: trim text}]

  EdgeTextInput graphId edgeId text -> do
    state <- H.get
    case lookupEdgeById edgeId =<< (state.megagraph ^? _graph graphId) of
      Nothing -> pure unit
      Just edge ->
        let
          op = [GraphComponentOperation graphId $ UpdateEdges [edge] [(edge {text = text})]]
        in do
          interpretAndSend op
          handleAction $ UpdateFocus $ Just $ EdgeElement graphId edgeId

  TitleTextInput graphId newTitleText -> do
    state <- H.get
    case state.megagraph ^? _graph graphId of
      Nothing -> pure unit
      Just graph ->
        let
          op = [GraphComponentOperation graphId $ UpdateTitle graph.title.text (trim newTitleText)]
        in do
          -- Check uniqueness of title in the database
          callbackId <- H.liftEffect UUID.genUUID
          let
            callback = \msg ->
              case parseGraphUpsertResponse msg of
                Right 1 -> Right $ UpdateTitleValidity graphId true
                _ -> Right $ UpdateTitleValidity graphId false
          H.modify_ $ registerCallback callbackId callback [graphId]
          H.raise $ SendOperation
                    $ renderMutation callbackId
                    $ renderGraphUpsertQuery [{id: graphId, title: trim newTitleText}]
          H.modify_ $ applyMegagraphUpdate op

  UpdateTitleValidity graphId isValid ->
    H.modify_ $ _megagraph <<< _graph graphId <<< _title <<< _isValid .~ isValid

  UpdateNodeValidity graphId nodeId isValid ->
    H.modify_ $ _megagraph <<< _graph graphId <<< _node nodeId <<< _isValid .~ isValid

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
              handleAction $ UpdateFocusPane $ MappingComponent mapping.id mapping.sourceGraph mapping.targetGraph
      case state.focusedPane of
        Just (GraphComponent sourceGraphId) -> newMappingFocus sourceGraphId graphId
        Just (MappingComponent mappingId sourceGraphId targetGraphId) -> newMappingFocus sourceGraphId graphId
        _ -> handleAction $ UpdateFocusPane $ GraphComponent graphId
    else do
      handleAction $ UpdateFocus Nothing
      handleAction $ UpdateFocusPane $ GraphComponent graphId
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
    H.modify_ $ _megagraph <<< _pane graphId <<< _origin .~ newGraphOrigin

  BackgroundDragMove (Drag.Done _) _ _ subscriptionId ->
    H.unsubscribe subscriptionId

  NodeDragStart graphId nodeId initialNodePos mouseEvent -> do
    H.subscribe' \subscriptionId ->
      Drag.dragEventSource mouseEvent
        \e -> NodeDragMove e graphId nodeId initialNodePos subscriptionId
    handleAction $ UpdateFocus $ Just $ NodeElement graphId nodeId

  NodeDragMove (Drag.Move _ dragData) graphId nodeId (GraphSpacePoint2D initialNodePos) _ -> do
    state <- H.get
    case state.megagraph ^? _pane graphId of
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
        case state.megagraph ^? _graph graphId <<< _node nodeId of
          Nothing -> pure unit
          Just node ->
            let
              op = [GraphComponentOperation graphId
                    $ UpdateNodes [node] [(node {positionX = newNodePos.x, positionY = newNodePos.y})]]
            in do
              interpretAndSend op

  NodeDragMove (Drag.Done _) graphId nodeId (GraphSpacePoint2D initialNodePos) subscriptionId -> do
    H.unsubscribe subscriptionId
    state <- H.get
    case state.megagraph ^? _graph graphId <<< _node nodeId of
      Nothing -> pure unit
      Just node ->
        let
          op = [GraphComponentOperation graphId
                $ UpdateNodes [(node {positionX = initialNodePos.x, positionY = initialNodePos.y})] [node]]
        in
          H.modify_ $ updateHistory (Insert op) (GraphComponent graphId)

  NodeMappingEdgeDragStart mappingId initialNodeMappingEdge mouseEvent -> do
    state <- H.get
    case state.megagraph ^? _mapping mappingId of
      Nothing -> pure unit
      Just mapping ->
        H.subscribe' \subscriptionId -> Drag.dragEventSource mouseEvent \e ->
          NodeMappingEdgeDragMove e mapping initialNodeMappingEdge.id (mappingEdgeMidpoint initialNodeMappingEdge) subscriptionId
    handleAction $ UpdateFocus $ Just $ NodeMappingEdgeElement mappingId initialNodeMappingEdge.id

  NodeMappingEdgeDragMove (Drag.Move _ dragData) mapping nodeMappingEdgeId initialMidpoint _ -> do
    state <- H.get
    case do
      currentNodeMappingEdge <- mapping ^. _nodeMappingEdges <<< at nodeMappingEdgeId
      sourcePos <- state.megagraph ^? _graph mapping.sourceGraph <<< _node currentNodeMappingEdge.sourceNode <<< _position
      sourcePane <- state.megagraph ^? _pane mapping.sourceGraph
      targetPos <- state.megagraph ^? _graph mapping.targetGraph <<< _node currentNodeMappingEdge.targetNode <<< _position
      targetPane <- state.megagraph ^? _pane mapping.targetGraph
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
        op = [ MappingComponentOperation mapping.id mapping.sourceGraph mapping.targetGraph
               $ UpdateNodeMappingEdges [currentNodeMappingEdge] [updatedNodeMappingEdge]
             ]
      pure op
    of
      Nothing -> pure unit
      Just op -> interpretAndSend op

  NodeMappingEdgeDragMove (Drag.Done _) mapping nodeMappingEdgeId (PageEdgeSpacePoint2D initialMidpoint) subscriptionId -> do
    H.unsubscribe subscriptionId
    state <- H.get
    case state.megagraph ^? _mapping mapping.id <<< _nodeMappingEdge nodeMappingEdgeId of
      Nothing -> pure unit
      Just nodeMappingEdge ->
        let
          initialNodeMappingEdge = nodeMappingEdge {midpointAngle = initialMidpoint.angle, midpointRadius = initialMidpoint.radius}
          op = [ MappingComponentOperation mapping.id mapping.sourceGraph mapping.targetGraph
                 $ UpdateNodeMappingEdges [initialNodeMappingEdge] [nodeMappingEdge]
               ]
          target = MappingComponent mapping.id mapping.sourceGraph mapping.targetGraph
        in
          H.modify_ $ updateHistory (Insert op) target

  EdgeMappingEdgeDragStart mappingId initialEdgeMappingEdge mouseEvent -> do
    state <- H.get
    case state.megagraph ^? _mapping mappingId of
      Nothing -> pure unit
      Just mapping ->
        H.subscribe' \subscriptionId -> Drag.dragEventSource mouseEvent \e ->
          EdgeMappingEdgeDragMove e mapping initialEdgeMappingEdge.id (mappingEdgeMidpoint initialEdgeMappingEdge) subscriptionId
    handleAction $ UpdateFocus $ Just $ EdgeMappingEdgeElement mappingId initialEdgeMappingEdge.id

  EdgeMappingEdgeDragMove (Drag.Move _ dragData) mapping edgeMappingEdgeId initialMidpoint _ -> do
    state <- H.get
    case do
      currentEdgeMappingEdge <- mapping ^? _edgeMappingEdge edgeMappingEdgeId
      sourceGraph <- state.megagraph ^? _graph mapping.sourceGraph
      sourceEdge <- lookupEdgeById currentEdgeMappingEdge.sourceEdge sourceGraph
      sourcePane <- state.megagraph ^? _pane mapping.sourceGraph
      sourceEdgeSourcePos <- sourceGraph ^? _node sourceEdge.source <<< _position
      sourceEdgeTargetPos <- sourceGraph ^? _node sourceEdge.target <<< _position
      targetGraph <- state.megagraph ^? _graph mapping.targetGraph
      targetEdge <- lookupEdgeById currentEdgeMappingEdge.targetEdge targetGraph
      targetPane <- state.megagraph ^? _pane mapping.targetGraph
      targetEdgeSourcePos <- targetGraph ^? _node targetEdge.source <<< _position
      targetEdgeTargetPos <- targetGraph ^? _node targetEdge.target <<< _position
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
        newEdgeMappingEdge = currentEdgeMappingEdge {midpointAngle = newMidpoint.angle, midpointRadius = newMidpoint.radius}
        op = [ MappingComponentOperation mapping.id mapping.sourceGraph mapping.targetGraph
               $ UpdateEdgeMappingEdges [currentEdgeMappingEdge] [newEdgeMappingEdge]
             ]
      pure op
    of
      Nothing -> pure unit
      Just op -> interpretAndSend op

  EdgeMappingEdgeDragMove (Drag.Done _) mapping edgeMappingEdgeId (PageEdgeSpacePoint2D initialMidpoint) subscriptionId -> do
    H.unsubscribe subscriptionId
    state <- H.get
    case state.megagraph ^? _mapping mapping.id <<< _edgeMappingEdge edgeMappingEdgeId of
      Nothing -> pure unit
      Just edgeMappingEdge ->
        let
          initialEdgeMappingEdge = edgeMappingEdge {midpointAngle = initialMidpoint.angle, midpointRadius = initialMidpoint.radius}
          op = [ MappingComponentOperation mapping.id mapping.sourceGraph mapping.targetGraph
                 $ UpdateEdgeMappingEdges [initialEdgeMappingEdge] [edgeMappingEdge]
               ]
          target = MappingComponent mapping.id mapping.sourceGraph mapping.targetGraph
        in
          H.modify_ $ updateHistory (Insert op) target

  EdgeDragStart graphId edgeId initialMidpoint mouseEvent -> do
    H.subscribe' \subscriptionId ->
      Drag.dragEventSource mouseEvent
      \e -> EdgeDragMove e graphId edgeId initialMidpoint subscriptionId
    handleAction $ UpdateFocus $ Just $ EdgeElement graphId edgeId

  EdgeDragMove (Drag.Move _ dragData) graphId edgeId initialMidpoint _ -> do
    state <- H.get
    case do
      graph <- state.megagraph ^? _graph graphId
      edge <- lookupEdgeById edgeId graph
      sourcePos <- graph ^? _node edge.source <<< _position
      targetPos <- graph ^? _node edge.target <<< _position
      pane <- state.megagraph ^? _pane graphId
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
        op = [ GraphComponentOperation graphId
               $ UpdateEdges [edge] [(edge {midpointAngle = newMidpoint.angle, midpointRadius = newMidpoint.radius})]
             ]
      pure $ interpretAndSend op
    of
      Nothing -> pure unit
      Just thingsToDo -> thingsToDo

  EdgeDragMove (Drag.Done _) graphId edgeId (GraphEdgeSpacePoint2D initialMidpoint) subscriptionId -> do
    H.unsubscribe subscriptionId
    state <- H.get
    case state.megagraph ^? _graph graphId <<< _edge edgeId of
      Nothing -> pure unit
      Just edge ->
        let
          op = [ GraphComponentOperation graphId
                 $ UpdateEdges [(edge {midpointAngle = initialMidpoint.angle, midpointRadius = initialMidpoint.radius})] [edge]
               ]
          target = GraphComponent graphId
        in
          H.modify_ $ updateHistory (Insert op) target

  EdgeDrawStart pane sourceElement mouseEvent -> do
    state <- H.get
    case
      case sourceElement of
        NodeSource nodeId -> do
          node <- state.megagraph ^? _graph pane.graphId <<< _node nodeId
          pure (nodePosition node)
        EdgeSource edgeId -> do
          graph <- state.megagraph ^? _graph pane.graphId
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
            NodeSource nodeId -> NodeElement pane.graphId nodeId
            EdgeSource edgeId -> EdgeElement pane.graphId edgeId
          H.subscribe' \subscriptionId ->
            Drag.dragEventSource mouseEvent
            $ \e -> EdgeDrawMove e pane.graphId drawingEdgeId subscriptionId

  -- | Check which pane the point of the drawing edge is in and update
  -- | the drawingEdgeState so it can be drawn properly in both panes
  EdgeDrawMove (Drag.Move _ dragData) graphId drawingEdgeId _ -> do
    state <- H.get
    let
      panes = state.megagraph.panes # Map.values # Array.fromFoldable
      edgePageTargetPosition = PageSpacePoint2D { x : dragData.x, y : dragData.y }
    case paneContainingPoint panes edgePageTargetPosition <|> state.megagraph ^? _pane graphId of
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
    state <- H.get
    case Map.lookup drawingEdgeId state.drawingEdges of
      Nothing -> pure unit
      Just drawingEdge ->
        for_ state.hoveredElements \hoveredElementId ->
          case drawingEdge.source, hoveredElementId of
            NodeSource sourceNodeId, NodeHaloId targetGraphId targetNodeId ->
              createEdgeBetweenNodes sourceNodeId drawingEdge.sourceGraph targetNodeId targetGraphId
            NodeSource sourceNodeId, NodeBorderId targetGraphId targetNodeId ->
              createEdgeBetweenNodes sourceNodeId drawingEdge.sourceGraph targetNodeId targetGraphId
            EdgeSource sourceEdgeId, EdgeHaloId (GraphComponent targetGraphId) targetEdgeId ->
              createEdgeBetweenEdges sourceEdgeId drawingEdge.sourceGraph targetEdgeId targetGraphId
            EdgeSource sourceEdgeId, EdgeBorderId (GraphComponent targetGraphId) targetEdgeId ->
              createEdgeBetweenEdges sourceEdgeId drawingEdge.sourceGraph targetEdgeId targetGraphId
            _, _ -> pure unit
    -- Remove the drawing edge
    H.modify_ $ _{ drawingEdges = Map.delete drawingEdgeId state.drawingEdges }
    H.unsubscribe subscriptionId

  AppCreateNode pane mouseEvent -> do
    newNodeId <- H.liftEffect genUUID
    state <- H.get
    let
      GraphSpacePoint2D newNodePosition = pageSpaceToGraphSpace pane $ mouseEventPosition mouseEvent
      newNode = (freshNode pane.graphId newNodeId) {positionX = newNodePosition.x, positionY = newNodePosition.y}
      op = [GraphComponentOperation pane.graphId $ UpdateNodes [newNode {deleted = true}] [newNode]]
      target = GraphComponent pane.graphId
    interpretAndSend op
    H.modify_ $ updateHistory (Insert op) target
    handleAction $ UpdateFocus $ Just $ NodeElement pane.graphId newNodeId

  AppDeleteNode node -> do
    state <- H.get
    case state.megagraph ^? _graph node.graphId of
      Nothing -> pure unit
      Just graph ->
        let
          allEdges = edgeArray graph
                     # Array.filter (\edge ->
                         edge.source == node.id || edge.target == node.id)
          focus = case Array.head allEdges of
            Just edge ->
              case edge.source == node.id, edge.target == node.id of
                true, false -> Just $ NodeElement edge.graphId edge.target
                false, true -> Just $ NodeElement edge.graphId edge.source
                _, _ -> Nothing
            Nothing -> Nothing
        in do
          handleAction $ UpdateFocus focus
          state' <- H.get
          let
            op = removeEdgesOp node.graphId allEdges <> removeNodesOp node.graphId [node]
            target = GraphComponent node.graphId
          interpretAndSend op
          H.modify_ $ updateHistory (Insert op) target
          handleAction $ UnHover $ NodeBorderId node.graphId node.id
          handleAction $ UnHover $ NodeHaloId node.graphId node.id

  AppCreateEdge edgeMetadata ->
    if edgeMetadata.source == edgeMetadata.target
    then pure unit
    else
      let
        newEdge = freshEdge edgeMetadata
        op = [GraphComponentOperation edgeMetadata.graphId $ UpdateEdges [newEdge {deleted = true}] [newEdge]]
        target = GraphComponent edgeMetadata.graphId
      in do
        interpretAndSend op
        H.modify_ $ updateHistory (Insert op) target
        handleAction $ UpdateFocus $ Just $ EdgeElement edgeMetadata.graphId edgeMetadata.id

  AppDeleteEdge edge -> do
    state <- H.get
    let
      op = [GraphComponentOperation edge.graphId $ UpdateEdges [edge] [edge {deleted = true}]]
      target = GraphComponent edge.graphId
    interpretAndSend op
    H.modify_ $ updateHistory (Insert op) target
    handleAction $ UpdateFocus $ Just $ NodeElement edge.graphId edge.source
    handleAction $ UnHover $ EdgeBorderId (GraphComponent edge.graphId) edge.id
    handleAction $ UnHover $ EdgeHaloId (GraphComponent edge.graphId) edge.id

  AppCreateNodeMappingEdge edgeId sourceNodeId sourceGraphId targetNodeId targetGraphId -> do
    state <- H.get
    mappingId <- case lookupMapping sourceGraphId targetGraphId state of
      Nothing -> H.liftEffect UUID.genUUID
      Just mapping -> pure mapping.id
    let
      nodeMappingEdge = freshNodeMappingEdge edgeId mappingId sourceNodeId targetNodeId
      op = [ MappingComponentOperation mappingId sourceGraphId targetGraphId
             $ UpdateNodeMappingEdges [nodeMappingEdge {deleted = true}] [nodeMappingEdge] ]
      target = MappingComponent mappingId sourceGraphId targetGraphId
    interpretAndSend op
    H.modify_ $ updateHistory (Insert op) target

  AppDeleteNodeMappingEdge mappingId edgeId -> do
    state <- H.get
    H.modify_ $ _megagraph <<< _mapping mappingId <<< _nodeMappingEdges <<< at edgeId .~ Nothing
    case state.megagraph ^? _mapping mappingId of
      Nothing -> pure unit
      Just mapping ->
        handleAction $ UnHover $ EdgeBorderId (MappingComponent mapping.id mapping.sourceGraph mapping.targetGraph) edgeId

  AppCreateEdgeMappingEdge edgeId sourceEdgeId sourceGraphId targetEdgeId targetGraphId -> do
    state <- H.get
    mappingId <- case lookupMapping sourceGraphId targetGraphId state of
      Nothing -> H.liftEffect UUID.genUUID
      Just mapping -> pure $ mapping.id
    let
      edgeMappingEdge = freshEdgeMappingEdge edgeId mappingId sourceEdgeId targetEdgeId
      op = [ MappingComponentOperation mappingId sourceGraphId targetGraphId
             $ UpdateEdgeMappingEdges [edgeMappingEdge {deleted = true}] [edgeMappingEdge] ]
      target = MappingComponent mappingId sourceGraphId targetGraphId
    interpretAndSend op
    H.modify_ $ updateHistory (Insert op) target

  AppDeleteEdgeMappingEdge mappingId edgeId -> do
    state <- H.get
    H.modify_ $ _megagraph <<< _mapping mappingId <<< _edgeMappingEdges <<< at edgeId .~ Nothing
    case state.megagraph ^? _mapping mappingId of
      Nothing -> pure unit
      Just mapping ->
        handleAction $ UnHover $ EdgeBorderId (MappingComponent mapping.id mapping.sourceGraph mapping.targetGraph) edgeId

  UpdateNodeSubgraph graphId nodeId maybeSubgraphId -> do
    state <- H.get
    case state.megagraph ^? _graph graphId <<< _node nodeId of
      Nothing -> pure unit
      Just node ->
        let
          op = [GraphComponentOperation graphId $ UpdateNodes [node] [node {subgraph = maybeSubgraphId}]]
          target = GraphComponent graphId
        in do
          interpretAndSend op
          H.modify_ $ updateHistory (Insert op) target

  UpdateFocus newFocus -> do
    H.modify_ _{ focus = newFocus }
    _   <- case newFocus of
      Just (NodeElement _ nodeId)   -> H.query _nodeTextField nodeId $ H.tell SVGContentEditable.Focus
      Just (EdgeElement _ edgeId) -> H.query _edgeTextField edgeId $ H.tell SVGContentEditable.Focus
      _                         -> pure $ Just unit
    state <- H.get
    case newFocus of
      Just (NodeElement graphId _) -> handleAction $ UpdateFocusPane $ GraphComponent graphId
      Just (EdgeElement graphId _) -> handleAction $ UpdateFocusPane $ GraphComponent graphId
      Just (NodeMappingEdgeElement mappingId _) ->
        case state.megagraph ^? _mapping mappingId of
          Nothing -> pure unit
          Just mapping -> handleAction $ UpdateFocusPane $ MappingComponent mapping.id mapping.sourceGraph mapping.targetGraph
      Just (EdgeMappingEdgeElement mappingId _) ->
        case state.megagraph ^? _mapping mappingId of
          Nothing -> pure unit
          Just mapping -> handleAction $ UpdateFocusPane $ MappingComponent mapping.id mapping.sourceGraph mapping.targetGraph
      _ -> pure unit

  UpdateFocusPane megagraphElement ->
    H.modify_ _{ focusedPane = Just megagraphElement }

  DeleteFocus -> do
    state <- H.get
    let maybeAction = case state.focus of
          Just (NodeElement graphId nodeId) -> do
            node <- state.megagraph ^? _graph graphId <<< _node nodeId
            pure $ AppDeleteNode node
          Just (EdgeElement graphId edgeId) -> do
            edge <- state.megagraph ^? _graph graphId >>= lookupEdgeById edgeId
            pure $ AppDeleteEdge edge
          Just (NodeMappingEdgeElement mappingId nodeMappingEdgeId) ->
            pure $ AppDeleteNodeMappingEdge mappingId nodeMappingEdgeId
          Just (EdgeMappingEdgeElement mappingId edgeMappingEdgeId) ->
            pure $ AppDeleteEdgeMappingEdge mappingId edgeMappingEdgeId
          Nothing -> Nothing
    case maybeAction of
      Nothing -> pure unit
      Just action -> handleAction action

  Hover elementId -> do
    H.modify_ $ _hoveredElements %~ Set.insert elementId

  UnHover hoveredElementId -> do
    H.modify_ $ _hoveredElements %~ Set.delete hoveredElementId

  OnFocusText textFieldElement giveStrForMegagraphUpdate ->
    H.modify_ _{ textFocused = Just { textFieldElement: textFieldElement
                                    , historyUpdater: giveStrForMegagraphUpdate
                                    }
               }

  OnBlurText -> do
    state <- H.get
    case do -- Maybe
      {textFieldElement, historyUpdater} <- state.textFocused
      text <- case textFieldElement of
        NodeTextField graphId nodeId -> state.megagraph ^? _graph graphId <<< _node nodeId <<< _text
        EdgeTextField graphId edgeId -> state.megagraph ^? _graph graphId <<< _edge edgeId <<< _text
        TitleTextField graphId -> state.megagraph ^? _graph graphId <<< _title <<< _text
      historyUpdater text
    of
      Nothing -> pure unit
      Just (Tuple op target) -> do
        H.modify_ $ updateHistory (Insert op) target
    H.modify_ _{ textFocused = Nothing }

  BlurFocusedTextField -> do
    state <- H.get
    case state.textFocused <#> _.textFieldElement of
      Nothing -> pure unit
      Just textFieldElement -> case textFieldElement of
        NodeTextField graphId nodeId ->
          void $ H.query _nodeTextField nodeId $ H.tell $ SVGContentEditable.Blur
        EdgeTextField graphId edgeId ->
          void $ H.query _edgeTextField edgeId $ H.tell $ SVGContentEditable.Blur
        TitleTextField graphId ->
          void $ H.query _titleTextField graphId $ H.tell $ SVGContentEditable.Blur

  -- | Zoom in/out holding the mouse position invariant
  Zoom graphId wheelEvent -> do
    state <- H.get
    case state.megagraph ^? _pane graphId of
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
          H.modify_ $ _megagraph <<< _pane graphId .~ newPane

  CenterGraphOriginAndZoom -> do
    state <- H.get
    case state.focusedPane of
      Just (GraphComponent graphId) ->
        H.modify_ $ _megagraph <<< _pane graphId %~
          ((_origin .~ PageSpacePoint2D { x : 0.0, y : 0.0 })
           >>>
           (_zoom .~ 1.0))
      _ -> pure unit

  Undo (GraphComponent graphId) -> do
    state <- H.get
    case state.megagraphHistory.graphHistory ^? at graphId <<< traversed <<< _history >>= Array.uncons of
      Nothing -> pure unit
      Just {head, tail} -> do
        undoOp head (GraphComponent graphId)
        handleAction $ UpdateContentEditableText graphId

  Undo (MappingComponent mappingId sourceGraph targetGraph) -> do
    state <- H.get
    case state.megagraphHistory.mappingHistory ^? at mappingId <<< traversed <<< _history >>= Array.uncons of
      Nothing -> pure unit
      Just {head, tail} -> undoOp head (MappingComponent mappingId sourceGraph targetGraph)

  Redo (GraphComponent graphId) -> do
    state <- H.get
    case state.megagraphHistory.graphHistory ^? at graphId <<< traversed <<< _undone >>= Array.uncons of
      Nothing -> pure unit
      Just {head, tail} -> do
        redoOp head (GraphComponent graphId)
        handleAction $ UpdateContentEditableText graphId

  Redo (MappingComponent mappingId sourceGraph targetGraph) -> do
    state <- H.get
    case state.megagraphHistory.mappingHistory ^? at mappingId <<< traversed <<< _undone >>= Array.uncons of
      Nothing -> pure unit
      Just {head, tail} -> redoOp head (MappingComponent mappingId sourceGraph targetGraph)

  RemovePane graphId -> do
    H.modify_ $ _megagraph %~
      (_graphs %~ Map.delete graphId)
      >>>
      (_panes %~ Map.delete graphId)
      >>>
      (_mappings %~ Map.filter (not <<< mappingTouchesGraph))
    _ <- H.modify $ _megagraphHistory <<< _graphHistory <<< at graphId .~ Nothing
    state <- H.get
    for_ (state.megagraphHistory.mappingHistory # Map.keys) (\mappingId ->
      H.modify_ $ _megagraphHistory <<< _mappingHistory <<< at mappingId .~ Nothing)
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
    foreignJSON <- H.liftEffect $ FileReader.result fileReader
    case
      lmap (show <<< map renderForeignError)
      $ runExcept $ (Foreign.readString foreignJSON
                     >>= decodeJSON :: F WireDataRaw)
                     >>= decodeWireData
    of
      Left errors ->
        H.liftEffect $ Console.log $ "Failed to parse JSON: " <> errors
      Right wireData -> do
        H.liftEffect $ Console.log $ "Loading saved graph encoded with version "
                                     <> wireData.metadata.version
        state <- H.get
        H.modify_ $ applyMegagraphUpdate $ createTargetsIfNotExist state.megagraph wireData.op
        H.modify_ arrangePanes
        -- Keep text fields in sync
        state' <- H.get
        for_ (state'.megagraph.graphs # Map.keys) \graphId ->
          handleAction $ UpdateContentEditableText graphId
    H.unsubscribe subscriptionId

  SaveLocalFile -> do
    state <- H.get
    rawWireDatas <- H.liftEffect $ encodeWireData $ encodeMegagraphAsMegagraphUpdate state.megagraph
    let
      titles = state ^.. _megagraph <<< _graphs <<< traversed <<< _title <<< _text
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

  ReceiveOperation callbackId msgJson a -> Just a <$ do
    state <- H.get
    case Map.lookup callbackId state.callbacks of
      Just callback -> callCallback callbackId callback msgJson
      Nothing -> H.liftEffect $ Console.log
                 $ "Error: message recieved without handler ready. ID: "
                 <> show callbackId <> " msgJson: " <> stringify msgJson

  QLoadGraph graphId a -> Just a <$ do
    handleAction $ LoadGraph graphId

applyReceivedMegagraphUpdate :: MegagraphUpdate -> H.HalogenM AppState Action Slots Message Aff Unit
applyReceivedMegagraphUpdate op = do
  state <- H.get
  H.modify_ $ applyMegagraphUpdate (createTargetsIfNotExist state.megagraph op)
  for_ (updatedNodes op) (handleAction <<< tupleApply UpdateNodeContentEditableText)
  H.modify_ arrangePanes

undoOp :: MegagraphUpdate -> MegagraphComponent -> H.HalogenM AppState Action Slots Message Aff Unit
undoOp op target =
  let
    reversedLastOp = invertMegagraphUpdate op
  in do
    interpretAndSend reversedLastOp
    H.modify_ $ updateHistory Pop target
    H.modify_ $ updateUndone (Insert op) target

redoOp :: MegagraphUpdate -> MegagraphComponent -> H.HalogenM AppState Action Slots Message Aff Unit
redoOp op target = do
  interpretAndSend op
  H.modify_ $ updateHistory (Insert op) target
  H.modify_ $ updateUndone Pop target

-- | Interpret the operation, send the operation to the backend,
-- | and register a callback to track when the backend has
-- | actioned the operation.
interpretAndSend :: MegagraphUpdate -> H.HalogenM AppState Action Slots Message Aff Unit
interpretAndSend op = do
  callbackId <- H.liftEffect UUID.genUUID
  let
    targets = Array.concatMap megagraphOperationTargets op
    callback = \msg -> do
      megagraphUpsertResponse <- parseMegagraphUpsertResponse msg
      pure $ DoMany []
  H.modify_ $ registerCallback callbackId callback targets
  state <- H.get
  let
    preprocessedOp = createTargetsIfNotExist state.megagraph op
    query = renderMutation callbackId $ megagraphUpdateToQuery preprocessedOp
  H.raise $ SendOperation $ query
  H.modify_ $ applyMegagraphUpdate preprocessedOp


------
-- Callback helpers

registerCallback :: UUID -> (Json -> Either String Action) -> Array UUID -> AppState -> AppState
registerCallback callbackId callback elementIds =
  (_callbacks %~ Map.insert callbackId callback)
  >>>
  (_pending %~ Map.union (Map.fromFoldable (elementIds <#> \elementId -> Tuple elementId callbackId)))

callCallback :: CallbackId -> (Json -> Either String Action) -> Json -> H.HalogenM AppState Action Slots Message Aff Unit
callCallback callbackId callback msgJson = do
  case callback msgJson of
    Left err -> do
      H.liftEffect $ Console.log "callback failed:"
      H.liftEffect $ Console.log err
      H.liftEffect $ Console.log $ stringify msgJson
    Right action -> do
      handleAction action
      -- cleanup app state
      H.modify_ $ _callbacks <<< at callbackId .~ Nothing
      H.modify_ $ _pending %~ Map.filter ((/=) callbackId)
      -- TODO
      H.modify_ $ _pending .~ Map.empty
  state <- H.get
  H.liftEffect $ Console.log "pending:"
  H.liftEffect $ Console.log $ show state.pending


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
      H.liftEffect $ Console.log $ "Keypress: " <> show (KE.key keyboardEvent)
      case KE.key keyboardEvent of
        "Control" -> do
          H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
          H.modify_ $ _keyHoldState <<< _controlDown .~ true
          state <- H.get
          if state.keyHoldState.spaceDown
          then handleAction BlurFocusedTextField
          else pure unit

        " " -> do
          H.modify_ $ _keyHoldState <<< _spaceDown .~ true
          state <- H.get
          if state.keyHoldState.controlDown
          then handleAction BlurFocusedTextField
          else pure unit

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
            H.liftEffect loadFile

        "s" -> do
          state <- H.get
          if not state.keyHoldState.controlDown
          then pure unit
          else
             if not state.keyHoldState.spaceDown
             -- Save current graph to local JSON file
             then do
               H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
               H.liftEffect $ Console.log "Saving current graphs to file"
               handleAction $ SaveLocalFile
             -- Connect focused node to Subgraph with same title as node text.
             -- Send a qeury to get the graphId for the graph whose title matches
             -- the focused node's text, and register a callback to handle the
             -- response by updating the nodes subgraph field.
             else do
               H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
               case state.focus of
                 Just (NodeElement graphId nodeId) -> do
                   case state.megagraph ^? _graph graphId <<< _node nodeId <<< _text of
                     Nothing -> pure unit
                     Just nodeText -> do
                       callbackId <- H.liftEffect UUID.genUUID
                       let
                         callback = \msg ->
                           case parseGraphIdWithTitleResponse msg of
                             Left err -> Left err
                             Right maybeGraphRow -> Right $ UpdateNodeSubgraph graphId nodeId (maybeGraphRow <#> _.id)
                       H.modify_ $ registerCallback callbackId callback [nodeId]
                       H.raise $ SendOperation $ renderQuery callbackId $ graphIdWithTitleQuery $ trim nodeText
                 _ -> pure unit

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
              case state.focus of
                Just (NodeElement graphId nodeId) ->
                  case state.megagraph ^? _graph graphId <<< _node nodeId <<< _subgraph <<< traversed of
                    Nothing -> pure unit
                    Just subgraphId -> handleAction $ LoadGraph subgraphId
                _ -> pure unit

        -- Delete node/edge currently in focus
        "Delete" -> do
          state <- H.get
          case state.focusedPane of
            Nothing -> pure unit
            Just graphId -> handleAction DeleteFocus

        -- Unfocus
        -- Cancel pending callbacks
        "Escape" -> do
          state <- H.get
          for_ (Map.keys state.megagraph.graphs) (\graphId -> do
            handleAction $ UpdateFocus Nothing
            handleAction $ UpdateContentEditableText graphId)
          void $ H.modify $ _callbacks .~ Map.empty
          void $ H.modify $ _pending .~ Map.empty
          pure unit

        -- Close pane
        "c" -> do
          state <- H.get
          if not state.keyHoldState.controlDown || not state.keyHoldState.spaceDown then pure unit else
            unit <$ runMaybeT do
              lift $ H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
              focusedElement <- MaybeT $ pure $ state.focusedPane
              case focusedElement of
                GraphComponent graphId -> lift $ handleAction $ RemovePane graphId
                _ -> pure unit

        -- new Graph
        "g" -> do
          state <- H.get
          if not state.keyHoldState.controlDown || not state.keyHoldState.spaceDown then pure unit else do
            H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
            newGraphId <- H.liftEffect genUUID
            let op = [CreateComponentOperation $ CreateGraph newGraphId (UUID.toString newGraphId)]
            interpretAndSend op
            H.modify_ arrangePanes

        -- jump Down into the subgraph of the focused node
        "d" -> do
          state <- H.get
          if not state.keyHoldState.controlDown || not state.keyHoldState.spaceDown then pure unit else do
            H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
            case state.focus of
              Just (NodeElement graphId nodeId) ->
                case state.megagraph ^? _graph graphId <<< _node nodeId <<< _subgraph <<< traversed of
                  Nothing -> pure unit
                  Just subgraphId -> do
                    handleAction $ RemovePane graphId
                    handleAction $ LoadGraph subgraphId
              _ -> pure unit

        -- Jump Up to the graphs that have nodes that point to the current graph
        -- as a subgraph.
        "u" -> do
          state <- H.get
          if not state.keyHoldState.controlDown || not state.keyHoldState.spaceDown then pure unit else do
            H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
            case state.focusedPane of
              Just (GraphComponent graphId) -> do
                callbackId <- H.liftEffect UUID.genUUID
                let
                  callback = \msg -> do
                    nodes <- parseNodesWithSubgraphResponse msg
                    pure $ DoMany $ [RemovePane graphId] <> (LoadGraph <$> (nodes <#> _.graphId))
                H.modify_ $ registerCallback callbackId callback [graphId]
                H.raise $ SendOperation $ renderQuery callbackId $ nodesWithSubgraphQuery graphId
              _ -> pure unit

        -- load the Knowledge navigator
        "k" -> do
          state <- H.get
          if not state.keyHoldState.controlDown || not state.keyHoldState.spaceDown then pure unit else do
            H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
            case UUID.parseUUID Config.homeGraphId of
              Nothing -> pure unit
              Just homeGraphId -> handleAction $ LoadGraph homeGraphId

        _ -> pure unit
