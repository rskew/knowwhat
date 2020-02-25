-- | Purescript data structure implementation
module Interpreter where

import Prelude

import AppOperation (AppOperation(..), HistoryUpdate(..))
import AppState (MegagraphElement(..), MegagraphState, _graph, _graphState, _graphs, _mapping, _mappingState, _mappings, _pane, emptyGraphState, emptyMappingState)
import Data.Array as Array
import Data.Foldable (foldl)
import Data.Lens ((%~), (^?), (.~), traversed)
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Data.UUID as UUID
import HasuraQuery (GraphQLMutation, emptyMutation)
import Megagraph (Graph, GraphEdgeSpacePoint2D(..), GraphSpacePoint2D(..), Mapping, _boundingRect, _height, _nodes, _text, _title, connectSubgraph, deleteEdge, deleteEdgeMappingEdge, deleteNode, deleteNodeMappingEdge, deletePathEquation, freshEdge, freshNode, insertNewEdge, insertNewNode, insertPathEquation, moveNode, setTitleValidity, updateEdgeMappingEdgeMidpoint, updateEdgeMidpoint, updateEdgeText, updateNodeMappingEdgeMidpoint, updateNodeText, updateTitle, updateEdge, updateEdgeMappingEdge, updateNode, updateNodeMappingEdge)
import MegagraphOperation (CreateOperation(..), EquationOperation(..), GraphOperation(..), MappingOperation(..), MegagraphOperation(..), MegagraphUpdate)
import Query (GraphSchema, edgeDeleteQuery, edgeUpdateQuery, edgeUpsertQuery, graphUpsertQuery, nodeDeleteQuery, nodeUpdateQuery, nodeUpsertQuery)

--import UI.Panes (arrangePanes, insertPaneImpl, rescalePaneImpl)


interpretGraphOperation :: GraphOperation -> Graph -> Graph
interpretGraphOperation = case _ of
  InsertNode nodeId ->               insertNewNode nodeId
  UpdateNode from to ->              updateNode to
  DeleteNode nodeId ->               deleteNode nodeId
  InsertEdge edgeMetadata ->         insertNewEdge edgeMetadata
  UpdateEdge from to ->              updateEdge to
  DeleteEdge edgeMetadata ->         deleteEdge edgeMetadata.id
  MoveNode nodeId from to ->         moveNode nodeId to
  UpdateNodeText nodeId from to ->   updateNodeText nodeId to
  UpdateEdgeText edgeId from to ->   updateEdgeText edgeId to
  MoveEdgeMidpoint edgeId from to -> updateEdgeMidpoint edgeId to
  UpdateTitle from to ->             updateTitle to
  SetTitleValidity old new ->        setTitleValidity new
  ConnectSubgraph nodeId old new ->  connectSubgraph nodeId new

interpretEquationOperation :: EquationOperation -> Graph -> Graph
interpretEquationOperation = case _ of
  InsertPathEquation pathEquation -> insertPathEquation pathEquation
  DeletePathEquation pathEquation -> deletePathEquation pathEquation

interpretMappingOperation :: MappingOperation -> Mapping -> Mapping
interpretMappingOperation = case _ of
  InsertNodeMappingEdge nodeMappingEdge ->
    updateNodeMappingEdge nodeMappingEdge
  UpdateNodeMappingEdge from to ->
    updateNodeMappingEdge to
  DeleteNodeMappingEdge nodeMappingEdge ->
    deleteNodeMappingEdge nodeMappingEdge.id
  InsertEdgeMappingEdge edgeMappingEdge ->
    updateEdgeMappingEdge edgeMappingEdge
  UpdateEdgeMappingEdge from to ->
    updateEdgeMappingEdge to
  DeleteEdgeMappingEdge edgeMappingEdge ->
    deleteEdgeMappingEdge edgeMappingEdge.id
  MoveNodeMappingEdgeMidpoint id from to ->
    updateNodeMappingEdgeMidpoint id to
  MoveEdgeMappingEdgeMidpoint id from to ->
    updateEdgeMappingEdgeMidpoint id to

interpretCreateOperation :: CreateOperation -> MegagraphState -> MegagraphState
interpretCreateOperation = case _ of
  CreateGraph graphId title -> \megagraph ->
    let
      megagraphHeight = fromMaybe 0.0 (megagraph ^? _graphs <<< traversed <<< _pane <<< _boundingRect <<< _height)
      newRect = { height : megagraphHeight, bottom : megagraphHeight, width : 0.0, top : 0.0, left : 0.0, right : 0.0 }
      newGraphState = emptyGraphState graphId newRect
    in
      megagraph # _graphs %~ Map.insert graphId (newGraphState # _graph <<< _title <<< _text .~ title)
  DeleteGraph graphId title -> identity
  CreateMapping mappingId from to ->
    _mappings %~ Map.insert mappingId (emptyMappingState mappingId from to)
  DeleteMapping mappingId from to -> identity

interpretMegagraphOperation :: MegagraphOperation -> MegagraphState -> MegagraphState
interpretMegagraphOperation = case _ of
  GraphElementOperation graphId graphOp ->
    _graphState graphId <<< traversed <<< _graph %~ interpretGraphOperation graphOp
  GraphElementEquationOperation graphId equationOp ->
    _graphState graphId <<< traversed <<< _graph %~ interpretEquationOperation equationOp
  MappingElementOperation mappingId mappingOp ->
    _mappingState mappingId <<< traversed <<< _mapping %~ interpretMappingOperation mappingOp
  CreateElementOperation createOp ->
    interpretCreateOperation createOp

preprocessAppOperation :: AppOperation -> MegagraphState -> AppOperation
preprocessAppOperation (AppOperation appOp) megagraph =
  let
    megagraphHeight = fromMaybe 0.0 (megagraph ^? _graphs <<< traversed <<< _pane <<< _boundingRect <<< _height)
    newRect = { height : megagraphHeight, bottom : megagraphHeight, width : 0.0, top : 0.0, left : 0.0, right : 0.0 }
    createTargetIfNotExists (GraphElement graphId) = case Map.lookup graphId megagraph.graphs of
      Just _ -> identity
      Nothing -> Array.cons (CreateElementOperation $ CreateGraph graphId (UUID.toString graphId))
    createTargetIfNotExists (MappingElement mappingId from to) = case Map.lookup mappingId megagraph.mappings of
      Just _ -> identity
      Nothing -> Array.cons (CreateElementOperation $ CreateMapping mappingId from to)
  in
    AppOperation (appOp {op = appOp.op # createTargetIfNotExists appOp.target})

interpretAppOperation :: AppOperation -> MegagraphState -> MegagraphState
interpretAppOperation (AppOperation {op, target, historyUpdate, undoneUpdate}) megagraph =
  let
    applyMegagraphUpdate = \op' megagraph' -> foldl (flip interpretMegagraphOperation) megagraph' op'
    historyUpdater = case _ of
      Insert op'' -> Array.cons op''
      Pop -> Array.drop 1
      Replace newHistory -> const newHistory
      NoOp -> identity
    updateComponentHistory target' = case target' of
      GraphElement graphId ->
        (_graphState graphId <<< traversed <<< prop (SProxy :: SProxy "history") %~ historyUpdater historyUpdate)
        >>>
        (_graphState graphId <<< traversed <<< prop (SProxy :: SProxy "undone") %~ historyUpdater undoneUpdate)
      MappingElement mappingId from to ->
        (_mappingState mappingId <<< traversed <<< prop (SProxy :: SProxy "history") %~ historyUpdater historyUpdate)
        >>>
        (_mappingState mappingId <<< traversed <<< prop (SProxy :: SProxy "undone") %~ historyUpdater undoneUpdate)
  in
    megagraph
    # applyMegagraphUpdate op
    # updateComponentHistory target


--removePaneImpl :: GraphId -> AppState -> AppState
--removePaneImpl graphId =
--  removeGraphData graphId
--  >>>
--  arrangePanes
--  -- Focus on the rightmost pane
--  >>> \appState -> case appState.graphData.panes
--                        # Map.values >>> Array.fromFoldable
--                        # Array.sortBy (comparing _.boundingRect.right)
--                        # Array.head of
--        Nothing -> appState
--        Just nextPane -> appState { focusedPane = Just nextPane.graphId }


------
-- Hasura graphQL query translation

-- TODO
--megagraphOperationToQuery :: MegagraphState -> MegagraphOperation -> Maybe (GraphQLMutation GraphSchema)
--megagraphOperationToQuery megagraph =
--  let
--    lookupNode = \nodeId -> megagraph # _graph <<< _nodes <<< at nodeId
--  in case _ of
--    GraphElementOperation graphId graphOp ->
--      case graphOp of
--        InsertNode nodeId                                    -> Just $ nodeUpdateQuery (freshNode graphId nodeId)
--        UpdateNode from to                                   -> Just $ nodeUpdateQuery to
--        DeleteNode nodeId                                    -> Just $ nodeDeleteQuery nodeId
--        InsertEdge edgeMetadata                              -> Just $ edgeUpdateQuery (freshEdge edgeMetadata)
--        UpdateEdge from to                                   -> Just $ edgeUpdateQuery to
--        DeleteEdge edgeMetadata                              -> Just $ edgeDeleteQuery edgeMetadata.id
--        MoveNode nodeId _ (GraphSpacePoint2D to) ->
--          lookupNode >>= (pure <<< nodeUpdateQuery <<< _{positionX = to.x, positionY = to.y})
--        UpdateNodeText nodeId _ to                           -> nodeUpdateQuery {id : nodeId, text: to}
--        UpdateEdgeText edgeId _ to                           -> edgeUpdateQuery {id: edgeId, text: to}
--        MoveEdgeMidpoint edgeId _ (GraphEdgeSpacePoint2D to) -> edgeUpdateQuery {id: edgeId, midpointAngle: to.angle, midpointRadius: to.radius}
--        UpdateTitle _ to                                     -> graphUpdateQuery {id: graphId, title: to}
--        SetTitleValidity _ new                               -> emptyMutation
--        ConnectSubgraph nodeId _ new                         -> nodeUpdateQuery {id: nodeId, subgraph: new}
--    GraphElementEquationOperation graphId equationOp ->
--      case equationOp of -- TODO
--        InsertPathEquation pathEquation -> emptyMutation
--        DeletePathEquation pathEquation -> emptyMutation
--    MappingElementOperation mappingId mappingOp ->
--      case mappingOp of -- TODO
--        InsertNodeMappingEdge nodeMappingEdge -> emptyMutation
--        DeleteNodeMappingEdge nodeMappingEdge -> emptyMutation
--        InsertEdgeMappingEdge edgeMappingEdge -> emptyMutation
--        DeleteEdgeMappingEdge edgeMappingEdge -> emptyMutation
--        MoveNodeMappingEdgeMidpoint id from to -> emptyMutation
--        MoveEdgeMappingEdgeMidpoint id from to -> emptyMutation
--    CreateElementOperation createOp ->
--      case createOp of
--        CreateGraph graphId title -> graphUpdateQuery {id: graphId, title: title}
--        DeleteGraph graphId title -> emptyMutation
--        -- TODO
--        CreateMapping mappingId from to -> emptyMutation
--        DeleteMapping mappingId from to -> emptyMutation

--appOperationToQuery :: AppOperation -> GraphQLMutation GraphSchema
--appOperationToQuery (AppOperation appOp) =
--  foldl (<>) emptyMutation $ megagraphOperationToQuery <$> appOp.op
