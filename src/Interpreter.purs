-- | Purescript data structure implementation
module Interpreter where

import Prelude

import AppOperation (AppOperation(..), HistoryUpdate(..))
import AppState (MegagraphElement(..), MegagraphState, _graph, _graphState, _graphs, _mapping, _mappingState, _mappings, _pane, emptyGraphState, emptyMappingState)
import Data.Array as Array
import Data.Foldable (foldl, foldr)
import Data.Lens ((%~), (^?), (.~), traversed)
import Data.Lens.Record (prop)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Data.UUID as UUID
import HasuraQuery (GraphQLMutation, emptyMutation)
import Megagraph (Graph, Mapping, _boundingRect, _height, _text, _title, deleteEdge, deleteEdgeMappingEdge, deleteNode, deleteNodeMappingEdge, deletePathEquation, insertEdge, insertNode, insertPathEquation, setTitleValidity, updateEdge, updateEdgeMappingEdge, updateNode, updateNodeMappingEdge, updateTitle)
import MegagraphOperation (CreateOperation(..), EquationOperation(..), GraphOperation(..), MappingOperation(..), MegagraphOperation(..))
import Query (GraphSchema, renderEdgeDeleteQuery, renderEdgeUpsertQuery, renderGraphUpsertQuery, renderNodeDeleteQuery, renderNodeUpsertQuery)

--import UI.Panes (arrangePanes, insertPaneImpl, rescalePaneImpl)


interpretGraphOperation :: GraphOperation -> Graph -> Graph
interpretGraphOperation = case _ of
  InsertNodes nodes ->        \graph -> foldr insertNode graph nodes
  DeleteNodes nodes ->        \graph -> foldr (deleteNode <<< _.id) graph nodes
  UpdateNodes from to ->      \graph -> foldr updateNode graph to
  InsertEdges edges ->        \graph -> foldr insertEdge graph edges
  DeleteEdges edges ->        \graph -> foldr (deleteEdge <<< _.id) graph edges
  UpdateEdges from to ->      \graph -> foldr updateEdge graph to
  UpdateTitle from to ->      updateTitle to
  SetTitleValidity old new -> setTitleValidity new

interpretEquationOperation :: EquationOperation -> Graph -> Graph
interpretEquationOperation = case _ of
  InsertPathEquations pathEquations -> \mapping -> foldr insertPathEquation mapping pathEquations
  DeletePathEquations pathEquations -> \mapping -> foldr deletePathEquation mapping pathEquations

interpretMappingOperation :: MappingOperation -> Mapping -> Mapping
interpretMappingOperation = case _ of
  InsertNodeMappingEdges nodeMappingEdges ->
    \mapping -> foldr updateNodeMappingEdge mapping nodeMappingEdges
  UpdateNodeMappingEdges from to ->
    \mapping -> foldr updateNodeMappingEdge mapping to
  DeleteNodeMappingEdges nodeMappingEdge ->
    \mapping -> foldr (deleteNodeMappingEdge <<< _.id) mapping nodeMappingEdge
  InsertEdgeMappingEdges edgeMappingEdge ->
    \mapping -> foldr updateEdgeMappingEdge mapping edgeMappingEdge
  UpdateEdgeMappingEdges from to ->
    \mapping -> foldr updateEdgeMappingEdge mapping to
  DeleteEdgeMappingEdges edgeMappingEdge ->
    \mapping -> foldr (deleteEdgeMappingEdge <<< _.id) mapping edgeMappingEdge

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


-- TODO use or remove
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

megagraphOperationToGraphMutation :: MegagraphOperation -> GraphQLMutation GraphSchema
megagraphOperationToGraphMutation =
  case _ of
    GraphElementOperation graphId graphOp ->
      case graphOp of
        InsertNodes nodes      -> renderNodeUpsertQuery nodes
        DeleteNodes nodes      -> renderNodeDeleteQuery $ _.id <$> nodes
        UpdateNodes from to    -> renderNodeUpsertQuery to
        InsertEdges edges      -> renderEdgeUpsertQuery edges
        DeleteEdges edges      -> renderEdgeDeleteQuery $ _.id <$> edges
        UpdateEdges from to    -> renderEdgeUpsertQuery to
        UpdateTitle _ to       -> renderGraphUpsertQuery [{id: graphId, title: to}]
        SetTitleValidity _ new -> emptyMutation
    GraphElementEquationOperation graphId equationOp ->
      case equationOp of -- TODO
        InsertPathEquations pathEquations -> emptyMutation
        DeletePathEquations pathEquations -> emptyMutation
    MappingElementOperation mappingId mappingOp ->
      case mappingOp of -- TODO
        InsertNodeMappingEdges nodeMappingEdges -> emptyMutation
        DeleteNodeMappingEdges nodeMappingEdges -> emptyMutation
        UpdateNodeMappingEdges from to -> emptyMutation
        InsertEdgeMappingEdges edgeMappingEdges -> emptyMutation
        DeleteEdgeMappingEdges edgeMappingEdges -> emptyMutation
        UpdateEdgeMappingEdges from to -> emptyMutation
    CreateElementOperation createOp ->
      case createOp of
        CreateGraph graphId title -> renderGraphUpsertQuery [{id: graphId, title: title}]
        DeleteGraph graphId title -> emptyMutation
        -- TODO
        CreateMapping mappingId from to -> emptyMutation
        DeleteMapping mappingId from to -> emptyMutation

appOperationToQuery :: AppOperation -> GraphQLMutation GraphSchema
appOperationToQuery (AppOperation appOp) =
  foldl (<>) emptyMutation $ megagraphOperationToGraphMutation <$> appOp.op
