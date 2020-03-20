-- | Purescript data structure implementation
module Interpreter where

import Prelude

import AppState (AppState, _megagraph)
import Data.Foldable (foldl, foldr)
import Data.Lens ((%~), (.~), (?~))
import Data.Lens.At (at)
import HasuraQuery (GraphQLMutation, emptyMutation)
import Megagraph (Graph, Mapping, _graph, _graphs, _mapping, _mappings, _panes, _text, _title, deletePathEquation, emptyGraph, emptyMapping, freshPane, insertPathEquation, updateEdge, updateEdgeMappingEdge, updateNode, updateNodeMappingEdge, updateTitle)
import MegagraphOperation (CreateOperation(..), EquationOperation(..), GraphOperation(..), MappingOperation(..), MegagraphOperation(..), MegagraphUpdate)
import Query (MegagraphSchema, renderEdgeMappingEdgeUpsertQuery, renderEdgeUpsertQuery, renderGraphUpsertQuery, renderMappingUpsertQuery, renderNodeMappingEdgeUpsertQuery, renderNodeUpsertQuery)

interpretGraphOperation :: GraphOperation -> Graph -> Graph
interpretGraphOperation = case _ of
  UpdateNodes from to ->      \graph -> foldr updateNode graph to
  UpdateEdges from to ->      \graph -> foldr updateEdge graph to
  UpdateTitle from to ->      updateTitle to

interpretEquationOperation :: EquationOperation -> Graph -> Graph
interpretEquationOperation = case _ of
  InsertPathEquations pathEquations -> \mapping -> foldr insertPathEquation mapping pathEquations
  DeletePathEquations pathEquations -> \mapping -> foldr deletePathEquation mapping pathEquations

interpretMappingOperation :: MappingOperation -> Mapping -> Mapping
interpretMappingOperation = case _ of
  UpdateNodeMappingEdges from to ->
    \mapping -> foldr updateNodeMappingEdge mapping to
  UpdateEdgeMappingEdges from to ->
    \mapping -> foldr updateEdgeMappingEdge mapping to

interpretCreateOperation :: CreateOperation -> AppState -> AppState
interpretCreateOperation = case _ of
  CreateGraph graphId title -> \state ->
    let
      height = state.windowBoundingRect.height
      newRect = { height : height, bottom : height, width : 0.0, top : 0.0, left : 0.0, right : 0.0 }
      newGraph = emptyGraph graphId # _title <<< _text .~ title
      newPane = freshPane graphId newRect
    in
      state
      # _megagraph %~
        (_graphs <<< at graphId ?~ newGraph)
        >>>
        (_panes <<< at graphId ?~ newPane)
  CreateMapping mappingId from to title ->
    let
      newMapping = emptyMapping mappingId from to # _title .~ title
    in
      _megagraph <<< _mappings <<< at mappingId ?~ newMapping

interpretMegagraphOperation :: MegagraphOperation -> AppState -> AppState
interpretMegagraphOperation = case _ of
  GraphComponentOperation graphId graphOp ->
    _megagraph <<< _graph graphId %~ interpretGraphOperation graphOp
  GraphComponentEquationOperation graphId equationOp ->
    _megagraph <<< _graph graphId %~ interpretEquationOperation equationOp
  MappingComponentOperation mappingId _ _ mappingOp ->
    _megagraph <<< _mapping mappingId %~ interpretMappingOperation mappingOp
  CreateComponentOperation createOp ->
    interpretCreateOperation createOp
  MegagraphOperationNoOp -> identity

applyMegagraphUpdate :: MegagraphUpdate -> AppState -> AppState
applyMegagraphUpdate op state = foldl (flip interpretMegagraphOperation) state op


------
-- Hasura graphQL query translation

megagraphOperationToGraphMutation :: MegagraphOperation -> GraphQLMutation MegagraphSchema
megagraphOperationToGraphMutation =
  case _ of
    GraphComponentOperation graphId graphOp ->
      case graphOp of
        UpdateNodes from to    -> renderNodeUpsertQuery to
        UpdateEdges from to    -> renderEdgeUpsertQuery to
        UpdateTitle _ to       -> renderGraphUpsertQuery [{id: graphId, title: to}]
    GraphComponentEquationOperation graphId equationOp ->
      case equationOp of
        InsertPathEquations pathEquations -> emptyMutation
        DeletePathEquations pathEquations -> emptyMutation
    MappingComponentOperation mappingId _ _ mappingOp ->
      case mappingOp of
        UpdateNodeMappingEdges from to -> renderNodeMappingEdgeUpsertQuery to
        UpdateEdgeMappingEdges from to -> renderEdgeMappingEdgeUpsertQuery to
    CreateComponentOperation createOp ->
      case createOp of
        CreateGraph graphId title -> renderGraphUpsertQuery [{id: graphId, title: title}]
        CreateMapping mappingId from to title ->
          renderMappingUpsertQuery [{id: mappingId, sourceGraph: from, targetGraph: to, title: title}]
    MegagraphOperationNoOp -> emptyMutation

megagraphUpdateToQuery :: MegagraphUpdate -> GraphQLMutation MegagraphSchema
megagraphUpdateToQuery op =
  foldl (<>) emptyMutation $ megagraphOperationToGraphMutation <$> op
