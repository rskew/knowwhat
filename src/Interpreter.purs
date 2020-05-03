-- | Purescript data structure implementation
module Interpreter where

import Prelude

import Data.Foldable (foldl, foldr)
import Data.Lens ((%~), (.~), (?~))
import Data.Lens.At (at)
import HasuraQuery (GraphQLMutation, emptyMutation)
import Megagraph (Megagraph, _graph, _graphs, _mapping, _mappings, _text, _title, emptyGraph, emptyMapping, updateEdge, updateEdgeMappingEdge, updateNode, updateNodeMappingEdge, updatePathEquation, updateTitle)
import MegagraphStateUpdate (MegagraphStateUpdate(..))
import Query (MegagraphSchema, renderEdgeMappingEdgeUpsertQuery, renderEdgeUpsertQuery, renderGraphUpsertQuery, renderMappingUpsertQuery, renderNodeMappingEdgeUpsertQuery, renderNodeUpsertQuery, renderPathEquationUpsertQuery)

interpretMegagraphStateUpdate :: MegagraphStateUpdate -> Megagraph -> Megagraph
interpretMegagraphStateUpdate = case _ of
  UpdateNodes _ to -> \megagraph -> foldr (\node -> _graph node.graphId %~ updateNode node) megagraph to
  UpdateEdges _ to -> \megagraph -> foldr (\edge -> _graph edge.graphId %~ updateEdge edge) megagraph to
  UpdateTitle graphId _ to -> _graph graphId %~ updateTitle to
  UpdateNodeMappingEdges from to ->
    \megagraph -> foldr (\nodeMappingEdge -> _mapping nodeMappingEdge.mappingId %~ updateNodeMappingEdge nodeMappingEdge) megagraph to
  UpdateEdgeMappingEdges from to ->
    \megagraph -> foldr (\edgeMappingEdge -> _mapping edgeMappingEdge.mappingId %~ updateEdgeMappingEdge edgeMappingEdge) megagraph to
  UpdatePathEquation from to -> _graph to.graphId %~ updatePathEquation to
  CreateGraph graphId title ->
    let
      newGraph = emptyGraph graphId # _title <<< _text .~ title
    in
      _graphs <<< at graphId ?~ newGraph
  CreateMapping mappingId from to title ->
    let
      newMapping = emptyMapping mappingId from to # _title .~ title
    in
      _mappings <<< at mappingId ?~ newMapping
  MegagraphStateUpdateNoOp -> identity


------
-- Hasura graphQL query translation

megagraphOperationToGraphMutation :: MegagraphStateUpdate -> GraphQLMutation MegagraphSchema
megagraphOperationToGraphMutation =
  case _ of
    UpdateNodes _ to    -> renderNodeUpsertQuery to
    UpdateEdges _ to    -> renderEdgeUpsertQuery to
    UpdateTitle graphId _ to -> renderGraphUpsertQuery [{id: graphId, title: to}]
    UpdatePathEquation from to -> renderPathEquationUpsertQuery to
    UpdateNodeMappingEdges _ to -> renderNodeMappingEdgeUpsertQuery to
    UpdateEdgeMappingEdges _ to -> renderEdgeMappingEdgeUpsertQuery to
    CreateGraph graphId title -> renderGraphUpsertQuery [{id: graphId, title: title}]
    CreateMapping mappingId from to title ->
      renderMappingUpsertQuery [{id: mappingId, sourceGraph: from, targetGraph: to, title: title}]
    MegagraphStateUpdateNoOp -> emptyMutation

megagraphUpdateToQuery :: Array MegagraphStateUpdate -> GraphQLMutation MegagraphSchema
megagraphUpdateToQuery op =
  foldl (<>) emptyMutation $ megagraphOperationToGraphMutation <$> op
