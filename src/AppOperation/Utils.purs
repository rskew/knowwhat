module AppOperation.Utils where

import Prelude

import AppState (AppState)
import Data.Array as Array
import Data.Map as Map
import Megagraph (Edge, GraphId, Mapping, Node)
import MegagraphOperation (MegagraphUpdate, encodeEdgeMappingEdgesAsMegagraphUpdate, encodeEdgesAsMegagraphUpdate, encodeNodeMappingEdgesAsMegagraphUpdate, encodeNodesAsMegagraphUpdate, invertMegagraphUpdate)

-- TODO cleanup

--mappingsToFromGraph :: AppState -> GraphId -> Array Mapping
--mappingsToFromGraph state graphId =
--  state.megagraph.mappings
--  # Map.values # Array.fromFoldable
--  <#> _.mapping
--  # Array.filter \mapping -> mapping.sourceGraph == graphId
--                             || mapping.targetGraph == graphId

------
-- Delete helpers

removeEdgesOp :: GraphId -> Array Edge -> MegagraphUpdate
removeEdgesOp graphId = invertMegagraphUpdate <<< encodeEdgesAsMegagraphUpdate graphId

removeNodesOp :: GraphId -> Array Node -> MegagraphUpdate
removeNodesOp graphId = invertMegagraphUpdate <<< encodeNodesAsMegagraphUpdate graphId

--removeNodeMappingEdgesOp :: AppState -> Node -> MegagraphUpdate
--removeNodeMappingEdgesOp state node =
--  let
--    nodeMappingEdgesToFromEdge =
--      Array.concatMap
--        (_.nodeMappingEdges >>> Array.fromFoldable)
--        (mappingsToFromGraph state node.graphId)
--      # Array.filter \nodeMappingEdge -> nodeMappingEdge.sourceNode == node.id
--                                         || nodeMappingEdge.targetNode == node.id
--  in
--   invertMegagraphUpdate
--   $ Array.concatMap
--       encodeNodeMappingEdgeAsMegagraphUpdate
--       nodeMappingEdgesToFromEdge
--
--removeEdgeMappingEdgesOp :: AppState -> Edge -> MegagraphUpdate
--removeEdgeMappingEdgesOp state edge =
--  let
--    edgeMappingEdgesToFromEdge =
--      Array.concatMap
--        (_.edgeMappingEdges >>> Array.fromFoldable)
--        (mappingsToFromGraph state edge.graphId)
--      # Array.filter \edgeMappingEdge -> edgeMappingEdge.sourceEdge == edge.id
--                                         || edgeMappingEdge.targetEdge == edge.id
--  in
--    invertMegagraphUpdate
--    $ Array.concatMap
--        encodeEdgeMappingEdgeAsMegagraphUpdate
--        edgeMappingEdgesToFromEdge
