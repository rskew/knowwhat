module Workflow.Graph where

import Prelude
import Workflow.Core (Graph(..), Node(..), Edge(..), NodeId, EdgeId, Focus(..), _nodes, _edgeId, _isDual, _children, _parents, _sourceTarget, _targetSource, _source, _target)

import Control.Alt ((<|>))
import Data.Foldable (foldl, foldr, foldMap)
import Data.Lens (view, (^.), (^?), (.~), (%~), (?~))
import Data.Lens.At (at)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.UUID (genUUID)
import Effect (Effect)


freshEdge :: EdgeId -> Edge
freshEdge edgeId = Edge
                   { id : edgeId
                   , text : ""
                   , isValid : true
                   }

freshNode :: Effect Node
freshNode = genUUID >>= \id -> pure $
            Node
            { id : id
            , children : Map.empty
            , parents : Map.empty
            , subgraph : Nothing
            , position : { x : 0.0, y : 0.0 }
            , text : ""
            , isValid : true
            }

emptyGraph :: Graph
emptyGraph = Graph
  { nodes : Map.empty
  , isDual : false
  , focus : NoFocus
  , highlighted : Set.empty
  }


------
-- Interface

lookupNode :: Graph -> NodeId -> Maybe Node
lookupNode g nodeId = g ^. _nodes <<< at nodeId

dualEdgeId :: EdgeId -> EdgeId
dualEdgeId edgeId = { source : edgeId.target
                    , target : edgeId.source
                    }

dualEdge :: Edge -> Edge
dualEdge edge =
  edge # _edgeId %~ dualEdgeId

lookupChildren :: Graph -> Node -> Set Node
lookupChildren g n =
  let
    children = case g ^. _isDual of
      false -> n ^. _children
      true -> n ^. _parents
    childIds = Map.keys children
  in
    Set.mapMaybe (lookupNode g) $ childIds

lookupParents :: Graph -> Node -> Set Node
lookupParents graph node =
  let
    dualG = graph # _isDual %~ not
  in
    lookupChildren dualG node

lookupParentsOfGroup :: Graph -> Set Node -> Set Node
lookupParentsOfGroup graph nodes = foldMap (lookupParents graph) nodes

lookupChildrenOfGroup :: Graph -> Set Node -> Set Node
lookupChildrenOfGroup graph nodes = lookupParentsOfGroup (graph # _isDual %~ not) nodes

lookupSiblings :: Graph -> Node -> Set Node
lookupSiblings graph node = foldMap (lookupChildren graph) $ lookupParents graph node

lookupCoparents :: Graph -> Node -> Set Node
lookupCoparents g n =
  let
    dualG = g # _isDual %~ not
  in
    lookupSiblings dualG n

lookupEdge :: Graph -> EdgeId -> Maybe Edge
lookupEdge graph edgeId =
  case graph ^. _isDual of
    false ->              graph ^? _sourceTarget edgeId.source edgeId.target # join
    true -> dualEdge <$> (graph ^? _sourceTarget edgeId.target edgeId.source # join)

lookupOutgoingEdges :: Graph -> Node -> Set Edge
lookupOutgoingEdges graph node =
  case graph ^. _isDual of
    false ->              Set.fromFoldable $ Map.values $ node ^. _children
    true -> dualEdge `Set.map` (Set.fromFoldable $ Map.values $ node ^. _parents)

lookupIncomingEdges :: Graph -> Node -> Set Edge
lookupIncomingEdges graph node =
  case graph ^. _isDual of
    false ->              Set.fromFoldable $ Map.values $ node ^. _parents
    true -> dualEdge `Set.map` (Set.fromFoldable $ Map.values $ node ^. _children)

lookupNodeEdges :: Graph -> Node -> Set Edge
lookupNodeEdges graph node =
  lookupOutgoingEdges graph node <> lookupIncomingEdges graph node

allEdges :: Graph -> Set Edge
allEdges graph =
  foldl (\edges node -> foldr Set.insert edges (lookupNodeEdges graph node)) Set.empty $ graph ^. _nodes

insertEdgeImpl :: Edge -> Graph -> Graph
insertEdgeImpl edge graph =
  let
    edge' = if graph ^. _isDual then dualEdge edge else edge
    edgeId = edge' ^. _edgeId
  in
    -- check that source and target both exist in the graph
    case lookupNode graph edgeId.source <|> lookupNode graph edgeId.target of
      Nothing -> graph
      Just _ ->
        graph
        # _sourceTarget edgeId.source edgeId.target ?~ edge'
        # _targetSource edgeId.source edgeId.target ?~ edge'

deleteEdgeImpl :: Edge -> Graph -> Graph
deleteEdgeImpl = view _edgeId >>> deleteEdgeId

deleteEdgeId :: EdgeId -> Graph -> Graph
deleteEdgeId edgeId graph =
  if graph ^. _isDual
  then graph
       # _sourceTarget edgeId.target edgeId.source .~ Nothing
       # _targetSource edgeId.target edgeId.source .~ Nothing
  else graph
       # _sourceTarget edgeId.source edgeId.target .~ Nothing
       # _targetSource edgeId.source edgeId.target .~ Nothing

modifyEdge :: EdgeId -> (Edge -> Edge) -> Graph -> Graph
modifyEdge edgeId f graph =
  let
    edgeId' = if graph ^. _isDual then dualEdgeId edgeId else edgeId
  in
    graph
    # _sourceTarget edgeId'.source edgeId'.target %~ map (dualEdge >>> f >>> dualEdge)
    # _targetSource edgeId'.source edgeId'.target %~ map (dualEdge >>> f >>> dualEdge)

lookupEdgesBetweenGraphs :: Graph -> Graph -> Set Edge
lookupEdgesBetweenGraphs graphA graphB =
  let
    edgePartiallyInGraph graph edge =
      Map.member (edge ^. _source) (graph ^. _nodes)
      ||
      Map.member (edge ^. _target) (graph ^. _nodes)
    aToFromB = Set.filter (edgePartiallyInGraph graphB) $ allEdges graphA
    bToFromA = Set.filter (edgePartiallyInGraph graphA) $ allEdges graphB
  in
    aToFromB <> bToFromA

---- TODO: update to return GraphOp or remove
---- | Take a child graph that has edges to a parent graph that are not
---- | represented in the parent graph, add the matching edges from parent
---- | graph to child graph and merge the node sets.
---- | This is used to merge a collapsed subgraph back into the main graph,
---- | and will also support splitting and joining graphs for filtering
---- | and composing semantic networks or something :D
--glue :: Graph -> Graph -> Graph
--glue graphA graphB =
--  let
--    graphBNodes = Map.values $ graphB ^. _nodes
--    graphBEdges = allEdges graphB
--    edgesBetweenAB = lookupEdgesBetweenGraphs graphA graphB
--  in
--    graphA
--    # (\g -> foldr insertNode g graphBNodes)
--    # (\g -> foldr insertEdge g graphBEdges)
--    # (\g -> foldr insertEdge g edgesBetweenAB)
--
---- | Inverse of glue (given the array of nodes that made up
---- | the glued-in child graph)
--unglue :: Set Node -> Graph -> { parentGraph :: Graph, childGraph :: Graph }
--unglue subgraphNodes graph =
--  let
--    parentGraph = foldr deleteNode graph subgraphNodes
--    emptyGraph' = foldr deleteNode graph $ graph ^. _nodes
--    childGraph = foldr insertNode emptyGraph' subgraphNodes
--  in
--    { parentGraph : parentGraph
--    , childGraph : childGraph
--    }
