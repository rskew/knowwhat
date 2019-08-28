module Workflow.Core
       ( NodeId
       , EdgeId
       , class Graph
       , withDual
       , dualEdge
       , _isDual
       , _nodes
       , _parents
       , _children
       , _nodeId
       , _subgraph
       , _edgeId
       , _source
       , _target
       , lookupEdge
       , insertEdge
       , deleteEdge
       , deleteEdgeId
       , modifyEdge
       , lookupNode
       , lookupChildren
       , lookupParents
       , lookupSiblings
       , lookupCoparents
       , lookupIncomingEdges
       , lookupOutgoingEdges
       , lookupNodeEdges
       , allEdges
       , insertNode
       , deleteNode
       , glue
       , unglue
       , lookupEdgesBetweenGraphs
       ) where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (foldl, foldr, foldMap)
import Data.Lens (Lens', Traversal', traversed, view, (^.), (^?), (.~), (%~), (?~))
import Data.Lens.At (at)
import Data.Map (Map)
import Data.Map as Map
import Data.Set (Set)
import Data.Set as Set
import Data.Maybe (Maybe(..))
import Data.UUID (UUID)
import Data.Symbol (SProxy(..))
import Data.Lens.Record (prop)


------
-- Constants

version :: String
version = "0.0.001"


------
-- Types

type NodeId = UUID

type EdgeId = { source :: NodeId, target :: NodeId }

class (Ord edge, Ord node) <= Graph graph node edge | graph -> node, graph -> edge, node -> graph, edge -> graph where
  _isDual :: Lens' graph Boolean
  _nodes :: Lens' graph (Map NodeId node)
  _parents :: Lens' node (Map NodeId edge)
  _children :: Lens' node (Map NodeId edge)
  _nodeId :: Lens' node NodeId
  _subgraph :: Lens' node graph
  _edgeId :: Lens' edge EdgeId

-- | The dual graph is the graph with the edge directions flipped.
-- | Using the dual representation allows the use of the symmetry of edge flipping
-- | explicitly.
-- |
-- | The lookupEdge, insertEdge and deleteEdge functions respect the
-- | current view (dual or not dual) of the graph. withDual allows running
-- | a graph operation temprarily with the dual representation.
withDual :: forall graph node edge. Graph graph node edge =>
            (graph -> graph) -> graph -> graph
withDual op graph =
  graph
  # _isDual %~ not
  # op
  # _isDual %~ not


------
-- Lenses

_source :: forall graph node edge. Graph graph node edge =>
           Lens' edge NodeId
_source = _edgeId <<< prop (SProxy :: SProxy "source")

_target :: forall graph node edge. Graph graph node edge =>
           Lens' edge NodeId
_target = _edgeId <<< prop (SProxy :: SProxy "target")

_sourceTarget :: forall graph node edge. Graph graph node edge =>
                 NodeId -> NodeId -> Traversal' graph (Maybe edge)
_sourceTarget source target = _nodes <<< at source <<< traversed <<< _children <<< at target
_targetSource :: forall graph node edge. Graph graph node edge =>
                 NodeId -> NodeId -> Traversal' graph (Maybe edge)
_targetSource source target = _nodes <<< at target <<< traversed <<< _parents <<< at source


------
-- Interface

lookupNode :: forall graph node edge. Graph graph node edge =>
              graph -> NodeId -> Maybe node
lookupNode g nodeId = g ^. _nodes <<< at nodeId

insertNode :: forall graph node edge. Graph graph node edge =>
              node -> graph -> graph
insertNode node graph =
  graph
  # ((_nodes <<< (at (node ^. _nodeId))) ?~ node)
  # (\graph' -> (foldr insertEdge graph' (lookupNodeEdges graph' node)))

deleteNode :: forall graph node edge. Graph graph node edge =>
              node -> graph -> graph
deleteNode node graph =
    graph
    # (\graph' -> foldr deleteEdge graph' $ (node ^. _children) <> (node ^. _parents))
    # (_nodes <<< at (node ^. _nodeId)) .~ Nothing

dualEdgeId :: EdgeId -> EdgeId
dualEdgeId edgeId = { source : edgeId.target
                    , target : edgeId.source
                    }

dualEdge :: forall graph node edge. Graph graph node edge =>
            edge -> edge
dualEdge edge =
  edge # _edgeId %~ dualEdgeId

lookupChildren :: forall graph node edge. Graph graph node edge =>
                  graph -> node -> Set node
lookupChildren g n =
  let
    children = case g ^. _isDual of
      false -> n ^. _children
      true -> n ^. _parents
    childIds = Map.keys children
  in
    Set.mapMaybe (lookupNode g) $ childIds

lookupParents :: forall graph node edge. Graph graph node edge =>
                 graph -> node -> Set node
lookupParents graph node =
  let
    dualG = graph # _isDual %~ not
  in
    lookupChildren dualG node

lookupParentsOfGroup :: forall graph node edge. Graph graph node edge =>
                        graph -> Set node -> Set node
lookupParentsOfGroup graph nodes = foldMap (lookupParents graph) nodes

lookupChildrenOfGroup :: forall graph node edge. Graph graph node edge =>
                         graph -> Set node -> Set node
lookupChildrenOfGroup graph nodes = lookupParentsOfGroup (graph # _isDual %~ not) nodes

lookupSiblings :: forall graph node edge. Graph graph node edge =>
                  graph -> node -> Set node
lookupSiblings graph node = foldMap (lookupChildren graph) $ lookupParents graph node

lookupCoparents :: forall graph node edge. Graph graph node edge =>
             graph -> node -> Set node
lookupCoparents g n =
  let
    dualG = g # _isDual %~ not
  in
    lookupSiblings dualG n

lookupEdge :: forall graph node edge. Graph graph node edge =>
              graph -> EdgeId -> Maybe edge
lookupEdge graph edgeId =
  case graph ^. _isDual of
    false ->              graph ^? _sourceTarget edgeId.source edgeId.target # join
    true -> dualEdge <$> (graph ^? _sourceTarget edgeId.target edgeId.source # join)

lookupOutgoingEdges :: forall graph node edge. Graph graph node edge =>
                       graph -> node -> Set edge
lookupOutgoingEdges graph node =
  case graph ^. _isDual of
    false ->              Set.fromFoldable $ Map.values $ node ^. _children
    true -> dualEdge `Set.map` (Set.fromFoldable $ Map.values $ node ^. _parents)

lookupIncomingEdges :: forall graph node edge. Graph graph node edge =>
                       graph -> node -> Set edge
lookupIncomingEdges graph node =
  case graph ^. _isDual of
    false ->              Set.fromFoldable $ Map.values $ node ^. _parents
    true -> dualEdge `Set.map` (Set.fromFoldable $ Map.values $ node ^. _children)

lookupNodeEdges :: forall graph node edge. Graph graph node edge =>
                   graph -> node -> Set edge
lookupNodeEdges graph node =
  lookupOutgoingEdges graph node <> lookupIncomingEdges graph node

allEdges :: forall graph node edge. Graph graph node edge =>
            graph -> Set edge
allEdges graph =
  foldl (\edges node -> foldr Set.insert edges (lookupNodeEdges graph node)) Set.empty $ graph ^. _nodes

insertEdge :: forall graph node edge. Graph graph node edge =>
              edge -> graph -> graph
insertEdge edge graph =
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

deleteEdge :: forall graph node edge. Graph graph node edge =>
              edge -> graph -> graph
deleteEdge = view _edgeId >>> deleteEdgeId

deleteEdgeId :: forall graph node edge. Graph graph node edge =>
             EdgeId -> graph -> graph
deleteEdgeId edgeId graph =
  if graph ^. _isDual
  then graph
       # _sourceTarget edgeId.target edgeId.source .~ Nothing
       # _targetSource edgeId.target edgeId.source .~ Nothing
  else graph
       # _sourceTarget edgeId.source edgeId.target .~ Nothing
       # _targetSource edgeId.source edgeId.target .~ Nothing

modifyEdge :: forall graph node edge. Graph graph node edge =>
              EdgeId -> (edge -> edge) -> graph -> graph
modifyEdge edgeId f graph =
  let
    edgeId' = if graph ^. _isDual then dualEdgeId edgeId else edgeId
  in
    graph
    # _sourceTarget edgeId'.source edgeId'.target %~ map (dualEdge >>> f >>> dualEdge)
    # _targetSource edgeId'.source edgeId'.target %~ map (dualEdge >>> f >>> dualEdge)

lookupEdgesBetweenGraphs :: forall graph node edge. Graph graph node edge =>
                            graph -> graph -> Set edge
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

-- | Take a child graph that has edges to a parent graph that are not
-- | represented in the parent graph, add the matching edges from parent
-- | graph to child graph and merge the node sets.
-- | This is used to merge a collapsed subgraph back into the main graph,
-- | and will also support splitting and joining graphs for filtering
-- | and composing semantic networks or something :D
glue :: forall graph node edge. Graph graph node edge =>
        graph -> graph -> graph
glue graphA graphB =
  let
    graphBNodes = Map.values $ graphB ^. _nodes
    graphBEdges = allEdges graphB
    edgesBetweenAB = lookupEdgesBetweenGraphs graphA graphB
  in
    graphA
    # (\g -> foldr insertNode g graphBNodes)
    # (\g -> foldr insertEdge g graphBEdges)
    # (\g -> foldr insertEdge g edgesBetweenAB)

-- | Inverse of glue (given the array of nodes that made up
-- | the glued-in child graph)
unglue :: forall graph node edge. Graph graph node edge =>
          Set node -> graph -> { parentGraph :: graph, childGraph :: graph }
unglue subgraphNodes graph =
  let
    parentGraph = foldr deleteNode graph subgraphNodes
    emptyGraph = foldr deleteNode graph $ graph ^. _nodes
    childGraph = foldr insertNode emptyGraph subgraphNodes
  in
    { parentGraph : parentGraph
    , childGraph : childGraph
    }
