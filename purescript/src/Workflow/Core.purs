module Workflow.Core
       ( class Graph
       , class Node
       , class Edge
       , emptyGraph
       , NodeId
       , NodeIdSet
       , nodeIdSetFromArray
       , insertNodeId
       , deleteNodeId
       , nodes
       , createNode
       , viewId
       , viewParents
       , viewChildren
       , removeParents
       , removeChildren
       , replaceEdges
       , createEdge
       , edgeId
       , edgeSource
       , edgeTarget
       , insertNode
       , removeNode
       , addEdge
       , removeEdge
       , lookupEdge
       , viewSubgraph
       , replaceSubgraph
       , glue
       , unglue
       , lookupNode
       , lookupNodes
       , resolvedGraphEdges
       , genericEncodeOpts
       , graphToJSON
       , graphFromJSON
       , nodeIdSetMember
       ) where

import Control.Monad.Except.Trans (ExceptT)
import Data.Array (mapMaybe, catMaybes)
import Data.Foldable (foldl, elem)
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Foreign (ForeignError)
import Foreign.Generic (defaultOptions, genericDecodeJSON, genericEncodeJSON)
import Foreign.Generic.Class (class GenericDecode, class GenericEncode)
import Foreign.Generic.Types (SumEncoding)
import Foreign.Object (Object, keys, values)
import Foreign.Object as Object
import Prelude (Unit, unit, ($), (>>>), map, flip, (<>), bind, pure)


type NodeId = String

type NodeIdSet = Object Unit

type EdgeId = String

class Node node where
  createNode :: NodeIdSet -> NodeIdSet -> Effect node
  viewId :: node -> NodeId
  viewParents :: node -> NodeIdSet
  viewChildren :: node -> NodeIdSet

class Edge edge where
  createEdge :: NodeId -> NodeId -> edge
  edgeSource :: edge -> NodeId
  edgeTarget :: edge -> NodeId

class (Node node, Edge edge) <=
      Graph graph node edge | graph -> node, graph -> edge where
  emptyGraph :: graph
  nodes :: graph -> Object node
  lookupNode :: NodeId -> graph -> Maybe node
  insertNode :: node -> graph -> graph
  -- removeNode should also remove edges to/from that node
  removeNode :: NodeId -> graph -> graph
  lookupEdge :: NodeId -> NodeId -> graph -> Maybe edge
  addEdge :: edge -> graph -> graph
  removeEdge :: NodeId -> NodeId -> graph -> graph
  viewSubgraph :: node -> graph
  replaceSubgraph :: graph -> node -> node

insertNodeId :: NodeId -> NodeIdSet -> NodeIdSet
insertNodeId nodeId = Object.insert nodeId unit

deleteNodeId :: NodeId -> NodeIdSet -> NodeIdSet
deleteNodeId = Object.delete

nodeIdSetFromArray :: Array NodeId -> NodeIdSet
nodeIdSetFromArray nodeIdArr = Object.fromFoldable $ map (\nodeId -> (Tuple nodeId unit)) nodeIdArr

emptyNodeIdSet :: NodeIdSet
emptyNodeIdSet = Object.empty

nodeIdSetMember :: NodeId -> NodeIdSet -> Boolean
nodeIdSetMember nodeId set = elem nodeId $ keys set

edgeId :: NodeId -> NodeId -> EdgeId
edgeId source target = source <> "." <> target

graphToJSON :: forall graph node edge rep.
               Generic graph rep =>
               GenericEncode rep =>
               Graph graph node edge =>
               graph -> String
graphToJSON g =
  genericEncodeJSON genericEncodeOpts g

graphFromJSON :: forall graph node edge rep.
                 Generic graph rep =>
                 GenericDecode rep =>
                 Graph graph node edge =>
                 String -> ExceptT (NonEmptyList ForeignError) Identity graph
graphFromJSON graphJSON = genericDecodeJSON genericEncodeOpts graphJSON


edgesToFromNode :: forall graph node edge.
                   Graph graph node edge =>
                   node -> graph -> Array edge
edgesToFromNode node g =
  let
    edgesToChildren = catMaybes $ map (
      \childNodeId -> lookupEdge (viewId node) childNodeId g
      ) $ keys $ viewChildren node
    edgesFromParents = catMaybes $ map (
      \parentNodeId -> lookupEdge parentNodeId (viewId node) g
      ) $ keys $ viewParents node
  in
    edgesToChildren <> edgesFromParents

resolveEdgeNodes :: forall graph node edge.
                    Graph graph node edge =>
                    edge -> graph -> Maybe { source :: node , target :: node }
resolveEdgeNodes edge g = do
  source <- lookupNode (edgeSource edge) g
  target <- lookupNode (edgeTarget edge) g
  pure { source : source, target : target }

graphEdges :: forall graph node edge.
              Graph graph node edge =>
              graph -> Array edge
graphEdges g = catMaybes do
  node <- values $ nodes g
  childId <- keys $ viewChildren node
  pure $ lookupEdge (viewId node) childId g

removeParents :: forall graph node edge.
                 Graph graph node edge =>
                 NodeId -> graph -> graph
removeParents nodeId g =
  case lookupNode nodeId g of
    Nothing -> g
    Just node ->
      foldl (\graph_ parentId ->
              removeEdge parentId nodeId graph_)
        g
        $ keys $ viewParents node

removeChildren :: forall graph node edge.
                  Graph graph node edge =>
                  NodeId -> graph -> graph
removeChildren nodeId g =
  case lookupNode nodeId g of
    Nothing -> g
    Just node ->
      foldl (\graph_ childId ->
              removeEdge nodeId childId graph_)
        g
        $ keys (viewChildren node)

replaceEdges :: forall graph node edge.
                Graph graph node edge =>
                NodeId -> Array edge -> graph -> graph
replaceEdges nodeId edges g =
  let
    noEdges = (removeChildren nodeId)
              >>>
              (removeParents nodeId)
              $ g
  in
    foldl (flip addEdge) noEdges edges

resolvedGraphEdges :: forall graph node edge.
                      Graph graph node edge =>
                      graph -> Array { source :: node, target :: node }
resolvedGraphEdges g =
  mapMaybe (flip resolveEdgeNodes g) $ graphEdges g

lookupNodes :: forall graph node edge.
               Graph graph node edge =>
               NodeIdSet -> graph -> Array node
lookupNodes nodeIds g = mapMaybe (flip lookupNode g) $ keys nodeIds

-- | Take a child graph that has edges to a parent graph that are not
-- | mirrored, add the matching edges from parent graph to child graph
-- | and merge the node sets.
-- | This is used to merge a collapsed subgraph back into the main graph,
-- | and will also support breaking down and joining graphs for filtering
-- | and composing semantic networks or something :D
glue :: forall graph node edge.
        Graph graph node edge =>
        graph -> graph -> graph
glue childGraph parentGraph =
  let
    childNodeArray = values $ nodes childGraph
    childGraphEdges = do
      childNode <- childNodeArray
      edgesToFromNode childNode childGraph
    allNodesGraph = foldl (flip insertNode) parentGraph childNodeArray
    allNodesEdgesGraph =
      foldl (flip addEdge) allNodesGraph childGraphEdges
  in
    allNodesEdgesGraph

-- | Almost-inverse of glue (modulo some type conversions)
-- | TODO: property-based tests for inverse relationship (glue <<< unglue == id)
unglue :: forall graph node edge.
          Graph graph node edge =>
          Array node -> graph -> { parentGraph :: graph, childGraph :: graph }
unglue childNodes g =
  let
    childNodeIds = map viewId childNodes
    childGraphEdges = do
      childNode <- childNodes
      edgesToFromNode childNode g
    parentGraph = foldl (flip removeNode) g childNodeIds
    childGraph' =
      foldl (flip insertNode) emptyGraph childNodes
    childGraph =
      foldl (flip addEdge) childGraph' childGraphEdges
  in
    { parentGraph : parentGraph
    , childGraph : childGraph
    }


------
-- Constants

version :: String
version = "0.001"

genericEncodeOpts ::
  { unwrapSingleConstructors :: Boolean
  , fieldTransform :: String -> String
  , sumEncoding :: SumEncoding
  , unwrapSingleArguments :: Boolean
  }
genericEncodeOpts = defaultOptions { unwrapSingleConstructors = true }
