module Graph where

import Prelude

import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', Traversal', traversed, (.~), (?~), (%~))
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple)
import Data.UUID (UUID)
import Data.UUID as UUID
import Foreign.Class (class Encode, class Decode)
import Foreign.Generic (genericEncode, genericDecode, defaultOptions)
import Record.Builder as Builder
import Web.HTML.HTMLElement as WHE


------
-- Types

type GraphId = UUID

type NodeId = UUID

type EdgeId = UUID

type Point2D = { x :: Number, y :: Number }

-- | A position in graph space, as distinct from a point on the page/window
newtype GraphSpacePoint2D = GraphSpacePoint2D Point2D

derive newtype instance showGraphSpacePoint2D :: Show GraphSpacePoint2D

derive newtype instance eqGraphSpacePoint2D :: Eq GraphSpacePoint2D

derive instance genericGraphSpacePoint2D :: Generic GraphSpacePoint2D _

instance encodeGraphSpacePoint2D :: Encode GraphSpacePoint2D where
  encode x = x # genericEncode defaultOptions

instance decodeGraphSpacePoint2D :: Decode GraphSpacePoint2D where
  decode x = x # genericDecode defaultOptions

-- | PageSpacePos represents a position on the browser window such as
-- | a mouse position, as distinct from a position in graph space.
newtype PageSpacePoint2D = PageSpacePoint2D Point2D

derive newtype instance showPageSpacePoint2D :: Show PageSpacePoint2D

derive newtype instance eqPageSpacePoint2D :: Eq PageSpacePoint2D

derive instance genericPageSpacePoint2D :: Generic PageSpacePoint2D _

instance encodePageSpacePoint2D :: Encode PageSpacePoint2D where
  encode x = x # genericEncode defaultOptions

instance decodePageSpacePoint2D :: Decode PageSpacePoint2D where
  decode x = x # genericDecode defaultOptions

toGraphSpace :: GraphView -> PageSpacePoint2D -> GraphSpacePoint2D
toGraphSpace pane (PageSpacePoint2D pagePosition) =
  let
    -- origin is relative to the pane, not the window
    PageSpacePoint2D origin = pane.origin
  in
    GraphSpacePoint2D
      { x : (pagePosition.x - pane.boundingRect.left - origin.x) * pane.zoom
      , y : (pagePosition.y - pane.boundingRect.top  - origin.y) * pane.zoom
      }

toPageSpace :: GraphView -> GraphSpacePoint2D -> PageSpacePoint2D
toPageSpace pane (GraphSpacePoint2D graphPosition) =
  let
    -- origin is relative to the pane, not the window :/
    PageSpacePoint2D origin = pane.origin
  in
    PageSpacePoint2D $ { x : (graphPosition.x / pane.zoom) + pane.boundingRect.left + origin.x
                       , y : (graphPosition.y / pane.zoom) + pane.boundingRect.top  + origin.y
                       }

type EdgeMetadata
  = { id      :: EdgeId
    , graphId :: GraphId
    , source  :: NodeId
    , target  :: NodeId
    }

-- | Invariants:
-- | edge.source.graphId == edge.graphId
-- | edge.target.graphId == edge.graphId
type Edge
  = { id      :: EdgeId
    , graphId :: GraphId
    , source  :: NodeId
    , target  :: NodeId
    , text    :: String
    , isValid :: Boolean
    }

freshEdge :: EdgeMetadata -> Edge
freshEdge edgeMetadata =
  Builder.build (Builder.merge edgeMetadata)
  $ { text    : ""
    , isValid : true
    }

type Node
  = { id       :: NodeId
    , graphId  :: GraphId
    , subgraph :: Maybe GraphId
    , position :: GraphSpacePoint2D
    , text     :: String
    , isValid  :: Boolean
    }

freshNode :: GraphId -> NodeId -> Node
freshNode graphId id
  = { id       : id
    , graphId  : graphId
    , subgraph : Nothing
    , position : GraphSpacePoint2D { x : 0.0, y : 0.0 }
    , text     : ""
    , isValid  : true
    }

data Focus =
  FocusNode NodeId
  | FocusEdge EdgeId (Array EdgeId)

derive instance eqFocus :: Eq Focus
derive instance ordFocus :: Ord Focus
derive instance genericFocus :: Generic Focus _

instance showFocus :: Show Focus where
  show = genericShow

type PathEquation
  = Tuple (Array EdgeId) (Array EdgeId)

type GraphTitle
  = { text :: String
    , isValid :: Boolean
    }

freshTitle :: GraphTitle
freshTitle = { text : "", isValid : true }

type Graph
  = { id :: GraphId
    , title :: GraphTitle
    , nodes :: Map NodeId Node
    , edges :: { sourceTarget :: Map NodeId (Map NodeId (Map EdgeId Edge))
               , targetSource :: Map NodeId (Map NodeId (Map EdgeId Edge))
               }
    , pathEquations :: Set PathEquation
    }

emptyGraph :: GraphId -> Graph
emptyGraph id
  = { id : id
    , title : { text : UUID.toString id
              , isValid : true
              }
    , nodes : Map.empty
    , edges : { sourceTarget : Map.empty
              , targetSource : Map.empty
              }
    , pathEquations : Set.empty
    }

type NodeMappingEdge
  = { sourceNode :: NodeId
    , targetNode :: NodeId
    }

type EdgeMappingEdge
  = { sourceEdge :: EdgeId
    , targetEdge :: EdgeId
    }

type MappingId = UUID

-- | Invariants:
-- | all ((mapping.nodeMappingEdges <#> _.sourceNode.graphId) == mapping.sourceGraph)
-- | all ((mapping.nodeMappingEdges <#> _.targetNode.graphId) == mapping.targetGraph)
-- | all ((mapping.edgeMappingEdges <#> _.graphId) == mapping.sourceGraph)
-- | all ((mapping.edgeMappingEdges <#> _.graphId) == mapping.targetGraph)
type Mapping
  = { id :: MappingId
    , name :: String
    , sourceGraph :: GraphId
    , targetGraph :: GraphId
    , nodeMappingEdges :: Set NodeMappingEdge
    , edgeMappingEdges :: Set EdgeMappingEdge
    }

emptyMapping :: MappingId -> GraphId -> GraphId -> Mapping
emptyMapping id sourceId targetId
  = { id : id
    , name : ""
    , sourceGraph : sourceId
    , targetGraph : targetId
    , nodeMappingEdges : Set.empty
    , edgeMappingEdges : Set.empty
    }

type GraphView
  = { graphId      :: GraphId
    , origin       :: PageSpacePoint2D
    , zoom         :: Number
    , focus        :: Maybe Focus
    , boundingRect :: WHE.DOMRect
    }

emptyPane :: GraphId -> GraphView
emptyPane graphId
  = { graphId      : graphId
    , origin       : PageSpacePoint2D { x : 0.0, y : 0.0 }
    , zoom         : 1.0
    , focus        : Nothing
    , boundingRect : { width  : 0.0
                     , height : 0.0
                     , left   : 0.0
                     , right  : 0.0
                     , top    : 0.0
                     , bottom : 0.0
                     }
    }


------
-- Lenses

_title :: Lens' Graph GraphTitle
_title = prop (SProxy :: SProxy "title")

_sourceTarget :: Lens' Graph (Map NodeId (Map NodeId (Map EdgeId Edge)))
_sourceTarget = prop (SProxy :: SProxy "edges") <<< prop (SProxy :: SProxy "sourceTarget")

_targetSource :: Lens' Graph (Map NodeId (Map NodeId (Map EdgeId Edge)))
_targetSource = prop (SProxy :: SProxy "edges") <<< prop (SProxy :: SProxy "targetSource")

_position :: NodeId -> Traversal' Graph GraphSpacePoint2D
_position nodeId = prop (SProxy :: SProxy "nodes") <<< at nodeId <<< traversed <<< prop (SProxy :: SProxy "position")

_nodeText :: NodeId -> Traversal' Graph String
_nodeText nodeId = prop (SProxy :: SProxy "nodes") <<< at nodeId <<< traversed <<< prop (SProxy :: SProxy "text")

_nodeSubgraph :: NodeId -> Traversal' Graph (Maybe GraphId)
_nodeSubgraph nodeId = prop (SProxy :: SProxy "nodes") <<< at nodeId <<< traversed <<< prop (SProxy :: SProxy "subgraph")

_pathEquations :: Graph -> Set PathEquation
_pathEquations = prop (SProxy :: SProxy "pathEquations")

-- These go to state
--_panes :: Lens' GraphData (Map GraphId GraphView)
--_panes = prop (SProxy :: SProxy "panes")
--
--_pane :: GraphId -> Traversal' GraphData GraphView
--_pane graphId = _panes <<< at graphId <<< traversed

_zoom :: Lens' GraphView Number
_zoom = prop (SProxy :: SProxy "zoom")

_origin :: Lens' GraphView PageSpacePoint2D
_origin = prop (SProxy :: SProxy "origin")

_boundingRect :: Lens' GraphView WHE.DOMRect
_boundingRect = prop (SProxy :: SProxy "boundingRect")

_nodeMappingEdges :: Lens' Mapping (Set NodeMappingEdge)
_nodeMappingEdges = prop (SProxy :: SProxy "nodeMappingEdges")

_edgeMappingEdges :: Lens' Mapping (Set EdgeMappingEdge)
_edgeMappingEdges = prop (SProxy :: SProxy "edgeMappingEdges")


------
-- Interface

insertNewNode :: GraphId -> NodeId -> Graph -> Graph
insertNewNode graphId nodeId graph =
  graph { nodes =
             Map.insert
             nodeId
             (freshNode graphId nodeId)
             graph.nodes
        }

deleteNode :: NodeId -> Graph -> Graph
deleteNode nodeId graph =
  graph { nodes = Map.delete nodeId graph.nodes }

insertNewEdge :: EdgeMetadata -> Graph -> Graph
insertNewEdge edgeMetadata graph =
  let
    newEdge = freshEdge edgeMetadata
    createSubmapsIfNotExists keyA keyB keyC value map = case Map.lookup keyA map of
      Nothing -> map # at keyA ?~ Map.singleton keyB (Map.singleton keyC value)
      Just subMap -> case Map.lookup keyB subMap of
        Nothing -> map # at keyA <<< traversed <<< at keyB ?~ Map.singleton keyC value
        Just subSubMap -> map # at keyA <<< traversed <<< at keyB <<< traversed <<< at keyC ?~ value
    edgesSourceTarget = createSubmapsIfNotExists
                          newEdge.source
                          newEdge.target
                          newEdge.id
                          newEdge
                          graph.edges.sourceTarget
    edgesTargetSource = createSubmapsIfNotExists
                          newEdge.target
                          newEdge.source
                          newEdge.id
                          newEdge
                          graph.edges.targetSource
  in
    graph { edges = { sourceTarget : edgesSourceTarget, targetSource : edgesTargetSource } }

-- why would I even want this?
--batchInsertEdges :: Graph -> Array Edge -> Graph
--batchInsertEdges = foldr (\edge -> insertEdgeImpl edge.id >>> updateEdgeData (const edge) edge.id)

updateEdgeData :: (Edge -> Edge) -> EdgeMetadata -> Graph -> Graph
updateEdgeData updater edgeMetadata =
  (_sourceTarget <<< at edgeMetadata.source <<< traversed <<< at edgeMetadata.target <<< traversed <<< at edgeMetadata.id <<< traversed %~ updater)
  >>>
  (_targetSource <<< at edgeMetadata.target <<< traversed <<< at edgeMetadata.source <<< traversed <<< at edgeMetadata.id <<< traversed %~ updater)

-- TODO continue from here
deleteEdge :: EdgeMetadata -> Graph -> Graph
deleteEdge edgeMetadata =
  (_sourceTarget <<< at edgeMetadata.source <<< traversed <<< at edgeMetadata.target .~ Nothing)
  >>>
  (_targetSource <<< at edgeMetadata.target <<< traversed <<< at edgeMetadata.source .~ Nothing)

moveNode :: NodeId -> GraphSpacePoint2D -> Graph -> Graph
moveNode nodeId newPos = _position nodeId .~ newPos

updateNodeText :: NodeId -> String -> Graph -> Graph
updateNodeText nodeId newText = _nodeText nodeId .~ newText

updateEdgeText :: EdgeMetadata -> String -> Graph -> Graph
updateEdgeText edgeMetadata newText = updateEdgeData _{ text = newText } edgeMetadata

updateTitle :: String -> Graph -> Graph
updateTitle newTitle =
  _title <<< prop (SProxy :: SProxy "text") .~ newTitle

setTitleValidity :: Boolean -> Graph -> Graph
setTitleValidity newValidity =
  _title <<< prop (SProxy :: SProxy "isValid") .~ newValidity

connectSubgraph :: NodeId -> Maybe GraphId -> Graph -> Graph
connectSubgraph nodeId maybeGraphId =
  _nodeSubgraph nodeId .~ maybeGraphId

insertPathEquation :: PathEquation -> Graph -> Graph
insertPathEquation pathEquation =
  _pathEquations %~ Set.insert pathEquation

deletePathEquation :: PathEquation -> Graph -> Graph
deletePathEquation pathEquation =
  _pathEquations %~ Set.delete pathEquation

insertNodeMappingEdge :: NodeId -> NodeId -> Mapping -> Mapping
insertNodeMappingEdge source target =
  _nodeMappingEdges %~ Set.insert { sourceNode : source, targetNode : target }

deleteNodeMappingEdge :: NodeId -> NodeId -> Mapping -> Mapping
deleteNodeMappingEdge source target =
  _nodeMappingEdges %~ Set.delete { sourceNode : source, targetNode : target }

insertEdgeMappingEdge :: EdgeId -> EdgeId -> Mapping -> Mapping
insertEdgeMappingEdge source target =
  _edgeMappingEdges %~ Set.insert { sourceEdge : source, targetEdge : target }

deleteEdgeMappingEdge :: EdgeId -> EdgeId -> Mapping -> Mapping
deleteEdgeMappingEdge source target =
  _edgeMappingEdges %~ Set.delete { sourceEdge : source, targetEdge : target }


------
-- Utilities

allEdgesTouchingNode :: NodeId -> Graph -> { incoming :: Array (Map EdgeId Edge), outgoing :: Array (Map EdgeId Edge) }
allEdgesTouchingNode nodeId graph =
  let
    outgoingEdges = case Map.lookup nodeId graph.edges.sourceTarget of
      Nothing -> []
      Just targetEdgeMap -> Array.fromFoldable $ Map.values targetEdgeMap
    incomingEdges = case Map.lookup nodeId graph.edges.targetSource of
      Nothing -> []
      Just targetEdgeMap -> Array.fromFoldable $ Map.values targetEdgeMap
  in
    { outgoing : outgoingEdges
    , incoming : incomingEdges
    }

edgeArray :: Graph -> Array Edge
edgeArray graph = Array.fromFoldable do
  targetEdgeMap <- Map.values graph.edges.sourceTarget
  edgeMap <- Map.values targetEdgeMap
  edge <- Map.values edgeMap
  pure edge

-- TODO
-- why do I want this?
--edgeIdMap :: Graph -> Map EdgeId Edge
--edgeIdMap graph =
--  Map.fromFoldable $ (\edge -> Tuple edge.id edge) <$> edgeArray graphData

-- TODO don't need these if graphs keps separately
--allEdgesBetweenGraphs :: GraphData -> Array Edge
--allEdgesBetweenGraphs =
--  edgeArray >>> Array.filter \edge -> edge.id.sourceGraph /= edge.id.targetGraph
--
--separateGraphs :: Graph -> Map GraphId GraphData
--separateGraphs graphSmoosh =
--  graphIds
--  # Array.mapMaybe (\graphId -> Tuple graphId <$> selectGraphData graphId graphSmoosh)
--  # Map.fromFoldable
--  where
--    graphIds = Array.fromFoldable $ Map.keys graphSmoosh.panes
--
--selectGraphData :: GraphId -> GraphData -> Maybe GraphData
--selectGraphData graphId graphData =
--  let
--    singleGraphNodes = Map.filter (\node -> node.graphId == graphId) graphData.nodes
--
--    singleGraphEdges = do
--      nodeId <- Array.fromFoldable $ Map.keys singleGraphNodes
--      let nodeEdges = allEdgesTouchingNode nodeId graphData
--      nodeEdges.incoming <> nodeEdges.outgoing
--
--    newGraphData = batchInsertEdges
--                   (emptyGraphData { nodes = singleGraphNodes })
--                   singleGraphEdges
--  in do
--    pane  <- Map.lookup graphId graphData.panes
--    title <- Map.lookup graphId graphData.titles
--    pure $ newGraphData { panes  = Map.singleton graphId pane
--                        , titles = Map.singleton graphId title
--                        }
