-- | A Megagraph is a graph where each node is linked to another graph, and each
-- | edge is linked to a mapping between graphs.
-- | Graphs can also have path-equations, allowing them to represent ologs, with
-- | mappings representing functors between ologs.
module Megagraph where

import Prelude

import Data.Array as Array
import Data.Foldable (foldr)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', Traversal', lens, traversed, (%~), (.~), (?~))
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Symbol (SProxy(..))
import Data.UUID (UUID)
import Data.UUID as UUID
import Foreign.Class (class Encode, class Decode)
import Foreign.Generic (genericEncode, genericDecode, defaultOptions)
import Math as Math
import Record.Builder as Builder
import Web.HTML.HTMLElement as WHE


------
-- Types

type GraphId = UUID

type NodeId = UUID

type EdgeId = UUID

type Point2D = { x :: Number, y :: Number }

type Point2DPolar = { angle :: Number, radius :: Number }

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

-- | Edge space is the coordinate system local to a particular edge,
-- | with the midpoint of the source and target as zero and the
-- | source and target aligned with the x-axis, with the source
-- | to the left of zero.
-- | This makes is simple to represent the curve of the edge
-- | in a way that's invariant to rotation, and preserves the deviation from
-- | the midpoint when elongated.
newtype GraphEdgeSpacePoint2D = GraphEdgeSpacePoint2D { angle :: Number, radius :: Number }

derive newtype instance showGraphEdgeSpacePoint2D :: Show GraphEdgeSpacePoint2D

derive newtype instance eqGraphEdgeSpacePoint2D :: Eq GraphEdgeSpacePoint2D

derive newtype instance ordGraphEdgeSpacePoint2D :: Ord GraphEdgeSpacePoint2D

derive instance genericGraphEdgeSpacePoint2D :: Generic GraphEdgeSpacePoint2D _

instance encodeGraphEdgeSpacePoint2D :: Encode GraphEdgeSpacePoint2D where
  encode x = x # genericEncode defaultOptions

instance decodeGraphEdgeSpacePoint2D :: Decode GraphEdgeSpacePoint2D where
  decode x = x # genericDecode defaultOptions

newtype PageEdgeSpacePoint2D = PageEdgeSpacePoint2D { angle :: Number, radius :: Number }

derive newtype instance showPageEdgeSpacePoint2D :: Show PageEdgeSpacePoint2D

derive newtype instance eqPageEdgeSpacePoint2D :: Eq PageEdgeSpacePoint2D

derive newtype instance ordPageEdgeSpacePoint2D :: Ord PageEdgeSpacePoint2D

derive instance genericPageEdgeSpacePoint2D :: Generic PageEdgeSpacePoint2D _

instance encodePageEdgeSpacePoint2D :: Encode PageEdgeSpacePoint2D where
  encode x = x # genericEncode defaultOptions

instance decodePageEdgeSpacePoint2D :: Decode PageEdgeSpacePoint2D where
  decode x = x # genericDecode defaultOptions

pageSpaceToGraphSpace :: GraphView -> PageSpacePoint2D -> GraphSpacePoint2D
pageSpaceToGraphSpace pane (PageSpacePoint2D pagePosition) =
  let
    -- origin is relative to the pane, not the window
    PageSpacePoint2D origin = pane.origin
  in
    GraphSpacePoint2D
      { x : (pagePosition.x - pane.boundingRect.left - origin.x) * pane.zoom
      , y : (pagePosition.y - pane.boundingRect.top  - origin.y) * pane.zoom
      }

graphSpaceToPageSpace :: GraphView -> GraphSpacePoint2D -> PageSpacePoint2D
graphSpaceToPageSpace pane (GraphSpacePoint2D graphPosition) =
  let
    -- origin is relative to the pane, not the window :/
    PageSpacePoint2D origin = pane.origin
  in
    PageSpacePoint2D $ { x : (graphPosition.x / pane.zoom) + pane.boundingRect.left + origin.x
                       , y : (graphPosition.y / pane.zoom) + pane.boundingRect.top  + origin.y
                       }

graphEdgeSpaceToGraphSpace :: GraphSpacePoint2D -> GraphSpacePoint2D -> GraphEdgeSpacePoint2D -> GraphSpacePoint2D
graphEdgeSpaceToGraphSpace (GraphSpacePoint2D sourcePos) (GraphSpacePoint2D targetPos) (GraphEdgeSpacePoint2D edgeSpacePoint) =
  GraphSpacePoint2D $ edgePolarToSourceTargetCartesian sourcePos targetPos edgeSpacePoint

edgePolarToSourceTargetCartesian :: Point2D -> Point2D -> Point2DPolar -> Point2D
edgePolarToSourceTargetCartesian sourcePos targetPos polarPoint =
  let
    sourceTargetVector = targetPos - sourcePos
    midpoint = { x : (targetPos.x + sourcePos.x) / 2.0
               , y : (targetPos.y + sourcePos.y) / 2.0
               }
    angle = Math.atan2 sourceTargetVector.y sourceTargetVector.x
  in
    { x : Math.cos (polarPoint.angle + angle) * polarPoint.radius + midpoint.x
    , y : Math.sin (polarPoint.angle + angle) * polarPoint.radius + midpoint.y
    }

graphSpaceToGraphEdgeSpace :: GraphSpacePoint2D -> GraphSpacePoint2D -> GraphSpacePoint2D -> GraphEdgeSpacePoint2D
graphSpaceToGraphEdgeSpace (GraphSpacePoint2D sourcePos) (GraphSpacePoint2D targetPos) (GraphSpacePoint2D graphSpacePoint) =
  GraphEdgeSpacePoint2D $ sourceTargetCartesianToEdgePolar sourcePos targetPos graphSpacePoint

sourceTargetCartesianToEdgePolar :: Point2D -> Point2D -> Point2D -> Point2DPolar
sourceTargetCartesianToEdgePolar sourcePos targetPos point =
  let
    sourceTargetVector = targetPos - sourcePos
    midpoint = { x : (targetPos.x + sourcePos.x) / 2.0
               , y : (targetPos.y + sourcePos.y) / 2.0
               }
    sourceTargetAngle = Math.atan2 sourceTargetVector.y sourceTargetVector.x
    positionRelativeToMidpoint = point - midpoint
    totalAngle = Math.atan2 positionRelativeToMidpoint.y positionRelativeToMidpoint.x
    norm point2D = Math.sqrt (point2D.x * point2D.x + point2D.y * point2D.y)
  in
    { angle : totalAngle - sourceTargetAngle
    , radius : norm positionRelativeToMidpoint
    }

pageEdgeSpaceToPageSpace :: PageSpacePoint2D -> PageSpacePoint2D -> PageEdgeSpacePoint2D -> PageSpacePoint2D
pageEdgeSpaceToPageSpace (PageSpacePoint2D sourcePos) (PageSpacePoint2D targetPos) (PageEdgeSpacePoint2D pageEdgeSpacePoint) =
  PageSpacePoint2D $ edgePolarToSourceTargetCartesian sourcePos targetPos pageEdgeSpacePoint

pageSpaceToPageEdgeSpace :: PageSpacePoint2D -> PageSpacePoint2D -> PageSpacePoint2D -> PageEdgeSpacePoint2D
pageSpaceToPageEdgeSpace (PageSpacePoint2D sourcePos) (PageSpacePoint2D targetPos) (PageSpacePoint2D pageSpacePoint) =
  PageEdgeSpacePoint2D $ sourceTargetCartesianToEdgePolar sourcePos targetPos pageSpacePoint

type EdgeMetadata
  = { id      :: EdgeId
    , graphId :: GraphId
    , source  :: NodeId
    , target  :: NodeId
    }

-- | Invariants:
-- | edge.source.graphId == edge.graphId
-- | edge.target.graphId == edge.graphId
type EdgeRow
  = ( id             :: EdgeId
    , graphId        :: GraphId
    , source         :: NodeId
    , target         :: NodeId
    , midpointAngle  :: Number
    , midpointRadius :: Number
    , text           :: String
    , isValid        :: Boolean
    )
type Edge = Record EdgeRow

freshEdge :: EdgeMetadata -> Edge
freshEdge edgeMetadata =
  Builder.build (Builder.merge edgeMetadata)
  $ { text           : ""
    , midpointAngle  : 0.0
    , midpointRadius : 0.0
    , isValid        : true
    }

type NodeRow
  = ( id        :: NodeId
    , graphId   :: GraphId
    , subgraph  :: Maybe GraphId
    , positionX :: Number
    , positionY :: Number
    , text      :: String
    , isValid   :: Boolean
    )
type Node = Record NodeRow

freshNode :: GraphId -> NodeId -> Node
freshNode graphId id
  = { id        : id
    , graphId   : graphId
    , subgraph  : Nothing
    , positionX : 0.0
    , positionY : 0.0
    , text      : ""
    , isValid   : true
    }

data Focus
  = FocusNode GraphId NodeId
  | FocusEdge GraphId EdgeId
  | FocusNodeMappingEdge MappingId EdgeId
  | FocusEdgeMappingEdge MappingId EdgeId

derive instance eqFocus :: Eq Focus
derive instance ordFocus :: Ord Focus
derive instance genericFocus :: Generic Focus _

instance showFocus :: Show Focus where
  show = genericShow

data PathEquation = PathEquation (Array EdgeId) (Array EdgeId)

derive instance eqPathEquation :: Eq PathEquation
derive instance ordPathEquation :: Ord PathEquation
derive instance genericPathEquation :: Generic PathEquation _
instance decodePathEquation :: Decode PathEquation where
  decode = genericDecode defaultOptions
instance encodePathEquation :: Encode PathEquation where
  encode = genericEncode defaultOptions
instance showPathEquation :: Show PathEquation where
  show (PathEquation leftPath rightPath) =
    "PathEquation: " <> show leftPath <> " == " <> show rightPath

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
               , idMap :: Map EdgeId Edge
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
              , idMap : Map.empty
              }
    , pathEquations : Set.empty
    }


type MappingId = UUID

-- | For mapping edges, the midpoint is interpreted in page-space coords,
-- | rather than graph-space coords like the normal edges.
type NodeMappingEdge
  = { id         :: EdgeId
    , mappingId  :: MappingId
    , sourceNode :: NodeId
    , targetNode :: NodeId
    , midpoint   :: PageEdgeSpacePoint2D
    }

-- | For mapping edges, the midpoint is interpreted in page-space coords,
-- | rather than graph-space coords like the normal edges.
type EdgeMappingEdge
  = { id         :: EdgeId
    , mappingId  :: MappingId
    , sourceEdge :: EdgeId
    , targetEdge :: EdgeId
    , midpoint   :: PageEdgeSpacePoint2D
    }

-- | all ((==) mapping.sourceGraph) (mapping.nodeMappingEdges <#> _.sourceNode.graphId)
-- | all ((==) mapping.targetGraph) (mapping.nodeMappingEdges <#> _.targetNode.graphId)
-- | all ((==) mapping.sourceGraph) (mapping.edgeMappingEdges <#> _.sourceEdge.graphId)
-- | all ((==) mapping.targetGraph) (mapping.edgeMappingEdges <#> _.targetEdge.graphId)
type Mapping
  = { id :: MappingId
    , name :: String
    , sourceGraph :: GraphId
    , targetGraph :: GraphId
    , nodeMappingEdges :: Map EdgeId NodeMappingEdge
    , edgeMappingEdges :: Map EdgeId EdgeMappingEdge
    }

emptyMapping :: MappingId -> GraphId -> GraphId -> Mapping
emptyMapping id sourceId targetId
  = { id : id
    , name : ""
    , sourceGraph : sourceId
    , targetGraph : targetId
    , nodeMappingEdges : Map.empty
    , edgeMappingEdges : Map.empty
    }

type GraphView
  = { graphId      :: GraphId
    , origin       :: PageSpacePoint2D
    , zoom         :: Number
    , focus        :: Maybe Focus
    , boundingRect :: WHE.DOMRect
    }

freshPane :: GraphId -> WHE.DOMRect -> GraphView
freshPane graphId rect
  = { graphId      : graphId
    , origin       : PageSpacePoint2D { x : 0.0, y : 0.0 }
    , zoom         : 1.0
    , focus        : Nothing
    , boundingRect : rect
    }


------
-- Lenses

_title :: Lens' Graph GraphTitle
_title = prop (SProxy :: SProxy "title")

_text :: Lens' GraphTitle String
_text = prop (SProxy :: SProxy "text")

_nodes :: Lens' Graph (Map NodeId Node)
_nodes = prop (SProxy :: SProxy "nodes")

_sourceTarget :: Lens' Graph (Map NodeId (Map NodeId (Map EdgeId Edge)))
_sourceTarget = prop (SProxy :: SProxy "edges") <<< prop (SProxy :: SProxy "sourceTarget")

_targetSource :: Lens' Graph (Map NodeId (Map NodeId (Map EdgeId Edge)))
_targetSource = prop (SProxy :: SProxy "edges") <<< prop (SProxy :: SProxy "targetSource")

_idMap :: Lens' Graph (Map EdgeId Edge)
_idMap = prop (SProxy :: SProxy "edges") <<< prop (SProxy :: SProxy "idMap")

_edge :: EdgeId -> Lens' Graph (Maybe Edge)
_edge edgeId = _idMap <<< at edgeId

_position :: Lens' Node GraphSpacePoint2D
_position = lens nodePosition updateNodePosition

_nodeText :: NodeId -> Traversal' Graph String
_nodeText nodeId = prop (SProxy :: SProxy "nodes") <<< at nodeId <<< traversed <<< prop (SProxy :: SProxy "text")

_nodeSubgraph :: NodeId -> Traversal' Graph (Maybe GraphId)
_nodeSubgraph nodeId = _nodes <<< at nodeId <<< traversed <<< prop (SProxy :: SProxy "subgraph")

_pathEquations :: Lens' Graph (Set PathEquation)
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

_focus :: Lens' GraphView (Maybe Focus)
_focus = prop (SProxy :: SProxy "focus")

_boundingRect :: Lens' GraphView WHE.DOMRect
_boundingRect = prop (SProxy :: SProxy "boundingRect")

_height :: Lens' WHE.DOMRect Number
_height = prop (SProxy :: SProxy "height")

_nodeMappingEdges :: Lens' Mapping (Map EdgeId NodeMappingEdge)
_nodeMappingEdges = prop (SProxy :: SProxy "nodeMappingEdges")

_edgeMappingEdges :: Lens' Mapping (Map EdgeId EdgeMappingEdge)
_edgeMappingEdges = prop (SProxy :: SProxy "edgeMappingEdges")

_source :: Lens' Edge NodeId
_source = prop (SProxy :: SProxy "source")

_target :: Lens' Edge NodeId
_target = prop (SProxy :: SProxy "target")

_subgraph :: Lens' Node (Maybe GraphId)
_subgraph = prop (SProxy :: SProxy "subgraph")


------
-- Interface

insertNewNode :: NodeId -> Graph -> Graph
insertNewNode nodeId graph =
  graph { nodes =
             Map.insert
             nodeId
             (freshNode graph.id nodeId)
             graph.nodes
        }

updateNode :: Node -> Graph -> Graph
updateNode node graph =
  graph # _nodes %~ Map.insert node.id node

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
    idMap = Map.insert edgeMetadata.id newEdge graph.edges.idMap
  in
    graph { edges = { sourceTarget : edgesSourceTarget
                    , targetSource : edgesTargetSource
                    , idMap : idMap
                    }
          }

batchInsertEdges :: Array Edge -> Graph -> Graph
batchInsertEdges edges graph = foldr updateEdge graph edges

updateEdge :: Edge -> Graph -> Graph
updateEdge edge =
  insertNewEdge (edgeToMetadata edge)
  >>>
  updateEdgeData (const edge) edge.id

updateEdgeData :: (Edge -> Edge) -> EdgeId -> Graph -> Graph
updateEdgeData updater edgeId graph =
  case lookupEdgeById edgeId graph of
    Nothing -> graph
    Just edge ->
      graph
      # (_sourceTarget <<< at edge.source <<< traversed <<< at edge.target <<< traversed <<< at edge.id <<< traversed %~ updater)
      # (_targetSource <<< at edge.target <<< traversed <<< at edge.source <<< traversed <<< at edge.id <<< traversed %~ updater)
      # (_idMap <<< at edge.id <<< traversed %~ updater)

deleteEdge :: EdgeId -> Graph -> Graph
deleteEdge edgeId graph =
  case lookupEdgeById edgeId graph of
    Nothing -> graph
    Just edge ->
      graph
      # (_sourceTarget <<< at edge.source <<< traversed <<< at edge.target <<< traversed <<< at edge.id .~ Nothing)
      # (_targetSource <<< at edge.target <<< traversed <<< at edge.source <<< traversed <<< at edge.id .~ Nothing)
      # (_idMap <<< at edge.id .~ Nothing)

nodePosition :: Node -> GraphSpacePoint2D
nodePosition node = GraphSpacePoint2D { x : node.positionX, y : node.positionY }

updateNodePosition :: Node -> GraphSpacePoint2D -> Node
updateNodePosition node (GraphSpacePoint2D newPos) =
  node { positionX = newPos.x, positionY = newPos.y }

moveNode :: NodeId -> GraphSpacePoint2D -> Graph -> Graph
moveNode nodeId newPos =
  prop (SProxy :: SProxy "nodes") <<< at nodeId %~ map (flip updateNodePosition newPos)

updateNodeText :: NodeId -> String -> Graph -> Graph
updateNodeText nodeId newText = _nodeText nodeId .~ newText

updateEdgeText :: EdgeId -> String -> Graph -> Graph
updateEdgeText edgeId newText = updateEdgeData _{ text = newText } edgeId

edgeMidpoint :: Edge -> GraphEdgeSpacePoint2D
edgeMidpoint edge = GraphEdgeSpacePoint2D { angle : edge.midpointAngle, radius : edge.midpointRadius }

updateEdgeMidpoint :: EdgeId -> GraphEdgeSpacePoint2D -> Graph -> Graph
updateEdgeMidpoint edgeId (GraphEdgeSpacePoint2D newMidpoint) =
  updateEdgeData _{ midpointAngle = newMidpoint.angle, midpointRadius = newMidpoint.radius } edgeId

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

updateNodeMappingEdge :: NodeMappingEdge -> Mapping -> Mapping
updateNodeMappingEdge nodeMappingEdge =
  _nodeMappingEdges %~ Map.insert nodeMappingEdge.id nodeMappingEdge

deleteNodeMappingEdge :: EdgeId -> Mapping -> Mapping
deleteNodeMappingEdge nodeMappingEdgeId =
  _nodeMappingEdges %~ Map.delete nodeMappingEdgeId

updateEdgeMappingEdge :: EdgeMappingEdge -> Mapping -> Mapping
updateEdgeMappingEdge edgeMappingEdge =
  _edgeMappingEdges %~ Map.insert edgeMappingEdge.id edgeMappingEdge

deleteEdgeMappingEdge :: EdgeId -> Mapping -> Mapping
deleteEdgeMappingEdge edgeMappingEdgeId =
  _edgeMappingEdges %~ Map.delete edgeMappingEdgeId

updateNodeMappingEdgeMidpoint :: EdgeId -> PageEdgeSpacePoint2D -> Mapping -> Mapping
updateNodeMappingEdgeMidpoint nodeMappingEdgeId newMidpoint =
  _nodeMappingEdges <<< at nodeMappingEdgeId <<< traversed %~ _{ midpoint = newMidpoint }

updateEdgeMappingEdgeMidpoint :: EdgeId -> PageEdgeSpacePoint2D -> Mapping -> Mapping
updateEdgeMappingEdgeMidpoint edgeMappingEdgeId newMidpoint =
  _edgeMappingEdges <<< at edgeMappingEdgeId <<< traversed %~ _{ midpoint = newMidpoint }


------
-- Utilities

edgeToMetadata :: Edge -> EdgeMetadata
edgeToMetadata edge
  = { id : edge.id
    , graphId : edge.graphId
    , source : edge.source
    , target : edge.target
    }

allEdgesTouchingNode :: NodeId -> Graph -> { incoming :: Array Edge, outgoing :: Array Edge }
allEdgesTouchingNode nodeId graph =
  let
    outgoingEdges = case Map.lookup nodeId graph.edges.sourceTarget of
      Nothing -> []
      Just targetEdgeMap -> Array.fromFoldable $ List.concatMap Map.values $ Map.values targetEdgeMap
    incomingEdges = case Map.lookup nodeId graph.edges.targetSource of
      Nothing -> []
      Just targetEdgeMap -> Array.fromFoldable $ List.concatMap Map.values $ Map.values targetEdgeMap
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

lookupEdgeById :: EdgeId -> Graph -> Maybe Edge
lookupEdgeById edgeId graph = do
  Map.lookup edgeId graph.edges.idMap

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
