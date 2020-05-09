-- | A Megagraph is a graph where each node is linked to another graph, and each
-- | edge is linked to a mapping between graphs.
-- | Graphs can also have path-equations, allowing them to represent ologs, with
-- | mappings representing functors between ologs.
module Megagraph where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray, head, length)
import Data.Foldable (all, foldr)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', Traversal', lens, traversed, (%~), (.~), (?~))
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), fst)
import Data.UUID (UUID)
import Data.UUID as UUID
import Foreign.Class (class Encode, class Decode)
import Foreign.Generic (genericEncode, genericDecode, defaultOptions)
import FunctorialDataMigration.Core.Signature (Signature, sortPath)
import FunctorialDataMigration.Core.Signature as FDM
import FunctorialDataMigration.Core.SignatureMapping (SignatureMapping)
import Math as Math
import Record.Builder as Builder
import StringRewriting.KnuthBendix (Equation(..))
import Test.QuickCheck (class Arbitrary)
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

derive newtype instance arbitraryGraphEdgeSpacePoint2D :: Arbitrary GraphEdgeSpacePoint2D

instance encodeGraphEdgeSpacePoint2D :: Encode GraphEdgeSpacePoint2D where
  encode x = x # genericEncode defaultOptions

instance decodeGraphEdgeSpacePoint2D :: Decode GraphEdgeSpacePoint2D where
  decode x = x # genericDecode defaultOptions

newtype PageEdgeSpacePoint2D = PageEdgeSpacePoint2D { angle :: Number, radius :: Number }

derive newtype instance showPageEdgeSpacePoint2D :: Show PageEdgeSpacePoint2D

derive newtype instance eqPageEdgeSpacePoint2D :: Eq PageEdgeSpacePoint2D

derive newtype instance ordPageEdgeSpacePoint2D :: Ord PageEdgeSpacePoint2D

derive instance genericPageEdgeSpacePoint2D :: Generic PageEdgeSpacePoint2D _

derive newtype instance arbitraryPageEdgeSpacePoint2D :: Arbitrary PageEdgeSpacePoint2D

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
    , deleted        :: Boolean
    )
type Edge = Record EdgeRow

freshEdge :: EdgeMetadata -> Edge
freshEdge edgeMetadata =
  Builder.build (Builder.merge edgeMetadata)
  $ { text           : ""
    , midpointAngle  : 0.0
    , midpointRadius : 0.0
    , isValid        : true
    , deleted        : false
    }

type NodeRow
  = ( id        :: NodeId
    , graphId   :: GraphId
    , subgraph  :: Maybe GraphId
    , positionX :: Number
    , positionY :: Number
    , text      :: String
    , isValid   :: Boolean
    , deleted   :: Boolean
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
    , deleted   : false
    }

data MegagraphElement
  = NodeElement GraphId NodeId
  | EdgeElement GraphId EdgeId
  | NodeMappingEdgeElement MappingId EdgeId
  | EdgeMappingEdgeElement MappingId EdgeId

derive instance eqMegagraphElement :: Eq MegagraphElement

derive instance ordMegagraphElement :: Ord MegagraphElement

derive instance genericMegagraphElement :: Generic MegagraphElement _

instance showMegagraphElement :: Show MegagraphElement where
  show = genericShow

type PathEquationId = UUID

type PathEquation
  = { id :: PathEquationId
    , graphId :: GraphId
    , pathA :: Array EdgeId
    , pathB :: Array EdgeId
    , deleted :: Boolean
    }

freshPathEquation :: PathEquationId -> GraphId -> PathEquation
freshPathEquation id graphId = {id: id, graphId: graphId, pathA: [], pathB: [], deleted: false}

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
    , edges ::  Map EdgeId Edge
    , pathEquations :: Map PathEquationId PathEquation
    }

emptyGraph :: GraphId -> Graph
emptyGraph id
  = { id : id
    , title : { text : UUID.toString id
              , isValid : true
              }
    , nodes : Map.empty
    , edges : Map.empty
    , pathEquations : Map.empty
    }


type MappingId = UUID

-- | For mapping edges, the midpoint is interpreted in page-space coords,
-- | rather than graph-space coords like the normal edges.
type NodeMappingEdgeRow
  = ( id             :: EdgeId
    , mappingId      :: MappingId
    , sourceNode     :: NodeId
    , targetNode     :: NodeId
    , midpointAngle  :: Number
    , midpointRadius :: Number
    , deleted        :: Boolean
    )
type NodeMappingEdge = Record NodeMappingEdgeRow

freshNodeMappingEdge :: EdgeId -> MappingId -> NodeId -> NodeId -> NodeMappingEdge
freshNodeMappingEdge edgeId mappingId sourceNodeId targetNodeId
  = { id             : edgeId
    , mappingId      : mappingId
    , sourceNode     : sourceNodeId
    , targetNode     : targetNodeId
    , midpointAngle  : 0.0
    , midpointRadius : 0.0
    , deleted        : false
    }

-- | For mapping edges, the midpoint is interpreted in page-space coords,
-- | rather than graph-space coords like the normal edges.
type EdgeMappingEdgeRow
  = ( id             :: EdgeId
    , mappingId      :: MappingId
    , sourceEdge     :: EdgeId
    , targetEdge     :: EdgeId
    , midpointAngle  :: Number
    , midpointRadius :: Number
    , deleted        :: Boolean
    )
type EdgeMappingEdge = Record EdgeMappingEdgeRow

freshEdgeMappingEdge :: EdgeId -> MappingId -> EdgeId -> EdgeId -> EdgeMappingEdge
freshEdgeMappingEdge edgeId mappingId sourceEdgeId targetEdgeId
  = { id             : edgeId
    , mappingId      : mappingId
    , sourceEdge     : sourceEdgeId
    , targetEdge     : targetEdgeId
    , midpointAngle  : 0.0
    , midpointRadius : 0.0
    , deleted        : false
    }

-- | all ((==) mapping.sourceGraph) (mapping.nodeMappingEdges <#> _.sourceNode.graphId)
-- | all ((==) mapping.targetGraph) (mapping.nodeMappingEdges <#> _.targetNode.graphId)
-- | all ((==) mapping.sourceGraph) (mapping.edgeMappingEdges <#> _.sourceEdge.graphId)
-- | all ((==) mapping.targetGraph) (mapping.edgeMappingEdges <#> _.targetEdge.graphId)
type Mapping
  = { id :: MappingId
    , title :: String
    , sourceGraph :: GraphId
    , targetGraph :: GraphId
    , nodeMappingEdges :: Map EdgeId NodeMappingEdge
    , edgeMappingEdges :: Map EdgeId EdgeMappingEdge
    , isValid :: Boolean
    }

emptyMapping :: MappingId -> GraphId -> GraphId -> Mapping
emptyMapping id sourceId targetId
  = { id : id
    , title : UUID.toString id
    , sourceGraph : sourceId
    , targetGraph : targetId
    , nodeMappingEdges : Map.empty
    , edgeMappingEdges : Map.empty
    , isValid : true
    }

-- | A megagraph is a collection of graphs and mappings between graphs
-- | where each node has a subgraph and each edge has a submapping.
-- | A mapping is a set of edges between graphs, from nodes to nodes and edges to edges.
-- | The UI represents a sub-megagraph, where there is only a single mapping
-- | between any pairs of graphs. This is a view on a larger megagraph.
type Megagraph
  = { graphs :: Map GraphId Graph
    , mappings :: Map MappingId Mapping
    }

emptyMegagraph :: Megagraph
emptyMegagraph = { graphs : Map.empty
                 , mappings : Map.empty
                 }

type GraphView
  = { graphId      :: GraphId
    , origin       :: PageSpacePoint2D
    , zoom         :: Number
    , focus        :: Maybe MegagraphElement
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

_title :: forall r t. Lens' {title :: t | r} t
_title = prop (SProxy :: SProxy "title")

_isValid :: forall r. Lens' {isValid :: Boolean | r} Boolean
_isValid = prop (SProxy :: SProxy "isValid")

_text :: forall r. Lens' {text :: String | r} String
_text = prop (SProxy :: SProxy "text")

_nodes :: Lens' Graph (Map NodeId Node)
_nodes = prop (SProxy :: SProxy "nodes")

_node :: NodeId -> Traversal' Graph Node
_node nodeId = _nodes <<< at nodeId <<< traversed

_edges :: Lens' Graph (Map EdgeId Edge)
_edges = prop (SProxy :: SProxy "edges")

_edge :: EdgeId -> Traversal' Graph Edge
_edge edgeId = _edges <<< at edgeId <<< traversed

_position :: Lens' Node GraphSpacePoint2D
_position = lens nodePosition updateNodePosition

_deleted :: forall r. Lens' {deleted :: Boolean | r} Boolean
_deleted = prop (SProxy :: SProxy "deleted")

_nodeText :: NodeId -> Traversal' Graph String
_nodeText nodeId = prop (SProxy :: SProxy "nodes") <<< at nodeId <<< traversed <<< prop (SProxy :: SProxy "text")

_nodeSubgraph :: NodeId -> Traversal' Graph (Maybe GraphId)
_nodeSubgraph nodeId = _nodes <<< at nodeId <<< traversed <<< prop (SProxy :: SProxy "subgraph")

_pathEquations :: Lens' Graph (Map PathEquationId PathEquation)
_pathEquations = prop (SProxy :: SProxy "pathEquations")

_graphs :: Lens' Megagraph (Map GraphId Graph)
_graphs = prop (SProxy :: SProxy "graphs")

_graph :: GraphId -> Traversal' Megagraph Graph
_graph graphId = _graphs <<< at graphId <<< traversed

_mappings :: Lens' Megagraph (Map MappingId Mapping)
_mappings = prop (SProxy :: SProxy "mappings")

_mapping :: MappingId -> Traversal' Megagraph Mapping
_mapping mappingId = _mappings <<< at mappingId <<< traversed

_nodeMappingEdges :: Lens' Mapping (Map EdgeId NodeMappingEdge)
_nodeMappingEdges = prop (SProxy :: SProxy "nodeMappingEdges")

_nodeMappingEdge :: EdgeId -> Traversal' Mapping NodeMappingEdge
_nodeMappingEdge edgeId = _nodeMappingEdges <<< at edgeId <<< traversed

_edgeMappingEdges :: Lens' Mapping (Map EdgeId EdgeMappingEdge)
_edgeMappingEdges = prop (SProxy :: SProxy "edgeMappingEdges")

_edgeMappingEdge :: EdgeId -> Traversal' Mapping EdgeMappingEdge
_edgeMappingEdge edgeId = _edgeMappingEdges <<< at edgeId <<< traversed

_source :: Lens' Edge NodeId
_source = prop (SProxy :: SProxy "source")

_target :: Lens' Edge NodeId
_target = prop (SProxy :: SProxy "target")

_subgraph :: Lens' Node (Maybe GraphId)
_subgraph = prop (SProxy :: SProxy "subgraph")


------
-- Interface

insertNode :: Node -> Graph -> Graph
insertNode node graph =
  graph { nodes = graph.nodes # Map.insert node.id node }

updateNode :: Node -> Graph -> Graph
updateNode node = _nodes <<< at node.id ?~ node

deleteNode :: NodeId -> Graph -> Graph
deleteNode nodeId graph =
  graph { nodes = Map.delete nodeId graph.nodes }

insertEdge :: Edge -> Graph -> Graph
insertEdge edge = _edges <<< at edge.id ?~ edge

batchInsertEdges :: Array Edge -> Graph -> Graph
batchInsertEdges edges graph = foldr updateEdge graph edges

updateEdge :: Edge -> Graph -> Graph
updateEdge edge =
  insertEdge edge
  >>>
  updateEdgeData (const edge) edge.id

updateEdgeData :: (Edge -> Edge) -> EdgeId -> Graph -> Graph
updateEdgeData updater edgeId graph =
  case lookupEdgeById edgeId graph of
    Nothing -> graph
    Just edge -> graph # _edge edge.id %~ updater

deleteEdge :: EdgeId -> Graph -> Graph
deleteEdge edgeId graph =
  case lookupEdgeById edgeId graph of
    Nothing -> graph
    Just edge -> graph # _edges <<< at edge.id .~ Nothing

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

connectSubgraph :: NodeId -> Maybe GraphId -> Graph -> Graph
connectSubgraph nodeId maybeGraphId =
  _nodeSubgraph nodeId .~ maybeGraphId

updatePathEquation :: PathEquation -> Graph -> Graph
updatePathEquation pathEquation =
  _pathEquations %~ Map.insert pathEquation.id pathEquation

deletePathEquation :: PathEquation -> Graph -> Graph
deletePathEquation pathEquation =
  _pathEquations %~ Map.insert pathEquation.id (pathEquation {deleted = true})

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
updateNodeMappingEdgeMidpoint nodeMappingEdgeId (PageEdgeSpacePoint2D newMidpoint) =
  _nodeMappingEdges <<< at nodeMappingEdgeId <<< traversed %~ _{ midpointAngle = newMidpoint.angle, midpointRadius = newMidpoint.radius }

updateEdgeMappingEdgeMidpoint :: EdgeId -> PageEdgeSpacePoint2D -> Mapping -> Mapping
updateEdgeMappingEdgeMidpoint edgeMappingEdgeId (PageEdgeSpacePoint2D newMidpoint) =
  _edgeMappingEdges <<< at edgeMappingEdgeId <<< traversed %~ _{ midpointAngle = newMidpoint.angle, midpointRadius = newMidpoint.radius }

-- | Not typesafe, don't use with Edge!
mappingEdgeMidpoint :: forall r. {midpointAngle :: Number, midpointRadius :: Number | r} -> PageEdgeSpacePoint2D
mappingEdgeMidpoint mappingEdge = PageEdgeSpacePoint2D {angle: mappingEdge.midpointAngle, radius: mappingEdge.midpointRadius}


------
-- Path Equations

-- | For a path equation to be valid, it has to have two non-intersecting paths
-- | from the same source to the same target.
-- | All the edges have to be from the same graph.
edgeSetIsValidPathEquation :: Graph -> Set (Tuple GraphId EdgeId) -> Boolean
edgeSetIsValidPathEquation graph graphIdEdgeIdSet =
  -- edgeSet is non-empty
  (Set.size graphIdEdgeIdSet > 0)
  &&
  -- All edges are part of the same graph
  (all (fst >>> (==) graph.id) graphIdEdgeIdSet)
  &&
  -- Each node is counted twice when counting sources and targets.
  -- The source node will be counted twice for both initial edges of the paths,
  -- the target node will be counted twice for both final edges of the paths,
  -- and each intermediate node will be counted twice for its entering and leaving
  -- edges.
  -- This guarantees that the paths don't intersect, and that they are unbroken.
  (all ((==) 2) nodeCounts)
  &&
  (Set.size graphIdEdgeIdSet == Array.length edges)
  where
    edges = graphIdEdgeIdSet
            # Set.map (\(Tuple graphId edgeId) -> lookupEdgeById edgeId graph)
            # Array.fromFoldable
            # Array.catMaybes
    nodeCounts = foldr countNode Map.empty edges
    countNode edge nodeCounts' =
      nodeCounts'
      # Map.alter inc edge.source
      # Map.alter inc edge.target
    inc = case _ of
      Just n -> Just (n + 1)
      Nothing -> Just 1

edgeSetToPathEquation :: Graph -> PathEquationId -> Set (Tuple GraphId EdgeId) -> Maybe PathEquation
edgeSetToPathEquation graph id graphIdEdgeIdSet =
  if not edgeSetIsValidPathEquation graph graphIdEdgeIdSet then Nothing else do
    Tuple pathA pathB <- pathPairFromEdgeSet edges
    graphId <- _.graphId <$> Set.findMin edges
    pure $ (freshPathEquation id graphId) {pathA = _.id <$> pathA, pathB = _.id <$> pathB}
    where
      edges = graphIdEdgeIdSet
              # Set.mapMaybe (\(Tuple graphId edgeId) -> lookupEdgeById edgeId graph)
      sourceNodeId :: Set Edge -> Maybe NodeId
      sourceNodeId edgeSet = Set.findMin $ Set.filter (not <<< flip Set.member targets) sources
        where
          sources = Set.map _.source edgeSet
          targets = Set.map _.target edgeSet
      pathFromSource :: NodeId -> Set Edge -> Array Edge
      pathFromSource source edgeSet =
        case Set.findMin $ Set.filter (\edge -> edge.source == source) edgeSet of
          Nothing -> []
          Just firstEdge ->
            let
              rest = pathFromSource firstEdge.target edgeSet
            in
              Array.cons firstEdge rest
      pathPairFromEdgeSet :: Set Edge -> Maybe (Tuple (Array Edge) (Array Edge))
      pathPairFromEdgeSet edgeSet = do
        source <- sourceNodeId edgeSet
        let
          pathA = pathFromSource source edgeSet
          pathB = pathFromSource source (foldr Set.delete edgeSet pathA)
        pure $ Tuple pathA pathB


------
-- Conversion to Functorial Data Migration representations

nodeToFDMNode :: Node -> FDM.Node
nodeToFDMNode node = UUID.toString node.id

edgeToFDMEdge :: Edge -> FDM.Edge
edgeToFDMEdge edge = {source: UUID.toString edge.source, target: UUID.toString edge.target}

pathEquationToFDMEquation :: Graph -> PathEquation -> Equation FDM.Edge
pathEquationToFDMEquation graph pathEquation =
  let
    pathToFDMEdgeArray = Array.catMaybes <<< map (flip lookupEdgeById graph >>> map edgeToFDMEdge)
  in
    Equation (pathToFDMEdgeArray pathEquation.pathA) (pathToFDMEdgeArray pathEquation.pathB)

graphToSignature :: Graph -> Signature
graphToSignature graph
  = { nodes: graph.nodes
             # Map.filter (not <<< _.deleted)
             <#> nodeToFDMNode
             # Map.values # Set.fromFoldable
    , edges: graph.edges
             # Map.filter (not <<< _.deleted)
             <#> edgeToFDMEdge
             # Map.values # Set.fromFoldable
    , pathEquations: graph.pathEquations
                     # Map.filter (not <<< _.deleted)
                     <#> pathEquationToFDMEquation graph
                     # Map.values # Set.fromFoldable
    }

nodeMappingEdgeToFDMPair :: NodeMappingEdge -> Tuple FDM.Node FDM.Node
nodeMappingEdgeToFDMPair nodeMappingEdge =
  Tuple (UUID.toString nodeMappingEdge.sourceNode) (UUID.toString nodeMappingEdge.targetNode)

edgeMappingEdgesWithCommonSourceToFDMPair :: Graph -> Graph -> NonEmptyArray EdgeMappingEdge -> Maybe (Tuple FDM.Edge FDM.Path)
edgeMappingEdgesWithCommonSourceToFDMPair sourceGraph targetGraph edgeMappingEdgeGroup = do
  sourceFDMEdge <- head edgeMappingEdgeGroup
                   # _.sourceEdge
                   # flip lookupEdgeById sourceGraph
                   <#> edgeToFDMEdge
  targetFDMPath <- edgeMappingEdgeGroup
                   # Set.fromFoldable
                   # Set.mapMaybe (_.targetEdge >>> flip lookupEdgeById targetGraph)
                   # Set.map edgeToFDMEdge
                   # sortPath
  if Array.length targetFDMPath == length edgeMappingEdgeGroup
  then
    pure $ Tuple sourceFDMEdge targetFDMPath
  else Nothing

mappingToSignatureMapping :: Mapping -> Graph -> Graph -> SignatureMapping
mappingToSignatureMapping mapping sourceGraph targetGraph =
  let
    nodeMappingFunction = mapping.nodeMappingEdges
                          # Map.filter (not <<< _.deleted)
                          # Map.filter (_.sourceNode >>> flip Map.lookup sourceGraph.nodes >>> map (not <<< _.deleted) >>> fromMaybe true)
                          # Map.filter (_.targetNode >>> flip Map.lookup targetGraph.nodes >>> map (not <<< _.deleted) >>> fromMaybe true)
                          <#> nodeMappingEdgeToFDMPair
                          # Map.values # Set.fromFoldable
    edgeMappingFunction = mapping.edgeMappingEdges
                          # Map.filter (not <<< _.deleted)
                          # Map.values # Array.fromFoldable
                          # Array.filter (_.sourceEdge >>> flip Map.lookup sourceGraph.edges >>> map (not <<< _.deleted) >>> fromMaybe true)
                          # Array.filter (_.targetEdge >>> flip Map.lookup targetGraph.edges >>> map (not <<< _.deleted) >>> fromMaybe true)
                          # Array.sortBy (comparing _.sourceEdge)
                          # Array.groupBy (\eA eB -> eA.sourceEdge == eB.sourceEdge)
                          <#> edgeMappingEdgesWithCommonSourceToFDMPair sourceGraph targetGraph
                          # Array.catMaybes
                          # Set.fromFoldable
  in
    { source: graphToSignature sourceGraph
    , target: graphToSignature targetGraph
    , nodeFunction: nodeMappingFunction
    , edgeFunction: edgeMappingFunction
    }


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
  { outgoing : edgeArray graph # Array.filter (\edge -> edge.source == nodeId)
  , incoming : edgeArray graph # Array.filter (\edge -> edge.target == nodeId)
  }

edgeArray :: Graph -> Array Edge
edgeArray graph =
  graph.edges
  # Map.values
  # Array.fromFoldable

lookupEdgeById :: EdgeId -> Graph -> Maybe Edge
lookupEdgeById edgeId graph = do
  Map.lookup edgeId graph.edges

removeDeleted :: Megagraph -> Megagraph
removeDeleted =
  (_graphs %~ map (_nodes %~ Map.filter (not <<< _.deleted)))
  >>>
  (_graphs %~ map (_edges %~ Map.filter (not <<< _.deleted)))
  >>>
  (_graphs %~ map (_pathEquations %~ Map.filter (not <<< _.deleted)))
  >>>
  (_mappings %~ map (_nodeMappingEdges %~ Map.filter (not <<< _.deleted)))
  >>>
  (_mappings %~ map (_edgeMappingEdges %~ Map.filter (not <<< _.deleted)))
