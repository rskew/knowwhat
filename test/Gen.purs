module Gen where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Array (length, replicate, zipWith)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.NonEmpty (NonEmpty(..), (:|))
import Data.String (codePointFromChar, fromCodePointArray)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))
import Data.UUID as UUID
import Megagraph (Edge, EdgeId, EdgeMappingEdge, Graph, GraphEdgeSpacePoint2D(..), GraphId, Mapping, MappingId, Megagraph, Node, NodeMappingEdge, PageEdgeSpacePoint2D(..), PathEquation, emptyGraph, emptyMapping, freshEdge, freshNode, freshPathEquation, updateEdge, updateNode)
import MegagraphStateUpdate (MegagraphStateUpdate(..))
import Record (merge)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, arrayOf, elements, oneOf, vectorOf)
import Unsafe.Coerce (unsafeCoerce)

-- | Can't perform effects in quickcheck tests, so we do our own genUUID!
-- | (Can't `unsafePerformEffect genUUID` either without strange bugs from optimisations
-- |  memoising results or something like that).
genHackyUUID :: Gen UUID.UUID
genHackyUUID =
  let
    hexChars = '0' :| ['1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f']
  in do
    time_low                           <- vectorOf 8  $ elements hexChars
    time_mid                           <- vectorOf 4  $ elements hexChars
    time_high                          <- vectorOf 3  $ elements hexChars
    clock_seq_hi_and_res_clock_seq_low <- vectorOf 3  $ elements hexChars
    node                               <- vectorOf 12 $ elements hexChars
    let
      version = '4'
      variant = '8'
      chars = time_low <> ['-']
              <> time_mid <> ['-']
              <> [version] <> time_high <> ['-']
              <> [variant] <> clock_seq_hi_and_res_clock_seq_low <> ['-']
              <> node
      str = fromCodePointArray $ codePointFromChar <$> chars
    pure $ unsafeCoerce str

nodeGen :: GraphId -> Gen Node
nodeGen graphId = do
  nodeId <- genHackyUUID
  text  <- arbitrary :: Gen String
  x     <- arbitrary :: Gen Number
  y     <- arbitrary :: Gen Number
  pure $ (freshNode graphId nodeId) { text = text, positionX = x, positionY = y }

nodesGen :: GraphId -> Gen (Array Node)
nodesGen graphId = arrayOf $ nodeGen graphId

chooseNode :: Graph -> Maybe (Gen Node)
chooseNode graph =
  elements <$> nonEmptyValues graph.nodes

chooseNodes :: Graph -> Gen (Array Node)
chooseNodes graph = fromMaybe (pure []) $ arrayOf <$> chooseNode graph

nodeUpdatesGen :: Graph -> Array Node -> Gen (Array Node)
nodeUpdatesGen graph nodes = do
    nodeUpdates <- sequence $ replicate (length nodes) (nodeGen graph.id)
    pure $ zipWith (\prevNode updateNode -> updateNode {id = prevNode.id}) nodes nodeUpdates

-- | Generate an edge for the nodes in a single graph
edgeGen :: NonEmpty Array Node -> Gen Edge
edgeGen nodes = do
  edgeId <- genHackyUUID
  sourceNode <- elements nodes
  targetNode <- elements nodes
  angle <- arbitrary :: Gen Number
  radius <- arbitrary :: Gen Number
  pure $ (freshEdge { id      : edgeId
                    , source  : sourceNode.id
                    , target  : targetNode.id
                    , graphId : sourceNode.graphId
                    })
                    { midpointAngle = angle
                    , midpointRadius = radius
                    }

edgesGen :: Graph -> Maybe (Gen (Array Edge))
edgesGen graph =
  (arrayOf <<< edgeGen) <$> nonEmptyValues graph.nodes

chooseEdge :: Graph -> Maybe (Gen Edge)
chooseEdge graph =
  elements <$> nonEmptyValues graph.edges

chooseEdges :: Graph -> Gen (Array Edge)
chooseEdges graph = fromMaybe (pure []) $ arrayOf <$> chooseEdge graph

edgeUpdatesGen :: Graph -> Array Edge -> Gen (Array Edge)
edgeUpdatesGen graph edges = do
  midpoints <- arrayOf arbitrary :: Gen (Array GraphEdgeSpacePoint2D)
  texts <- arrayOf arbitrary :: Gen (Array String)
  let updates = zipWith (\(GraphEdgeSpacePoint2D midpoint) text -> merge {text: text} midpoint) midpoints texts
  pure $ zipWith (\edge updates' ->
                     edge {text = updates'.text, midpointAngle = updates'.angle, midpointRadius = updates'.radius})
                 edges
                 updates

graphGen :: Gen Graph
graphGen = do
  graphId <- genHackyUUID

  -- Generate a bunch of nodes, add them to graph
  node <- nodeGen graphId
  nodes' <- arrayOf $ nodeGen graphId
  let
    nodes = Array.cons node nodes'
    nonEmptyNodes = NonEmpty node nodes'

  let graph = foldl (flip updateNode) (emptyGraph graphId) nodes

  -- generate a bunch of edges between the nodes
  edges <- arrayOf $ edgeGen nonEmptyNodes

  let graph' = foldl (flip updateEdge) graph edges

  pure graph'

nodeMappingEdgeGen :: MappingId -> NonEmpty Array Node -> NonEmpty Array Node -> Gen NodeMappingEdge
nodeMappingEdgeGen mappingId sourceNodes targetNodes = do
  edgeId <- genHackyUUID
  sourceNode <- elements sourceNodes
  targetNode <- elements targetNodes
  PageEdgeSpacePoint2D midpoint <- arbitrary :: Gen PageEdgeSpacePoint2D
  angle <- arbitrary :: Gen Number
  radius <- arbitrary :: Gen Number
  pure $ { id         : edgeId
         , mappingId  : mappingId
         , sourceNode  : sourceNode.id
         , targetNode  : targetNode.id
         , midpointAngle : midpoint.angle
         , midpointRadius : midpoint.radius
         , deleted : false
         }

chooseNodeMappingEdge :: Mapping -> Maybe (Gen NodeMappingEdge)
chooseNodeMappingEdge mapping =
  elements <$> nonEmptyValues mapping.nodeMappingEdges

chooseNodeMappingEdges :: Mapping -> Gen (Array NodeMappingEdge)
chooseNodeMappingEdges mapping =
  fromMaybe (pure []) $ arrayOf <$> chooseNodeMappingEdge mapping

nodeMappingEdgeUpdatesGen :: Array NodeMappingEdge -> Gen (Array NodeMappingEdge)
nodeMappingEdgeUpdatesGen nodeMappingEdges = do
  updatedMidpoints <- sequence $ replicate (length nodeMappingEdges) (arbitrary :: Gen PageEdgeSpacePoint2D)
  pure $ zipWith (\prevEdge (PageEdgeSpacePoint2D newMidpoint) ->
                     prevEdge {midpointAngle = newMidpoint.angle, midpointRadius = newMidpoint.radius})
                 nodeMappingEdges
                 updatedMidpoints

edgeMappingEdgeGen :: MappingId -> NonEmpty Array Edge -> NonEmpty Array Edge -> Gen EdgeMappingEdge
edgeMappingEdgeGen mappingId sourceEdges targetEdges = do
  edgeId <- genHackyUUID
  sourceEdge <- elements sourceEdges
  targetEdge <- elements targetEdges
  PageEdgeSpacePoint2D midpoint <- arbitrary :: Gen PageEdgeSpacePoint2D
  pure $ { id         : edgeId
         , mappingId  : mappingId
         , sourceEdge  : sourceEdge.id
         , targetEdge  : targetEdge.id
         , midpointAngle : midpoint.angle
         , midpointRadius : midpoint.radius
         , deleted : false
         }

chooseEdgeMappingEdge :: Mapping -> Maybe (Gen EdgeMappingEdge)
chooseEdgeMappingEdge mapping =
  elements <$> nonEmptyValues mapping.edgeMappingEdges

chooseEdgeMappingEdges :: Mapping -> Gen (Array EdgeMappingEdge)
chooseEdgeMappingEdges mapping =
  fromMaybe (pure []) $ arrayOf <$> chooseEdgeMappingEdge mapping

edgeMappingEdgeUpdatesGen :: Array EdgeMappingEdge -> Gen (Array EdgeMappingEdge)
edgeMappingEdgeUpdatesGen edgeMappingEdges = do
  updatedMidpoints <- sequence $ replicate (length edgeMappingEdges) arbitrary :: Gen (Array PageEdgeSpacePoint2D)
  pure $ zipWith (\prevEdge (PageEdgeSpacePoint2D newMidpoint) ->
                     prevEdge {midpointAngle = newMidpoint.angle, midpointRadius = newMidpoint.radius})
                 edgeMappingEdges
                 updatedMidpoints

mappingGen :: NonEmpty Array Graph -> Gen Mapping
mappingGen graphs = do
  mappingId <- genHackyUUID
  sourceGraph <- elements graphs
  targetGraph <- elements graphs
  nodeMappingEdges <- mappingEdgesGen nodeMappingEdgeGen mappingId sourceGraph.nodes targetGraph.nodes
  edgeMappingEdges <- mappingEdgesGen edgeMappingEdgeGen mappingId sourceGraph.edges targetGraph.edges
  pure $ (emptyMapping mappingId sourceGraph.id targetGraph.id)
         { nodeMappingEdges = nodeMappingEdges
         , edgeMappingEdges = edgeMappingEdges
         }

megagraphGen :: Gen Megagraph
megagraphGen = do
  graph <- graphGen
  moreGraphs <- arrayOf graphGen
  let
    graphs = Array.cons graph moreGraphs
    nonEmptyGraphs = NonEmpty graph moreGraphs
  mappings <- arrayOf $ mappingGen nonEmptyGraphs
  pure $ { graphs: graphs
                   <#> (\graph' -> Tuple graph'.id graph')
                   # Map.fromFoldable
         , mappings: mappings
                     <#> (\mapping -> Tuple mapping.id mapping)
                     # Map.fromFoldable
         }

newtype TestMegagraph = TestMegagraph Megagraph

instance testMegagraphArbitrary :: Arbitrary TestMegagraph where
  arbitrary = TestMegagraph <$> megagraphGen


alwaysValidGraphOperationGen :: Graph -> Gen MegagraphStateUpdate
alwaysValidGraphOperationGen graph = do
  oneOf $ NonEmpty
    ( do
         nodes <- chooseNodes graph
         nodeUpdates <- nodeUpdatesGen graph nodes
         pure $ UpdateNodes nodes nodeUpdates)
    [ do
         newTitleText <- arbitrary :: Gen String
         pure $ UpdateTitle graph.id graph.title.text newTitleText
    ]

graphOperationGen :: Graph -> Gen (Maybe MegagraphStateUpdate)
graphOperationGen graph = do
  oneOf $ NonEmpty
    ( Just <$> alwaysValidGraphOperationGen graph )
    [ do
         edges <- chooseEdges graph
         edgeUpdates <- edgeUpdatesGen graph edges
         pure $ Just $ UpdateEdges edges edgeUpdates
    ]

-- | Just make paths from any old arrays of edges, without worrying about
-- | making a valid path.
pathEquationGen :: Graph -> Gen PathEquation
pathEquationGen graph = do
  pathA <- chooseEdges graph
  pathB <- chooseEdges graph
  id <- genHackyUUID
  pure $ (freshPathEquation id graph.id) {pathA = _.id <$> pathA, pathB = _.id <$> pathB}

choosePathEquation :: Graph -> Maybe (Gen PathEquation)
choosePathEquation graph =
  elements <$> nonEmptyOfEquations graph.pathEquations
    where
      nonEmptyOfEquations =
        map NonEmptyArray.toNonEmpty <<< NonEmptyArray.fromArray <<< Array.fromFoldable

equationOperationGen :: Graph -> Gen (Maybe MegagraphStateUpdate)
equationOperationGen graph = do
  pathEquation <- pathEquationGen graph
  pure $ Just $ UpdatePathEquation (pathEquation {deleted = not pathEquation.deleted}) pathEquation

mappingOperationGen :: Mapping -> Graph -> Graph -> Gen (Maybe MegagraphStateUpdate)
mappingOperationGen mapping sourceGraph targetGraph =
  oneOf $ NonEmpty
    ( do
         nodeMappingEdges <- chooseNodeMappingEdges mapping
         edgeUpdates <- nodeMappingEdgeUpdatesGen nodeMappingEdges
         pure $ Just $ UpdateNodeMappingEdges nodeMappingEdges edgeUpdates)
    [ do
         edgeMappingEdges <- chooseEdgeMappingEdges mapping
         edgeUpdates <- edgeMappingEdgeUpdatesGen edgeMappingEdges
         pure $ Just $ UpdateEdgeMappingEdges edgeMappingEdges edgeUpdates
    ]

megagraphOperationGen :: Megagraph -> Gen (Maybe MegagraphStateUpdate)
megagraphOperationGen megagraph =
  let
    mappingOpGen = runMaybeT do
      mapping <- MaybeT $ traverse elements (nonEmptyValues megagraph.mappings)
      sourceGraph :: Graph <- MaybeT $ pure $ Map.lookup mapping.sourceGraph megagraph.graphs
      targetGraph :: Graph <- MaybeT $ pure $ Map.lookup mapping.targetGraph megagraph.graphs
      MaybeT $ mappingOperationGen mapping sourceGraph targetGraph
    graphOpGen = runMaybeT do
      graph <- MaybeT $ traverse elements (nonEmptyValues megagraph.graphs)
      MaybeT $ graphOperationGen graph
    equationOpGen = runMaybeT do
      graph <- MaybeT $ traverse elements (nonEmptyValues megagraph.graphs)
      MaybeT $ equationOperationGen graph
  in
    oneOf $ mappingOpGen :| [graphOpGen, equationOpGen]

data TestMegagraphWithOp = TestMegagraphWithOp Megagraph (Array MegagraphStateUpdate)

instance arbitraryTestMegagraphWithOp :: Arbitrary TestMegagraphWithOp where
  arbitrary = do
    megagraph <- megagraphGen
    maybesOp <- arrayOf (megagraphOperationGen megagraph)
    pure $ TestMegagraphWithOp megagraph (Array.catMaybes maybesOp)

------
-- Utils

nonEmptyValues :: forall id element. Map id element -> Maybe (NonEmpty Array element)
nonEmptyValues =
  map NonEmptyArray.toNonEmpty <<< NonEmptyArray.fromArray <<< Array.fromFoldable <<< Map.values

mappingEdgesGen :: forall r element.
                   (MappingId -> NonEmpty Array element -> NonEmpty Array element -> Gen {id :: UUID.UUID | r})
                   -> MappingId -> Map.Map EdgeId element -> Map.Map EdgeId element -> Gen (Map.Map EdgeId {id :: UUID.UUID | r})
mappingEdgesGen gen id sourceElements targetElements =
  case nonEmptyValues sourceElements, nonEmptyValues targetElements of
    Just nonEmptySourceElements, Just nonEmptyTargetElements ->
      Map.fromFoldable
      <$> (map (\mappingEdge -> Tuple mappingEdge.id mappingEdge))
      <$> (arrayOf $ gen id nonEmptySourceElements nonEmptyTargetElements)
    _, _ -> pure Map.empty
