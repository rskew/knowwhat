module Gen where

import Prelude

import AppState (MegagraphState)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Array (length, replicate, zipWith)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.NonEmpty (NonEmpty(..))
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))
import Data.UUID as UUID
import Effect.Unsafe (unsafePerformEffect)
import Interpreter (interpretGraphOperation)
import Megagraph (Edge, EdgeId, EdgeMappingEdge, Graph, GraphEdgeSpacePoint2D(..), GraphId, Mapping, MappingId, Node, NodeMappingEdge, PageEdgeSpacePoint2D(..), PathEquation(..), emptyGraph, emptyMapping, freshEdge, freshNode, freshPane)
import MegagraphOperation (EquationOperation(..), GraphOperation(..), MappingOperation(..), MegagraphOperation(..))
import Record (merge)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, arrayOf, elements, oneOf)

nodeGen :: GraphId -> Gen Node
nodeGen graphId = do
  let nodeId = unsafePerformEffect UUID.genUUID
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
  let
    edgeId = unsafePerformEffect UUID.genUUID
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
  elements <$> nonEmptyValues graph.edges.idMap

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

newtype TestGraph = TestGraph Graph

instance testGraphArbitrary :: Arbitrary TestGraph where
  arbitrary = graphGen

derive instance newtypeTestGraph :: Newtype TestGraph _

graphGen :: Gen TestGraph
graphGen = do
  let graphId = unsafePerformEffect UUID.genUUID

  -- Generate a bunch of nodes, add them to graph
  node <- nodeGen graphId
  nodes' <- arrayOf $ nodeGen graphId
  let
    nodes = Array.cons node nodes'
    nonEmptyNodes = Array.cons node nodes'

  let graph = interpretGraphOperation (InsertNodes nodes) (emptyGraph graphId)

  -- generate a bunch of edges between the nodes
  edges <- arrayOf $ edgeGen $ NonEmpty node nonEmptyNodes

  let graph' = interpretGraphOperation (InsertEdges edges) graph

  pure $ TestGraph graph'

nodeMappingEdgeGen :: MappingId -> NonEmpty Array Node -> NonEmpty Array Node -> Gen NodeMappingEdge
nodeMappingEdgeGen mappingId sourceNodes targetNodes = do
  let
    edgeId = unsafePerformEffect UUID.genUUID
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
  let
    edgeId = unsafePerformEffect UUID.genUUID
  sourceEdge <- elements sourceEdges
  targetEdge <- elements targetEdges
  PageEdgeSpacePoint2D midpoint <- arbitrary :: Gen PageEdgeSpacePoint2D
  pure $ { id         : edgeId
         , mappingId  : mappingId
         , sourceEdge  : sourceEdge.id
         , targetEdge  : targetEdge.id
         , midpointAngle : midpoint.angle
         , midpointRadius : midpoint.radius
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
mappingGen graphs =
  let
    mappingId = unsafePerformEffect UUID.genUUID
  in do
    sourceGraph <- elements graphs
    targetGraph <- elements graphs
    edgeMappingEdges <- mappingEdgesGen edgeMappingEdgeGen mappingId sourceGraph.edges.idMap targetGraph.edges.idMap
    nodeMappingEdges <- mappingEdgesGen nodeMappingEdgeGen mappingId sourceGraph.nodes targetGraph.nodes
    pure $ (emptyMapping mappingId sourceGraph.id targetGraph.id)
           { nodeMappingEdges = nodeMappingEdges
           , edgeMappingEdges = edgeMappingEdges
           }

megagraphGen :: Gen MegagraphState
megagraphGen = do
  testGraph <- graphGen
  testGraphs <- arrayOf graphGen
  let
    graphs = Array.cons (unwrap testGraph) (unwrap <$> testGraphs)
    nonEmptyGraphs = NonEmpty (unwrap testGraph) (unwrap <$> testGraphs)
  mappings <- arrayOf $ mappingGen nonEmptyGraphs
  let rect = { width : 0.0, left : 0.0, right : 0.0, height : 0.0, top : 0.0, bottom : 0.0 }
  pure $ { graphs : graphs
                    <#> (\graph' -> Tuple graph'.id { graph : graph', view : freshPane graph'.id rect, history : [], undone : [] })
                    # Map.fromFoldable
         , mappings : mappings
                      <#> (\mapping' -> Tuple mapping'.id { mapping : mapping', history : [], undone : [] })
                      # Map.fromFoldable
         }

alwaysValidGraphOperationGen :: Graph -> Gen GraphOperation
alwaysValidGraphOperationGen graph = do
  oneOf $ NonEmpty
    ( do
        nodes <- nodesGen graph.id
        pure $ InsertNodes nodes )

    [ do
         nodes <- chooseNodes graph
         nodeUpdates <- nodeUpdatesGen graph nodes
         pure $ UpdateNodes nodes nodeUpdates

    , do
         nodes <- chooseNodes graph
         pure $ DeleteNodes nodes

    , do
         edges <- chooseEdges graph
         pure $ DeleteEdges edges

    , do
         newTitleText <- arbitrary :: Gen String
         pure $ UpdateTitle graph.title.text newTitleText

    , do
         newTitleIsValid <- arbitrary :: Gen Boolean
         pure $ SetTitleValidity graph.title.isValid newTitleIsValid
    ]

graphOperationGen :: Graph -> Gen (Maybe GraphOperation)
graphOperationGen graph = do
  oneOf $ NonEmpty
    ( Just <$> alwaysValidGraphOperationGen graph )

    [ sequence do
        genEdges <- edgesGen graph
        pure do
          edges <- genEdges
          pure $ InsertEdges edges

    , do
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
  pure $ PathEquation (_.id <$> pathA) (_.id <$> pathB)

choosePathEquation :: Graph -> Maybe (Gen PathEquation)
choosePathEquation graph =
  elements <$> nonEmptyOfEquations graph.pathEquations
    where
      nonEmptyOfEquations =
        map NonEmptyArray.toNonEmpty <<< NonEmptyArray.fromArray <<< Array.fromFoldable

equationOperationGen :: Graph -> Gen (Maybe EquationOperation)
equationOperationGen graph =
  oneOf $ NonEmpty
    ( do
        pathEquations <- arrayOf $ pathEquationGen graph
        pure $ Just $ InsertPathEquations pathEquations )

    [ sequence do
        genPathEquation <- choosePathEquation graph
        pure do
          pathEquations <- arrayOf genPathEquation
          pure $ DeletePathEquations pathEquations ]

mappingOperationGen :: Mapping -> Graph -> Graph -> Gen (Maybe MappingOperation)
mappingOperationGen mapping sourceGraph targetGraph =
  oneOf $ NonEmpty
    ( sequence do
         sourceNodes <- nonEmptyValues sourceGraph.nodes
         targetNodes <- nonEmptyValues targetGraph.nodes
         pure do
           nodeMappingEdges <- arrayOf $ nodeMappingEdgeGen mapping.id sourceNodes targetNodes
           pure $ InsertNodeMappingEdges nodeMappingEdges)


    [ do
         nodeMappingEdges <- chooseNodeMappingEdges mapping
         edgeUpdates <- nodeMappingEdgeUpdatesGen nodeMappingEdges
         pure $ Just $ UpdateNodeMappingEdges nodeMappingEdges edgeUpdates

    , do
         nodeMappingEdges <- chooseNodeMappingEdges mapping
         pure $ Just $ DeleteNodeMappingEdges nodeMappingEdges

    , sequence do
         sourceEdges <- nonEmptyValues sourceGraph.edges.idMap
         targetEdges <- nonEmptyValues targetGraph.edges.idMap
         pure do
           edgeMappingEdges <- arrayOf $ edgeMappingEdgeGen mapping.id sourceEdges targetEdges
           pure $ InsertEdgeMappingEdges edgeMappingEdges

    , do
         edgeMappingEdges <- chooseEdgeMappingEdges mapping
         edgeUpdates <- edgeMappingEdgeUpdatesGen edgeMappingEdges
         pure $ Just $ UpdateEdgeMappingEdges edgeMappingEdges edgeUpdates

    , do
         edgeMappingEdges <- chooseEdgeMappingEdges mapping
         pure $ Just $ DeleteEdgeMappingEdges edgeMappingEdges
    ]

megagraphOperationGen :: MegagraphState -> Gen (Maybe MegagraphOperation)
megagraphOperationGen megagraph =
  let
    mappingOpGen = runMaybeT do
      mappingState <- MaybeT $ traverse elements (nonEmptyValues megagraph.mappings)
      let mapping = mappingState.mapping
      sourceGraph :: Graph <- MaybeT $ pure $ Map.lookup mapping.sourceGraph megagraph.graphs <#> _.graph
      targetGraph :: Graph <- MaybeT $ pure $ Map.lookup mapping.targetGraph megagraph.graphs <#> _.graph
      mappingOperation <- MaybeT $ mappingOperationGen mapping sourceGraph targetGraph
      MaybeT $ pure $ Just $ MappingElementOperation mapping.id mappingOperation
  in
    mappingOpGen


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
      <$> (map (\nodeMappingEdge -> Tuple nodeMappingEdge.id nodeMappingEdge))
      <$> (arrayOf $ gen id nonEmptySourceElements nonEmptyTargetElements)
    _, _ -> pure Map.empty
