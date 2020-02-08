module Gen where

import Prelude

import AppState (MegagraphState)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.NonEmpty (NonEmpty(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.UUID as UUID
import Effect.Unsafe (unsafePerformEffect)
import Interpreter (interpretGraphOperation)
import Megagraph (Edge, EdgeId, EdgeMappingEdge, Graph, GraphEdgeSpacePoint2D(..), GraphId, GraphSpacePoint2D(..), Mapping, MappingId, Node, NodeMappingEdge, PageEdgeSpacePoint2D(..), PathEquation(..), edgeToMetadata, emptyGraph, emptyMapping, freshEdge, freshNode, freshPane)
import MegagraphOperation (EquationOperation(..), GraphOperation(..), MappingOperation(..), MegagraphOperation(..), encodeEdgeAsGraphOperations, encodeNodeAsGraphOperations)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, arrayOf, elements, oneOf)

nodeGen :: GraphId -> Gen Node
nodeGen graphId = do
  let nodeId = unsafePerformEffect UUID.genUUID
  text  <- arbitrary :: Gen String
  x     <- arbitrary :: Gen Number
  y     <- arbitrary :: Gen Number
  pure $ (freshNode graphId nodeId) { text = text, position = GraphSpacePoint2D { x : x, y : y } }

chooseNode :: Graph -> Gen (Maybe Node)
chooseNode graph =
  traverse elements $ nonEmptyValues graph.nodes

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
                    { midpoint = GraphEdgeSpacePoint2D { angle : angle, radius : radius }
                    }

chooseEdge :: Graph -> Gen (Maybe Edge)
chooseEdge graph =
  traverse elements $ nonEmptyValues graph.edges.idMap

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

  let graph = foldl
                (flip interpretGraphOperation)
                (emptyGraph graphId)
                (Array.concatMap encodeNodeAsGraphOperations nodes)

  -- generate a bunch of edges between the nodes
  edges <- arrayOf $ edgeGen $ NonEmpty node nonEmptyNodes

  let graph' = foldl
                 (flip interpretGraphOperation)
                 graph
                 (Array.concatMap encodeEdgeAsGraphOperations edges)

  pure $ TestGraph graph'

nodeMappingEdgeGen :: MappingId -> NonEmpty Array Node -> NonEmpty Array Node -> Gen NodeMappingEdge
nodeMappingEdgeGen mappingId sourceNodes targetNodes = do
  let
    edgeId = unsafePerformEffect UUID.genUUID
  sourceNode <- elements sourceNodes
  targetNode <- elements targetNodes
  angle <- arbitrary :: Gen Number
  radius <- arbitrary :: Gen Number
  pure $ { id         : edgeId
         , mappingId  : mappingId
         , sourceNode  : sourceNode.id
         , targetNode  : targetNode.id
         , midpoint : PageEdgeSpacePoint2D { angle : angle, radius : radius }
         }

chooseNodeMappingEdge :: Mapping -> Gen (Maybe NodeMappingEdge)
chooseNodeMappingEdge mapping =
  traverse elements $ nonEmptyValues mapping.nodeMappingEdges

edgeMappingEdgeGen :: MappingId -> NonEmpty Array Edge -> NonEmpty Array Edge -> Gen EdgeMappingEdge
edgeMappingEdgeGen mappingId sourceEdges targetEdges = do
  let
    edgeId = unsafePerformEffect UUID.genUUID
  sourceEdge <- elements sourceEdges
  targetEdge <- elements targetEdges
  angle <- arbitrary :: Gen Number
  radius <- arbitrary :: Gen Number
  pure $ { id         : edgeId
         , mappingId  : mappingId
         , sourceEdge  : sourceEdge.id
         , targetEdge  : targetEdge.id
         , midpoint : PageEdgeSpacePoint2D { angle : angle, radius : radius }
         }

chooseEdgeMappingEdge :: Mapping -> Gen (Maybe EdgeMappingEdge)
chooseEdgeMappingEdge mapping =
  traverse elements $ nonEmptyValues mapping.edgeMappingEdges

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
    ( let
         nodeId = unsafePerformEffect $ UUID.genUUID
      in
       pure $ InsertNode nodeId )
    [ let
         edgeMetadata = { id      : unsafePerformEffect UUID.genUUID
                        , graphId : unsafePerformEffect UUID.genUUID
                        , source  : unsafePerformEffect UUID.genUUID
                        , target  : unsafePerformEffect UUID.genUUID
                        }
      in
        pure $ InsertEdge edgeMetadata
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

    [ chooseNode graph >>= traverse \node ->
        pure $ DeleteNode node.id

    , chooseEdge graph >>= traverse \edge ->
        pure $ DeleteEdge (edgeToMetadata edge)

    , chooseNode graph >>= traverse \node -> do
        x <- arbitrary :: Gen Number
        y <- arbitrary :: Gen Number
        pure $ MoveNode node.id node.position (GraphSpacePoint2D {x: x, y: y})

    , chooseNode graph >>= traverse \node -> do
        newText <- arbitrary :: Gen String
        pure $ UpdateNodeText node.id node.text newText

    , chooseEdge graph >>= traverse \edge -> do
        newText <- arbitrary :: Gen String
        pure $ UpdateEdgeText edge.id edge.text newText

    , chooseEdge graph >>= traverse \edge -> do
        angle <- arbitrary :: Gen Number
        radius <- arbitrary :: Gen Number
        pure $ MoveEdgeMidpoint edge.id edge.midpoint (GraphEdgeSpacePoint2D {angle: angle, radius: radius})

    , chooseNode graph >>= traverse \node -> do
        newSubgraph <- oneOf $ NonEmpty (pure Nothing) [ pure $ Just $ unsafePerformEffect UUID.genUUID ]
        pure $ ConnectSubgraph node.id node.subgraph newSubgraph
    ]

-- | Just make paths from any old arrays of edges, without worrying about
-- | making a valid path.
pathEquationGen :: Graph -> Gen PathEquation
pathEquationGen graph = do
  pathA <- Array.catMaybes <$> arrayOf (chooseEdge graph)
  pathB <- Array.catMaybes <$> arrayOf (chooseEdge graph)
  pure $ PathEquation (_.id <$> pathA) (_.id <$> pathB)

choosePathEquation :: Graph -> Gen (Maybe PathEquation)
choosePathEquation graph =
  traverse elements $ nonEmptyOfEquations graph.pathEquations
    where
      nonEmptyOfEquations =
        map NonEmptyArray.toNonEmpty <<< NonEmptyArray.fromArray <<< Array.fromFoldable

equationOperationGen :: Graph -> Gen (Maybe EquationOperation)
equationOperationGen graph =
  oneOf $ NonEmpty
    ( do
        pathEquation <- pathEquationGen graph
        pure $ Just $ InsertPathEquation pathEquation )
    [ do
        pathEquation <- choosePathEquation graph
        pure $ DeletePathEquation <$> pathEquation ]

mappingOperationGen :: Mapping -> Graph -> Graph -> Gen (Maybe MappingOperation)
mappingOperationGen mapping sourceGraph targetGraph =
  oneOf $ NonEmpty
    ( case nonEmptyValues sourceGraph.nodes, nonEmptyValues targetGraph.nodes of
        Just sourceGraphNodes, Just targetGraphNodes -> do
          nodeMappingEdge <- nodeMappingEdgeGen mapping.id sourceGraphNodes targetGraphNodes
          pure $ Just $ InsertNodeMappingEdge nodeMappingEdge
        _, _ -> pure $ Nothing )
    [ chooseNodeMappingEdge mapping >>= (pure <<< map DeleteNodeMappingEdge)
    , case nonEmptyValues sourceGraph.edges.idMap, nonEmptyValues targetGraph.edges.idMap of
        Just sourceGraphEdges, Just targetGraphEdges -> do
          edgeMappingEdge <- edgeMappingEdgeGen mapping.id sourceGraphEdges targetGraphEdges
          pure $ Just $ InsertEdgeMappingEdge edgeMappingEdge
        _, _ -> pure $ Nothing
    , chooseEdgeMappingEdge mapping >>= (pure <<< map DeleteEdgeMappingEdge)
    , do
        angle <- arbitrary :: Gen Number
        radius <- arbitrary :: Gen Number
        let newMidpoint = PageEdgeSpacePoint2D {angle: angle, radius: radius}
        maybeNodeMappingEdge <- chooseNodeMappingEdge mapping
        pure (maybeNodeMappingEdge <#> \nodeMappingEdge ->
          MoveNodeMappingEdgeMidpoint nodeMappingEdge.id nodeMappingEdge.midpoint newMidpoint)
    , do
        angle <- arbitrary :: Gen Number
        radius <- arbitrary :: Gen Number
        let newMidpoint = PageEdgeSpacePoint2D {angle: angle, radius: radius}
        maybeEdgeMappingEdge <- chooseEdgeMappingEdge mapping
        pure (maybeEdgeMappingEdge <#> \edgeMappingEdge ->
          MoveEdgeMappingEdgeMidpoint edgeMappingEdge.id edgeMappingEdge.midpoint newMidpoint)
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
