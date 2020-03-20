module Gen where

--import Prelude
--
--import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
--import Data.Array (length, replicate, zipWith)
--import Data.Array as Array
--import Data.Array.NonEmpty as NonEmptyArray
--import Data.Map (Map)
--import Data.Map as Map
--import Data.Maybe (Maybe(..), fromMaybe)
--import Data.Newtype (class Newtype, unwrap)
--import Data.NonEmpty (NonEmpty(..), (:|))
--import Data.Traversable (sequence, traverse)
--import Data.Tuple (Tuple(..))
--import Data.UUID as UUID
--import Effect.Unsafe (unsafePerformEffect)
--import Interpreter (interpretMegagraphStateUpdate)
--import Megagraph (Edge, EdgeId, EdgeMappingEdge, Graph, GraphEdgeSpacePoint2D(..), GraphId, Mapping, MappingId, Megagraph, Node, NodeMappingEdge, PageEdgeSpacePoint2D(..), PathEquation(..), emptyGraph, emptyMapping, freshEdge, freshNode, freshPane)
--import MegagraphStateUpdate (MegagraphStateUpdate(..))
--import Record (merge)
--import Test.QuickCheck (class Arbitrary, arbitrary)
--import Test.QuickCheck.Gen (Gen, arrayOf, elements, oneOf)
--
--nodeGen :: GraphId -> Gen Node
--nodeGen graphId = do
--  let nodeId = unsafePerformEffect UUID.genUUID
--  text  <- arbitrary :: Gen String
--  x     <- arbitrary :: Gen Number
--  y     <- arbitrary :: Gen Number
--  pure $ (freshNode graphId nodeId) { text = text, positionX = x, positionY = y }
--
--nodesGen :: GraphId -> Gen (Array Node)
--nodesGen graphId = arrayOf $ nodeGen graphId
--
--chooseNode :: Graph -> Maybe (Gen Node)
--chooseNode graph =
--  elements <$> nonEmptyValues graph.nodes
--
--chooseNodes :: Graph -> Gen (Array Node)
--chooseNodes graph = fromMaybe (pure []) $ arrayOf <$> chooseNode graph
--
--nodeUpdatesGen :: Graph -> Array Node -> Gen (Array Node)
--nodeUpdatesGen graph nodes = do
--    nodeUpdates <- sequence $ replicate (length nodes) (nodeGen graph.id)
--    pure $ zipWith (\prevNode updateNode -> updateNode {id = prevNode.id}) nodes nodeUpdates
--
---- | Generate an edge for the nodes in a single graph
--edgeGen :: NonEmpty Array Node -> Gen Edge
--edgeGen nodes = do
--  let
--    edgeId = unsafePerformEffect UUID.genUUID
--  sourceNode <- elements nodes
--  targetNode <- elements nodes
--  angle <- arbitrary :: Gen Number
--  radius <- arbitrary :: Gen Number
--  pure $ (freshEdge { id      : edgeId
--                    , source  : sourceNode.id
--                    , target  : targetNode.id
--                    , graphId : sourceNode.graphId
--                    })
--                    { midpointAngle = angle
--                    , midpointRadius = radius
--                    }
--
--edgesGen :: Graph -> Maybe (Gen (Array Edge))
--edgesGen graph =
--  (arrayOf <<< edgeGen) <$> nonEmptyValues graph.nodes
--
--chooseEdge :: Graph -> Maybe (Gen Edge)
--chooseEdge graph =
--  elements <$> nonEmptyValues graph.edges
--
--chooseEdges :: Graph -> Gen (Array Edge)
--chooseEdges graph = fromMaybe (pure []) $ arrayOf <$> chooseEdge graph
--
--edgeUpdatesGen :: Graph -> Array Edge -> Gen (Array Edge)
--edgeUpdatesGen graph edges = do
--  midpoints <- arrayOf arbitrary :: Gen (Array GraphEdgeSpacePoint2D)
--  texts <- arrayOf arbitrary :: Gen (Array String)
--  let updates = zipWith (\(GraphEdgeSpacePoint2D midpoint) text -> merge {text: text} midpoint) midpoints texts
--  pure $ zipWith (\edge updates' ->
--                     edge {text = updates'.text, midpointAngle = updates'.angle, midpointRadius = updates'.radius})
--                 edges
--                 updates
--
--newtype TestGraph = TestGraph Graph
--
--instance testGraphArbitrary :: Arbitrary TestGraph where
--  arbitrary = graphGen
--
--derive instance newtypeTestGraph :: Newtype TestGraph _
--
--graphGen :: Gen TestGraph
--graphGen = do
--  let graphId = unsafePerformEffect UUID.genUUID
--
--  -- Generate a bunch of nodes, add them to graph
--  node <- nodeGen graphId
--  nodes' <- arrayOf $ nodeGen graphId
--  let
--    nodes = Array.cons node nodes'
--    nonEmptyNodes = Array.cons node nodes'
--
--  let graph = interpretMegagraphStateUpdate (UpdateNodes (_{deleted = true} <$> nodes) nodes) (emptyGraph graphId)
--
--  -- generate a bunch of edges between the nodes
--  edges <- arrayOf $ edgeGen $ NonEmpty node nonEmptyNodes
--
--  let graph' = interpretMegagraphStateUpdate (UpdateEdges (_{deleted = true} <$> edges) edges) graph
--
--  pure $ TestGraph graph'
--
--nodeMappingEdgeGen :: MappingId -> NonEmpty Array Node -> NonEmpty Array Node -> Gen NodeMappingEdge
--nodeMappingEdgeGen mappingId sourceNodes targetNodes = do
--  let
--    edgeId = unsafePerformEffect UUID.genUUID
--  sourceNode <- elements sourceNodes
--  targetNode <- elements targetNodes
--  PageEdgeSpacePoint2D midpoint <- arbitrary :: Gen PageEdgeSpacePoint2D
--  angle <- arbitrary :: Gen Number
--  radius <- arbitrary :: Gen Number
--  pure $ { id         : edgeId
--         , mappingId  : mappingId
--         , sourceNode  : sourceNode.id
--         , targetNode  : targetNode.id
--         , midpointAngle : midpoint.angle
--         , midpointRadius : midpoint.radius
--         , deleted : false
--         }
--
--chooseNodeMappingEdge :: Mapping -> Maybe (Gen NodeMappingEdge)
--chooseNodeMappingEdge mapping =
--  elements <$> nonEmptyValues mapping.nodeMappingEdges
--
--chooseNodeMappingEdges :: Mapping -> Gen (Array NodeMappingEdge)
--chooseNodeMappingEdges mapping =
--  fromMaybe (pure []) $ arrayOf <$> chooseNodeMappingEdge mapping
--
--nodeMappingEdgeUpdatesGen :: Array NodeMappingEdge -> Gen (Array NodeMappingEdge)
--nodeMappingEdgeUpdatesGen nodeMappingEdges = do
--  updatedMidpoints <- sequence $ replicate (length nodeMappingEdges) (arbitrary :: Gen PageEdgeSpacePoint2D)
--  pure $ zipWith (\prevEdge (PageEdgeSpacePoint2D newMidpoint) ->
--                     prevEdge {midpointAngle = newMidpoint.angle, midpointRadius = newMidpoint.radius})
--                 nodeMappingEdges
--                 updatedMidpoints
--
--edgeMappingEdgeGen :: MappingId -> NonEmpty Array Edge -> NonEmpty Array Edge -> Gen EdgeMappingEdge
--edgeMappingEdgeGen mappingId sourceEdges targetEdges = do
--  let
--    edgeId = unsafePerformEffect UUID.genUUID
--  sourceEdge <- elements sourceEdges
--  targetEdge <- elements targetEdges
--  PageEdgeSpacePoint2D midpoint <- arbitrary :: Gen PageEdgeSpacePoint2D
--  pure $ { id         : edgeId
--         , mappingId  : mappingId
--         , sourceEdge  : sourceEdge.id
--         , targetEdge  : targetEdge.id
--         , midpointAngle : midpoint.angle
--         , midpointRadius : midpoint.radius
--         , deleted : false
--         }
--
--chooseEdgeMappingEdge :: Mapping -> Maybe (Gen EdgeMappingEdge)
--chooseEdgeMappingEdge mapping =
--  elements <$> nonEmptyValues mapping.edgeMappingEdges
--
--chooseEdgeMappingEdges :: Mapping -> Gen (Array EdgeMappingEdge)
--chooseEdgeMappingEdges mapping =
--  fromMaybe (pure []) $ arrayOf <$> chooseEdgeMappingEdge mapping
--
--edgeMappingEdgeUpdatesGen :: Array EdgeMappingEdge -> Gen (Array EdgeMappingEdge)
--edgeMappingEdgeUpdatesGen edgeMappingEdges = do
--  updatedMidpoints <- sequence $ replicate (length edgeMappingEdges) arbitrary :: Gen (Array PageEdgeSpacePoint2D)
--  pure $ zipWith (\prevEdge (PageEdgeSpacePoint2D newMidpoint) ->
--                     prevEdge {midpointAngle = newMidpoint.angle, midpointRadius = newMidpoint.radius})
--                 edgeMappingEdges
--                 updatedMidpoints
--
--mappingGen :: NonEmpty Array Graph -> Gen Mapping
--mappingGen graphs =
--  let
--    mappingId = unsafePerformEffect UUID.genUUID
--  in do
--    sourceGraph <- elements graphs
--    targetGraph <- elements graphs
--    edgeMappingEdges <- mappingEdgesGen edgeMappingEdgeGen mappingId sourceGraph.edges targetGraph.edges
--    nodeMappingEdges <- mappingEdgesGen nodeMappingEdgeGen mappingId sourceGraph.nodes targetGraph.nodes
--    pure $ (emptyMapping mappingId sourceGraph.id targetGraph.id)
--           { nodeMappingEdges = nodeMappingEdges
--           , edgeMappingEdges = edgeMappingEdges
--           }
--
--megagraphGen :: Gen Megagraph
--megagraphGen = do
--  testGraph <- graphGen
--  testGraphs <- arrayOf graphGen
--  let
--    graphs = Array.cons (unwrap testGraph) (unwrap <$> testGraphs)
--    nonEmptyGraphs = NonEmpty (unwrap testGraph) (unwrap <$> testGraphs)
--  mappings <- arrayOf $ mappingGen nonEmptyGraphs
--  let rect = { width : 0.0, left : 0.0, right : 0.0, height : 0.0, top : 0.0, bottom : 0.0 }
--  pure $ { graphs: graphs
--                   <#> (\graph -> Tuple graph.id graph)
--                   # Map.fromFoldable
--         , panes: graphs
--                  <#> (\graph -> Tuple graph.id (freshPane graph.id rect))
--                  # Map.fromFoldable
--         , mappings: mappings
--                     <#> (\mapping -> Tuple mapping.id mapping)
--                     # Map.fromFoldable
--         }
--
--alwaysValidGraphOperationGen :: Graph -> Gen MegagraphStateUpdate
--alwaysValidGraphOperationGen graph = do
--  oneOf $ NonEmpty
--    ( do
--         nodes <- chooseNodes graph
--         nodeUpdates <- nodeUpdatesGen graph nodes
--         pure $ UpdateNodes nodes nodeUpdates)
--    [ do
--         newTitleText <- arbitrary :: Gen String
--         pure $ UpdateTitle graph.title.text newTitleText
--    ]
--
--graphOperationGen :: Graph -> Gen (Maybe MegagraphStateUpdate)
--graphOperationGen graph = do
--  oneOf $ NonEmpty
--    ( Just <$> alwaysValidGraphOperationGen graph )
--    [ do
--         edges <- chooseEdges graph
--         edgeUpdates <- edgeUpdatesGen graph edges
--         pure $ Just $ UpdateEdges edges edgeUpdates
--    ]
--
---- | Just make paths from any old arrays of edges, without worrying about
---- | making a valid path.
--pathEquationGen :: Graph -> Gen PathEquation
--pathEquationGen graph = do
--  pathA <- chooseEdges graph
--  pathB <- chooseEdges graph
--  pure $ PathEquation (_.id <$> pathA) (_.id <$> pathB)
--
--choosePathEquation :: Graph -> Maybe (Gen PathEquation)
--choosePathEquation graph =
--  elements <$> nonEmptyOfEquations graph.pathEquations
--    where
--      nonEmptyOfEquations =
--        map NonEmptyArray.toNonEmpty <<< NonEmptyArray.fromArray <<< Array.fromFoldable
--
--equationOperationGen :: Graph -> Gen (Maybe MegagraphStateUpdate)
--equationOperationGen graph =
--  oneOf $ NonEmpty
--    ( do
--        pathEquations <- arrayOf $ pathEquationGen graph
--        pure $ Just $ InsertPathEquations pathEquations )
--    [ sequence do
--        genPathEquation <- choosePathEquation graph
--        pure do
--          pathEquations <- arrayOf genPathEquation
--          pure $ DeletePathEquations pathEquations ]
--
--mappingOperationGen :: Mapping -> Graph -> Graph -> Gen (Maybe MegagraphStateUpdate)
--mappingOperationGen mapping sourceGraph targetGraph =
--  oneOf $ NonEmpty
--    ( do
--         nodeMappingEdges <- chooseNodeMappingEdges mapping
--         edgeUpdates <- nodeMappingEdgeUpdatesGen nodeMappingEdges
--         pure $ Just $ UpdateNodeMappingEdges nodeMappingEdges edgeUpdates)
--    [ do
--         edgeMappingEdges <- chooseEdgeMappingEdges mapping
--         edgeUpdates <- edgeMappingEdgeUpdatesGen edgeMappingEdges
--         pure $ Just $ UpdateEdgeMappingEdges edgeMappingEdges edgeUpdates
--    ]
--
--megagraphOperationGen :: Megagraph -> Gen (Maybe MegagraphStateUpdate)
--megagraphOperationGen megagraph =
--  let
--    mappingOpGen = runMaybeT do
--      mapping <- MaybeT $ traverse elements (nonEmptyValues megagraph.mappings)
--      sourceGraph :: Graph <- MaybeT $ pure $ Map.lookup mapping.sourceGraph megagraph.graphs
--      targetGraph :: Graph <- MaybeT $ pure $ Map.lookup mapping.targetGraph megagraph.graphs
--      MaybeT $ mappingOperationGen mapping sourceGraph targetGraph
--    graphOpGen = runMaybeT do
--      graph <- MaybeT $ traverse elements (nonEmptyValues megagraph.graphs)
--      MaybeT $ graphOperationGen graph
--    -- TODO equationOperation
--  in
--    oneOf $ mappingOpGen :| [graphOpGen]
--
--data TestGraphWithOp = TestGraphWithOp Graph MegagraphStateUpdate
--
--instance arbitraryTestGraphWithOp :: Arbitrary TestGraphWithOp where
--  arbitrary = do
--    TestGraph graph <- graphGen
--    maybeOp <- graphOperationGen graph
--    pure $ case maybeOp of
--      Nothing -> TestGraphWithOp graph (UpdateTitle graph.title.text graph.title.text)
--      Just op -> TestGraphWithOp graph op
--
--------
---- Utils
--
--nonEmptyValues :: forall id element. Map id element -> Maybe (NonEmpty Array element)
--nonEmptyValues =
--  map NonEmptyArray.toNonEmpty <<< NonEmptyArray.fromArray <<< Array.fromFoldable <<< Map.values
--
--mappingEdgesGen :: forall r element.
--                   (MappingId -> NonEmpty Array element -> NonEmpty Array element -> Gen {id :: UUID.UUID | r})
--                   -> MappingId -> Map.Map EdgeId element -> Map.Map EdgeId element -> Gen (Map.Map EdgeId {id :: UUID.UUID | r})
--mappingEdgesGen gen id sourceElements targetElements =
--  case nonEmptyValues sourceElements, nonEmptyValues targetElements of
--    Just nonEmptySourceElements, Just nonEmptyTargetElements ->
--      Map.fromFoldable
--      <$> (map (\nodeMappingEdge -> Tuple nodeMappingEdge.id nodeMappingEdge))
--      <$> (arrayOf $ gen id nonEmptySourceElements nonEmptyTargetElements)
--    _, _ -> pure Map.empty
