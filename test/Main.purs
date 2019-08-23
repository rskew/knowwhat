module Test.Main where

import Effect (Effect)
import Effect.Console (logShow, log)
import Effect.Random
import Data.Array as Array
import Data.NonEmpty (NonEmpty)
import Data.Map as Map
import Data.Lens
import Data.Maybe (Maybe(..))
import Data.List.Lazy (replicateM)
import Data.NonEmpty (NonEmpty(..))
import Data.Foldable (foldl)
import Data.Unfoldable
import Data.Foldable
import Prelude
import Test.QuickCheck
import Test.QuickCheck.Gen
import Data.UUID
import Unsafe.Coerce (unsafeCoerce)

import Workflow.Core
import Workflow.UIGraph

freshNode :: UUID -> UINode
freshNode id = UINode
  { id : id
  , children : Map.empty
  , parents : Map.empty
  , subgraph : emptyUIGraph
  , position : { x : 0.0, y : 0.0 }
  , text : ""
  , isValid : true
  }

freshEdge :: NodeId -> NodeId -> UIEdge
freshEdge source target = UIEdge { source : source, target : target, isValid : true }

nodeGen :: Gen UINode
nodeGen = do
  idStr <- (arbitrary :: Gen String)
  let id = unsafeCoerce idStr
  pure $ freshNode id

edgeGen :: UIGraph -> NonEmpty Array UINode -> Gen (Array UIEdge)
edgeGen graph nodes = do
  let nNodes = Map.size $ graph ^. _nodes
  nEdges <- chooseInt 0 nNodes
  Array.fromFoldable <$> replicateM nEdges (do
    source <- elements nodes
    target <- elements nodes
    pure $ freshEdge (source ^. _id) (target ^. _id))

subset :: forall a. Array a -> Gen (Array a)
subset stuff = do
  case Array.uncons stuff of
    Nothing -> pure []
    Just consStuff -> do
      let nonEmptyStuff = NonEmpty consStuff.head consStuff.tail
      let nStuffs = length stuff
      nSubStuffs <- chooseInt 0 nStuffs
      subStuff <- replicateM nSubStuffs
        $ elements nonEmptyStuff
      pure $ Array.fromFoldable subStuff

data TestGraph = TestGraph UIGraph (Array UINode) (Array UINode) (Array UIEdge)

graphGen :: Gen TestGraph
graphGen = do
  -- Generate a bunch of nodes, add them to graph
  graphNodes <- arrayOf nodeGen
  node <- nodeGen
  let nodes = NonEmpty node graphNodes

  let graph = foldl insertNode emptyUIGraph nodes

  -- generate a bunch of edges between the nodes
  edges <- edgeGen graph nodes
  let graph' = foldl insertEdge graph edges

  subgraphNodes <- subset $ Array.fromFoldable $ Map.values $ graph' ^. _nodes

  extraNodes <- arrayOf nodeGen
  extraEdges <- edgeGen graph' nodes

  pure $ TestGraph graph' subgraphNodes extraNodes extraEdges

instance testGraphArbitrary :: Arbitrary TestGraph where
  arbitrary = graphGen

prop_UnglueGlue :: TestGraph -> Result
prop_UnglueGlue (TestGraph graph subgraphNodes _ _) =
  let
    unglued = unglue subgraphNodes graph
    --reglued = glue unglued.childGraph unglued.parentGraph
    reglued = glue unglued.parentGraph unglued.childGraph
  in
    case graph == reglued of
      true -> Success
      false -> Failed $ "Graph: \n"
               <> show (graph ^. _nodes)
               <> "\ndoesn't equal reglued: \n"
               <> show (reglued ^. _nodes)
               <> "\nwith subgraphNodes: \n"
               <> (show $ (\node -> node ^. _id) <$> subgraphNodes)

prop_InsertDualEdges :: TestGraph -> Result
prop_InsertDualEdges (TestGraph graph _ _ extraEdges) =
  let
    dualEdges = dualEdge <$> extraEdges
    nonDualInsertGraph = foldl insertEdge graph extraEdges
    dualInsertGraph = graph # withDual \g -> foldl insertEdge g dualEdges
  in
    nonDualInsertGraph === dualInsertGraph



main :: Effect Unit
main = do
  quickCheck prop_UnglueGlue
  quickCheck prop_InsertDualEdges



-- TODO: uncomment when python graphputer stuff is back
--import Graphputer.Core
--import Graphputer.Parser

--import Data.Either (Either(..))
--import Data.Tuple (Tuple(..))
--import Effect (Effect)
--import Data.Maybe (Maybe(..))
--import Effect.Console (logShow, log)
--import Prelude (Unit, discard, class Eq, class Show, (<>), show, (==), ($), map)
--import Test.Assert (assert')
--import Text.Parsing.Parser (Parser, parseErrorPosition, runParser)
--import Text.Parsing.Parser.Pos (Position(..))
--
--main :: Effect Unit
--main = do
--  testParser
--
---- From purescript-parsing test/Main.purs
--parseTest :: forall s a. Show a => Eq a => s -> a -> Parser s a -> Effect Unit
--parseTest input expected p = case runParser input p of
--  Right actual -> do
--    assert' ("expected: " <> show expected <> ", actual: " <> show actual) (expected == actual)
--    logShow actual
--  Left err -> assert' ("error: " <> show err) false
--
--parseErrorTestPosition :: forall s a. Show a => Parser s a -> s -> Position -> Effect Unit
--parseErrorTestPosition p input expected = case runParser input p of
--  Right _ -> assert' "error: ParseError expected!" false
--  Left err -> do
--    let pos = parseErrorPosition err
--    assert' ("expected: " <> show expected <> ", pos: " <> show pos) (expected == pos)
--    logShow expected
--
--mkPos :: Int -> Position
--mkPos n = mkPos' n 1
--
--mkPos' :: Int -> Int -> Position
--mkPos' column line = Position { column: column, line: line }
--
--applyTest :: PyType -> PyType -> PyType -> Effect Unit
--applyTest funcType argType expected = case pyApply funcType argType of
--  Just actual -> do
--    assert' ("expected " <> show expected <> ", actual " <> show actual) $ expected == actual
--    log $ "( " <> show funcType <> " ) " <> show argType <> " == " <> show expected
--  Nothing -> do
--    assert' ("failure to apply " <> show funcType <> " to " <> show argType) false
--
--
--testParser :: Effect Unit
--testParser = do
--  parseTest "String" PyString pyType
--  parseTest "Float" PyFloat pyType
--  parseTest "Int" PyInt pyType
--  parseTest "Bool" PyBool pyType
--  parseTest "()" PyUnit pyType
--  parseTest "String" PyString pyType
--  parseTest "List String" (PyList PyString) pyType
--  parseTest "List List Float" (PyList (PyList PyFloat)) pyType
--
--  parseErrorTestPosition pyType "[String]" $ mkPos 1
--  parseErrorTestPosition pyType "List" $ mkPos 5
--
--  parseTest "Int -> String -> List Float" (PyFunc [PyInt, PyString] (PyList PyFloat)) pyType
--  parseErrorTestPosition pyTypeFunction "List -> String" $ mkPos 1
--  parseErrorTestPosition pyTypeFunction "Float -> -> String" $ mkPos 10
--  parseErrorTestPosition pyTypeFunction "Float ->> String" $ mkPos 9
--  parseErrorTestPosition pyTypeFunction "Float - String" $ mkPos 1
--  parseErrorTestPosition pyTypeFunction "Float > String" $ mkPos 1
--
--  parseTest "var :: Int" (Annotation "var" PyInt "") annotation
--  parseErrorTestPosition pyNode "var :: List" $ mkPos 12
--
--  parseTest
--    "func :: Int -> List String"
--    (Annotation "func" (PyFunc [PyInt] (PyList PyString)) "")
--    annotation
--  parseErrorTestPosition pyNode "func :: List -> Int" $ mkPos 14
--  parseErrorTestPosition pyNode "func : Float -> Int" $ mkPos 6
--  parseErrorTestPosition pyNode "func :: Float - Int" $ mkPos 14
--
--  parseTest "asdf = fdsa" (Tuple "asdf" "fdsa") assignment
--  parseTest "asdf = fdsa == qwer" (Tuple "asdf" "fdsa == qwer") assignment
--  parseErrorTestPosition assignment "asdf == fdsa" $ mkPos 7
--  parseErrorTestPosition assignment "asdf  fdsa" $ mkPos 7
--  parseErrorTestPosition assignment "var :: Int" $ mkPos 5
--
--  parseTest
--    "assigned_var = rhs :: List String"
--    (Annotation "assigned_var" (PyList PyString) "assigned_var = rhs")
--    annotation
--  parseErrorTestPosition pyNode "assigned_var = rhs : List String" $ mkPos 14
--  parseErrorTestPosition pyNode "assigned_var == rhs :: List String" $ mkPos 14
--
--  parseTest
--    "assigned_func = rhs :: Int -> List String -> Float"
--    (Annotation "assigned_func" (PyFunc [PyInt, PyList PyString] PyFloat) "assigned_func = rhs")
--    annotation
--
--  parseTest
--    "func_does_IO :: Int -> List String -> IO ()"
--    (Annotation "func_does_IO" (PyFunc [PyInt, PyList PyString] PyUnit) "")
--    annotation
--
--  parseTest
--    "def some_func(a, b, c):\n\
--    \    return print(\"this code doesn't have the type annotation delimiter in it: {}\"\\\n\
--    \        .format({\"a\": a, \"b\": b, \"c\": c))"
--    "def some_func(a, b, c):\n\
--    \    return print(\"this code doesn't have the type annotation delimiter in it: {}\"\\\n\
--    \        .format({\"a\": a, \"b\": b, \"c\": c))"
--    pythonCode
--
--  -- This test fails because of quoted `::` inside the python code. Rare enough not too worry.
--  --parseTest
--  --  "def cheeky_func(a, b, c):\n\
--  --  \    return print(\"this code DOES have the type annotation delimiter in it:: {}\"\\\n\
--  --  \        .format({\"a\": a, \"b\": b, \"c\": c))"
--  --  "def cheeky_func(a, b, c):\n\
--  --  \    return print(\"this code DOES have the type annotation delimiter in it:: {}\"\\\n\
--  --  \        .format({\"a\": a, \"b\": b, \"c\": c))"
--  --  pythonCode
--
--  assert' ("Can't compare python types???") $ PyInt == PyInt
--  assert' ("Can't compare python types???") $ PyFloat == PyFloat
--  applyTest (PyFunc [PyInt] PyFloat) PyInt PyFloat