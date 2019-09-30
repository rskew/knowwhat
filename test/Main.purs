module Test.Main where

import Prelude

import AppOperation.GraphOp (encodeNodeAsGraphOp, interpretGraphOp, invertGraphOp)
import Core (emptyGraphData, insertNodeImpl, moveNodeImpl, updateNodeTextImpl)
import Test.QuickCheck (Result(..), (===), quickCheck')

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Tuple (fst)
import Effect (Effect)
import Run as Run
import GraphGen (TestGraph(..))


------
-- Properties to test

prop_InsertNode :: TestGraph -> Result
prop_InsertNode (TestGraph graphId graphData extraNodes _) =
  case Array.head extraNodes of
    Nothing -> Success
    Just node ->
      let
        interpreted = graphData # (fst $ Run.extract $ interpretGraphOp $ encodeNodeAsGraphOp node)
        manual = graphData
                 # insertNodeImpl graphId node.id
                 # moveNodeImpl node.id node.position
                 # updateNodeTextImpl node.id node.text
      in
        interpreted === manual

prop_InsertRemoveNodeA :: TestGraph -> Result
prop_InsertRemoveNodeA (TestGraph graphId graphData extraNodes _) =
  case Array.head extraNodes of
    Nothing -> Success
    Just node ->
      (graphData # (fst $ Run.extract $ interpretGraphOp
                    $ encodeNodeAsGraphOp node >>= const (invertGraphOp $ encodeNodeAsGraphOp node)))
      ===
      graphData

prop_InsertRemoveNodeB :: TestGraph -> Result
prop_InsertRemoveNodeB (TestGraph graphId graphData extraNodes _) =
  case Array.head extraNodes of
    Nothing -> Success
    Just node ->
      (graphData
       # (fst $ Run.extract $ interpretGraphOp $ encodeNodeAsGraphOp node)
       # (fst $ Run.extract $ interpretGraphOp $ invertGraphOp $ encodeNodeAsGraphOp node))
      ===
      graphData

-- TODO: uncomment when synth code is removed
--prop_EncodeDecode :: TestGraph -> Result
--prop_EncodeDecode (TestGraph graphId graphData _ _) =
--  let
--    baseAppOp = map (const unit) $ AppOperation $ encodeGraphDataAsGraphOp graphData
--  in
--    case runExceptT $ decode $ encode baseAppOp
--      Identity (Left err) -> Failed $ show $ Foreign.renderForeignError <$> err
--      Identity (Right (decodedAppOperation :: AppOperation Unit)) ->
--        let
--          roundtripAppState = interpretAppOperation decodedAppOperation emptyAppState
--          baseAppState      = interpretAppOperation baseAppOp emptyAppState
--        in
--          roundtripAppState === baseAppState

prop_fail :: TestGraph -> Result
prop_fail (TestGraph graphId graphData extraNodes extraEdges) =
  graphData === emptyGraphData

main :: Effect Unit
main = do
  quickCheck' 1_000 prop_InsertNode
  quickCheck' 1_000 prop_InsertRemoveNodeA
  quickCheck' 1_000 prop_InsertRemoveNodeB

  -- TODO: uncomment when synth code is removed
  --quickCheck $ prop_EncodeDecode











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
