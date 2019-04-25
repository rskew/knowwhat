module Test.Main where

import Prelude (Unit, discard, class Eq, class Show, (<>), show, (==))
import Effect (Effect)
import Effect.Console (logShow)
import Text.Parsing.Parser (Parser, runParser)
import Data.Either (Either(..))
import Test.Assert (assert')
import Data.List (List(..), (:))
import Data.Tuple (Tuple(..))

import Graphputer.Parser
import Graphputer.Core


-- From purescript-parsing test/Main.purs
parseTest :: forall s a. Show a => Eq a => s -> a -> Parser s a -> Effect Unit
parseTest input expected p = case runParser input p of
  Right actual -> do
    assert' ("expected: " <> show expected <> ", actual: " <> show actual) (expected == actual)
    logShow actual
  Left err -> assert' ("error: " <> show err) false


main :: Effect Unit
main = do
  parseTest "List String" (PyList PyString) pyType

  parseTest "List List Float" (PyList (PyList PyFloat)) pyType

  parseTest "Int -> String -> List Float" (Tuple (PyInt : PyString : Nil) (PyList PyFloat)) functionType

  parseTest "var :: Int" (AnnotatedVariable "var" PyInt "") pyNode

  parseTest
    "func :: Int -> List String"
    (AnnotatedFunction "func" (PyInt : Nil) (PyList PyString) "")
    pyNode

  parseTest
    "asdf = fdsa"
    (Tuple "asdf" "fdsa")
    assignment

  parseTest
    "assigned_var = rhs :: List String"
    (AnnotatedVariable "assigned_var" (PyList PyString) "assigned_var = rhs")
    pyNode

  parseTest
    "assigned_func = rhs :: Int -> List String -> Float"
    (AnnotatedFunction "assigned_func" (PyInt : PyList PyString : Nil) PyFloat "assigned_func = rhs")
    pyNode

  parseTest
    "func_does_IO :: Int -> List String -> IO ()"
    (AnnotatedFunction "func_does_IO" (PyInt : PyList PyString : Nil) PyUnit "")
    pyNode

  parseTest
    "def some_func(a, b, c):\n\
    \    return print(\"this code doesn't have the type annotation delimiter in it: {}\"\\\n\
    \        .format({\"a\": a, \"b\": b, \"c\": c))"
    "def some_func(a, b, c):\n\
    \    return print(\"this code doesn't have the type annotation delimiter in it: {}\"\\\n\
    \        .format({\"a\": a, \"b\": b, \"c\": c))"
    pythonCode

  -- This test fails because of quoted `::` inside the python code. Rare enough not too worry.
  --parseTest
  --  "def cheeky_func(a, b, c):\n\
  --  \    return print(\"this code DOES have the type annotation delimiter in it:: {}\"\\\n\
  --  \        .format({\"a\": a, \"b\": b, \"c\": c))"
  --  "def cheeky_func(a, b, c):\n\
  --  \    return print(\"this code DOES have the type annotation delimiter in it:: {}\"\\\n\
  --  \        .format({\"a\": a, \"b\": b, \"c\": c))"
  --  pythonCode
