module Graphputer.Parser where

import Graphputer.Core

import Control.Alt ((<|>))
import Data.List (List, some)
import Data.List as List
import Data.String (trim)
import Data.String as String
import Data.String.CodePoints (codePointFromChar)
import Data.Tuple (Tuple(..))
import Prelude (bind, (>>=), ($), pure, (<>), (<$>), (<<<), map, (<*), (*>))
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (manyTill, (<?>), optional, try, lookAhead)
import Text.Parsing.Parser.String (string, oneOf, anyChar, noneOf, whiteSpace, eof)

pyNode :: Parser String PyNode
pyNode =
  try functionWithAssignment
  <|> try variableWithAssignment
  -- func_name :: PyType -> PyType
  <|> try (do
    variableName <- trim <$> charListToString <$> manyTill anyChar (lookAhead typeSignatureDelimiter)
    _ <- typeSignatureDelimiter
    Tuple inputTypes outputType <- functionType
    pure $ AnnotatedFunction variableName inputTypes outputType ""
  -- TODO: handle lambda function case
  -- var_name :: PyType
  ) <|> do
    variableName <- trim <$> charListToString <$> manyTill anyChar (lookAhead typeSignatureDelimiter)
    _ <- typeSignatureDelimiter
    t <- pyType
    pure $ AnnotatedVariable variableName t ""

pyTypeFundamental :: Parser String PyType
pyTypeFundamental = (string "Bool" >>= \_ -> pure PyBool)
                <|> (string "String" >>= \_ -> pure PyString)
                <|> (string "Int" >>= \_ -> pure PyInt)
                <|> (string "Float" >>= \_ -> pure PyFloat)
                <|> (string "()" >>= \_ -> pure PyUnit)

-- |The IO declaration is, just for documentation.
-- |It's stripped from parsed form.
pyType :: Parser String PyType
pyType = do
  _ <- optional (string "IO" <* whiteSpace)
  try pyTypeFundamental
  <|> do
      l <- (string "List" <* whiteSpace)
      t <- pyType
      pure $ PyList t
  <?> "type"

-- | If a variable or function includes an assignment, keep the left side as
-- | the variable name and the entire assignment as setup code
variableWithAssignment :: Parser String PyNode
variableWithAssignment = do
  Tuple variableName rightHandSide <- assignment
  _ <- typeSignatureDelimiter
  t <- pyType
  pure $ AnnotatedVariable variableName t
       $ variableName <> " = " <> rightHandSide
  <?> "variable with assignment"

functionWithAssignment :: Parser String PyNode
functionWithAssignment = do
  Tuple variableName rightHandSide <- assignment
  _ <- typeSignatureDelimiter
  Tuple inputTypes outputType <- functionType
  pure $ AnnotatedFunction variableName inputTypes outputType
       $ variableName <> " = " <> rightHandSide
  <?> "function with assignment"

assignment :: Parser String (Tuple String String)
assignment = do
  leftHandSide <- trim <$> charListToString <$> (some $ noneOf ['='])
  _ <- oneOf ['=']
  rightHandSide <- pythonCode
  pure $ Tuple leftHandSide rightHandSide
  <?> "assignment"

-- | Note: this fails when the type-annotation delimiter is used inside
-- | python code. This should only happen quoted in a string,
-- | and is rare enough to leave as a TODO.
pythonCode :: Parser String String
pythonCode = trim <$> charListToString <$> (
  manyTill anyChar
      $ lookAhead typeSignatureDelimiter
      <|> eof *> pure "")
  <?> "python code"

typeSignatureDelimiter :: Parser String String
typeSignatureDelimiter = whiteSpace *> string "::" <* whiteSpace

functionType :: Parser String (Tuple (List PyType) PyType)
functionType = do
  inputTypes <- some $ try $ pyType <* functionArrow
  --_ <- functionArrow
  outputType <- pyType
  pure $ Tuple inputTypes outputType
  where
    functionArrow = whiteSpace *> string "->" <* whiteSpace

charListToString :: List Char -> String
charListToString = String.fromCodePointArray <<< map codePointFromChar <<< List.toUnfoldable
