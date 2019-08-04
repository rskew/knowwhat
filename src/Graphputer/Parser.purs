module Graphputer.Parser where

import Graphputer.Core

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.List (List, (:), some)
import Data.List as List
import Data.Maybe (isJust)
import Data.String (trim)
import Data.String as String
import Data.String.CodePoints (codePointFromChar)
import Data.Tuple (Tuple(..))
import Prelude (bind, (>>=), ($), pure, (<>), (<$>), (<<<), map, (<*), (*>))
import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.Combinators (manyTill, (<?>), optional, try, lookAhead)
import Text.Parsing.Parser.String (string, char, oneOf, anyChar, noneOf, whiteSpace, eof)
import Text.Parsing.Parser.Token (letter, alphaNum)


canParseNodeText :: String -> Boolean
canParseNodeText nodeText = case runParser nodeText pyNode of
  Right _ -> true
  Left _ -> false

canCompose :: String -> String -> Boolean
canCompose funcStr argStr = case ( do
  Annotation _ funcType _  <- runParser funcStr annotation
  Annotation _ argType _ <- runParser argStr annotation
  case argType of
    PyFunc _ outputType ->
      pure $ isJust $ pyApply funcType outputType
    _ ->
      pure $ isJust $ pyApply funcType argType
  ) of
  Right ap -> ap
  Left _ -> false

pyNode :: Parser String PyNode
pyNode =
  FlowNode <$> annotation
  <|> do
    setupCode <- trim <$> pythonCode
    _ <- eof
    pure $ DeclarationNode setupCode

annotation :: Parser String Annotation
annotation =
  try withAssignment
  <|> do
    variableName <- pyIdentifier
    _ <- typeSignatureDelimiter
    t <- pyType
    _ <- eof
    pure $ Annotation variableName t ""

pyTypeFundamental :: Parser String PyType
pyTypeFundamental = (string "Bool" >>= \_ -> pure PyBool)
                <|> (string "String" >>= \_ -> pure PyString)
                <|> (string "Int" >>= \_ -> pure PyInt)
                <|> (string "Float" >>= \_ -> pure PyFloat)
                <|> (string "()" >>= \_ -> pure PyUnit)

-- | The IO declaration is just for documentation.
-- | It's stripped from parsed form.
pyTypePoint :: Parser String PyType
pyTypePoint = do
  _ <- optional $ special "IO"
  try pyTypeFundamental
  <|> do
      l <- special "List"
      t <- pyTypePoint
      pure $ PyList t

pyTypeFunction :: Parser String PyType
pyTypeFunction = do
  inputTypes <- some $ try $ pyTypePoint <* functionArrow
  outputType <- pyTypePoint
  pure $ PyFunc (List.toUnfoldable inputTypes) outputType
  <?> "type"
  where
    functionArrow = special "->"


pyType :: Parser String PyType
pyType = pyTypeFunction
  <|> pyTypePoint


pyIdentifier :: Parser String String
pyIdentifier = do
  _ <- whiteSpace
  first <- letter <|> char '_'
  rest <- some (alphaNum <|> char '_')
  _ <- whiteSpace
  pure $ charListToString $ first : rest

-- | If a variable or function includes an assignment, keep the left side as
-- | the variable name and the entire assignment as setup code
withAssignment :: Parser String Annotation
withAssignment = do
  Tuple variableName rightHandSide <- assignment
  _ <- typeSignatureDelimiter
  t <- pyType
  _ <- eof
  pure $ Annotation variableName t
       $ variableName <> " = " <> rightHandSide
  <?> "variable with assignment"

assignment :: Parser String (Tuple String String)
assignment = do
  -- TODO: parse python variable name
  --leftHandSide <- trim <$> charListToString <$> (some $ noneOf ['='])
  leftHandSide <- pyIdentifier
  _ <- oneOf ['=']
  _ <- lookAhead $ noneOf ['=']
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
typeSignatureDelimiter = special "::"

charListToString :: List Char -> String
charListToString = String.fromCodePointArray <<< map codePointFromChar <<< List.toUnfoldable

special :: String -> Parser String String
special s = whiteSpace *> string s <* whiteSpace
