module Graphputer.Core where

import Data.List (List)
import Data.Array (head, tail)
import Data.String (joinWith)
import Data.Tuple (Tuple)
import Prelude (class Show, (<>), show, ($), (==), map)
import Data.Eq (class Eq)
import Data.Maybe (Maybe(..))

import Workflow.Interaction.Impl


data PyType
  = PyUnit
  | PyBool
  | PyString
  | PyInt
  | PyFloat
  | PyList PyType
  | PyDict PyRow
  | PyFunc (Array PyType) PyType

derive instance eqPyType :: Eq PyType

instance showPyType :: Show PyType where
  show PyUnit = "Unit"
  show PyBool = "Bool"
  show PyString = "String"
  show PyInt = "Int"
  show PyFloat = "Float"
  show (PyList pyType) = "List " <> show pyType
  show (PyDict pyRow) = "Dict " <> show pyRow
  show (PyFunc inputs output) = joinWith " -> " $ map show $ inputs <> [output]

type PyRow = List (Tuple String PyType)

type SetupCode = String

data Annotation = Annotation String PyType SetupCode
derive instance eqAnnotation :: Eq Annotation
instance showAnnotation :: Show Annotation where
  show (Annotation name varType setup) = name <> " :: " <> show varType <> "\nsetup code: " <> setup

data PyNode
  = FlowNode Annotation
  | DeclarationNode SetupCode

derive instance eqPyNode :: Eq PyNode

instance showPyNode :: Show PyNode where
  show (FlowNode annotation) = show annotation
  show (DeclarationNode setupCode) = setupCode

pyApply :: PyType -> PyType -> Maybe PyType
pyApply func arg = case func of
  PyFunc inputs output -> case head inputs of
    Nothing -> Nothing
    Just firstInput ->
      if firstInput == arg
         then case tail inputs of
           Nothing -> Just output
           Just [] -> Just output
           Just restInputs -> Just $ PyFunc restInputs output
         else Nothing
  _ -> Nothing

convertToPython :: InterGraphImpl -> String
convertToPython g = "hmmmmm"
  -- collect setupCode nodes and concat text
  -- - put 'import' node text at beginning
  --
  -- wire up functions
  -- - what about intermediate variable names?
  -- entire function takes name from graph title, as does python module
                    
