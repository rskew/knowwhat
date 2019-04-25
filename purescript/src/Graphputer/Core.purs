module Graphputer.Core where

import Data.List (List)
import Data.Tuple (Tuple)
import Prelude (class Show, (<>), show)
import Data.Eq (class Eq)

--import Workflow.Core


data PyType
  = PyUnit
  | PyBool
  | PyString
  | PyInt
  | PyFloat
  | PyList PyType
  | PyDict PyRow
derive instance eqPyType :: Eq PyType
instance showPyType :: Show PyType where
  show PyUnit = "Unit"
  show PyBool = "Bool"
  show PyString = "String"
  show PyInt = "Int"
  show PyFloat = "Float"
  show (PyList pyType) = "List " <> show pyType
  show (PyDict pyRow) = "Dict " <> show pyRow

type PyRow = List (Tuple String PyType)

data PyNode
  = AnnotatedVariable String PyType SetupCode
  | AnnotatedFunction String (List PyType) PyType SetupCode
derive instance eqPyNode :: Eq PyNode
instance showPyNode :: Show PyNode where
  show (AnnotatedVariable name varType setup) = name <> " :: " <> show varType <> "\nsetup code: " <> setup
  show (AnnotatedFunction name inputTypes outputType setup) =
    name <> " :: " <> show inputTypes <> " -> " <> show outputType <> "\nsetup code: " <> setup

type SetupCode = String
