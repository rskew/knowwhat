module Workflow.UIGraph.Undoable.JSON where

import Prelude

import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity(..))
import Data.List.NonEmpty (NonEmptyList)
import Data.Map (Map)
import Data.Map as Map
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.UUID (UUID, parseUUID)
import Data.UUID as UUID
import Foreign (ForeignError, renderForeignError)
import Foreign.Class (class Encode, class Decode)
import Foreign.Generic (defaultOptions, genericDecodeJSON, genericEncodeJSON, genericEncode, genericDecode)
import Foreign.Generic.Types (SumEncoding)
import Foreign.Object (Object)
import Foreign.Object as Object

import Data.Undoable
import Workflow.UIGraph.JSON
import Workflow.UIGraph.UIGraphOp


newtype UndoableForeignUIGraph =
  UndoableForeignUIGraph
  (Undoable ForeignUIGraph (UIGraphOp Unit))
derive instance genericUndoableForeignUIGraph :: Generic UndoableForeignUIGraph _
instance encodeUndoableForeignUIGraph :: Encode UndoableForeignUIGraph where
  encode = genericEncode genericEncodeOpts
instance decodeUndoableForeignUIGraph :: Decode UndoableForeignUIGraph where
  decode = genericDecode genericEncodeOpts
