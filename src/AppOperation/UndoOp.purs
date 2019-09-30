module AppOperation.UndoOp where

import Prelude
import Core (GraphId)

import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Tuple (Tuple(..))
import Data.Symbol (SProxy(..))
import Data.UUID as UUID
import Foreign (Foreign)
import Foreign.Class (class Encode, class Decode)
import Foreign.Generic (genericEncode, genericDecode, defaultOptions)
import Foreign.Utils (parseUUIDEither, toExceptT)
import Run (Run, FProxy)
import Run as Run


------
-- UndoOp DSL adding undo-redo functionality to GraphOps

data UndoOpF next
  = Undo GraphId next
  | Redo GraphId next

derive instance functorUndoOpF :: Functor UndoOpF

type UNDOOP = FProxy UndoOpF

_undoOp :: SProxy "undoOp"
_undoOp = SProxy

showUndoOp :: forall a. UndoOpF a -> Tuple String a
showUndoOp = case _ of
  Undo graphId next -> Tuple ("Undo " <> show graphId) next
  Redo graphId next -> Tuple ("Redo " <> show graphId) next


------
-- Interface

undo :: forall r. GraphId -> Run (undoOp :: UNDOOP | r) Unit
undo graphId = Run.lift _undoOp $ Undo graphId unit

redo :: forall r. GraphId -> Run (undoOp :: UNDOOP | r) Unit
redo graphId = Run.lift _undoOp $ Redo graphId unit


------
-- Serialisation/deserialisation

instance decodeUndoOpF :: Decode (UndoOpF Unit) where
  decode x = x # genericDecode defaultOptions >>= toExceptT <<< fromForeignUndoOpF

data ForeignUndoOpF
  = ForeignUndo String
  | ForeignRedo String

derive instance genericForeignUndoOpF :: Generic ForeignUndoOpF _

instance encodeForeignUndoOpF :: Encode ForeignUndoOpF where
  encode x = x # genericEncode defaultOptions

instance decodeForeignUndoOpF :: Decode ForeignUndoOpF where
  decode x = x # genericDecode defaultOptions

toForeignUndoOpF :: forall a. UndoOpF a -> Tuple Foreign a
toForeignUndoOpF = lmap (genericEncode defaultOptions) <<< case _ of
  Undo graphId next ->
    Tuple (ForeignUndo (UUID.toString graphId)) next
  Redo graphId next ->
    Tuple (ForeignRedo (UUID.toString graphId)) next

fromForeignUndoOpF :: ForeignUndoOpF -> Either String (UndoOpF Unit)
fromForeignUndoOpF = case _ of
  ForeignUndo graphIdStr -> do
    graphId <- parseUUIDEither graphIdStr
    pure $ Undo graphId unit
  ForeignRedo graphIdStr -> do
    graphId <- parseUUIDEither graphIdStr
    pure $ Redo graphId unit
