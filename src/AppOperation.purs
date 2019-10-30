module AppOperation where

import Prelude

import AppOperation.GraphOp (GRAPHOP, GraphOpF, _graphOp, encodeGraphDataAsGraphOp, showGraphOp, toForeignGraphOpF)
import AppOperation.UIOp (UIOP, UIOpF, _uiOp, encodeGraphViewsAsUIOp, showUIOp, toForeignUIOpF)
import AppOperation.QueryServerOp (QueryServerOpF, QUERYSERVEROP, _queryServerOp, showQueryServerOp, toForeignQueryServerOpF)
import Control.Alt ((<|>))
import Control.Monad.Except (except)
import Core (GraphId, GraphData)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.List.NonEmpty (singleton)
import Data.Traversable (traverse, foldl)
import Data.Tuple (Tuple(..))
import Data.Symbol (SProxy(..))
import Data.UUID as UUID
import Foreign (Foreign, ForeignError(..))
import Foreign.Class (class Encode, encode, class Decode, decode)
import Foreign.Generic (genericEncode, genericDecode, defaultOptions)
import Foreign.Utils (parseUUIDEither, toExceptT)
import Foreign as Foreign
import Run (Run, FProxy, Step(..))
import Run as Run


appOperationVersion :: String
appOperationVersion = "0.0.0.0.0.1"


------
-- AppOperation DSL

type AppOperationRow =
  ( graphOp       :: GRAPHOP
  , uiOp          :: UIOP
  , undoOp        :: UNDOOP
  , queryServerOp :: QUERYSERVEROP
  )

data AppOperation a = AppOperation GraphId (Run AppOperationRow a)

derive instance functorAppOperation :: Functor AppOperation

instance showAppOperation :: Show (AppOperation a) where
  show (AppOperation graphId op) = do
    show graphId
    <>
    Run.extract (op # Run.runAccumPure
      (\accumulator -> Run.match
        { graphOp       : Loop <<< lmap ((<>) (accumulator <> " ")) <<< showGraphOp
        , uiOp          : Loop <<< lmap ((<>) (accumulator <> " ")) <<< showUIOp
        , undoOp        : Loop <<< lmap ((<>) (accumulator <> " ")) <<< showUndoOp
        , queryServerOp : Loop <<< lmap ((<>) (accumulator <> " ")) <<< showQueryServerOp
        })
      (\accumulator a -> accumulator)
      "")

instance eqAppOperation :: Eq a => Eq (AppOperation a) where
  eq opA opB = show opA == show opB


------
-- UndoOp DSL adding undo-redo functionality to GraphOps

data UndoOpF next
  -- For undoing/redoing actions from the UI
  = Undo GraphId next
  | Redo GraphId next
  -- For the server to push changes to the history state to the UI
  | ConsHistory GraphId (AppOperation Unit) next
  | ConsUndone  GraphId (AppOperation Unit) next
  | SetHistory GraphId (Array (AppOperation Unit)) next
  | SetUndone  GraphId (Array (AppOperation Unit)) next

derive instance functorUndoOpF :: Functor UndoOpF

type UNDOOP = FProxy UndoOpF

_undoOp :: SProxy "undoOp"
_undoOp = SProxy

showUndoOp :: forall a. UndoOpF a -> Tuple String a
showUndoOp = case _ of
  Undo graphId next -> Tuple ("Undo " <> show graphId) next
  Redo graphId next -> Tuple ("Redo " <> show graphId) next
  ConsHistory graphId op next -> Tuple ("ConsHistory for graph: " <> show graphId <> " op: " <> show op) next
  ConsUndone  graphId op next -> Tuple ("ConsUndone  for graph: " <> show graphId <> " op: " <> show op) next
  SetHistory graphId history next -> Tuple ("Set History for graph: " <> show graphId <> " to: " <> show history) next
  SetUndone graphId undone next -> Tuple ("Set Undone for graph: " <> show graphId <> " to: " <> show undone) next


------
-- Interface

undo :: forall r. GraphId -> Run (undoOp :: UNDOOP | r) Unit
undo graphId = Run.lift _undoOp $ Undo graphId unit

redo :: forall r. GraphId -> Run (undoOp :: UNDOOP | r) Unit
redo graphId = Run.lift _undoOp $ Redo graphId unit

consHistory :: forall r. GraphId -> AppOperation Unit -> Run (undoOp :: UNDOOP | r) Unit
consHistory graphId op = Run.lift _undoOp $ ConsHistory graphId op unit

consUndone :: forall r. GraphId -> AppOperation Unit -> Run (undoOp :: UNDOOP | r) Unit
consUndone graphId op = Run.lift _undoOp $ ConsUndone graphId op unit

setHistory :: forall r. GraphId -> Array (AppOperation Unit) -> Run (undoOp :: UNDOOP | r) Unit
setHistory graphId history = Run.lift _undoOp $ SetHistory graphId history unit

setUndone :: forall r. GraphId -> Array (AppOperation Unit) -> Run (undoOp :: UNDOOP | r) Unit
setUndone graphId undone   = Run.lift _undoOp $ SetUndone  graphId undone unit

------
-- Serialisation/deserialisation for UndoOp

instance decodeUndoOpF :: Decode (UndoOpF Unit) where
  decode x = x # genericDecode defaultOptions >>= toExceptT <<< fromForeignUndoOpF

data ForeignUndoOpF
  = ForeignUndo String
  | ForeignRedo String
  | ForeignConsHistory String (AppOperation Unit)
  | ForeignConsUndone  String (AppOperation Unit)
  | ForeignSetHistory String (Array (AppOperation Unit))
  | ForeignSetUndone  String (Array (AppOperation Unit))

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
  ConsHistory graphId op next ->
    Tuple (ForeignConsHistory (UUID.toString graphId) op) next
  ConsUndone graphId op next ->
    Tuple (ForeignConsUndone (UUID.toString graphId) op) next
  SetHistory graphId history next ->
    Tuple (ForeignSetHistory (UUID.toString graphId) history) next
  SetUndone graphId undone next ->
    Tuple (ForeignSetUndone (UUID.toString graphId) undone) next

fromForeignUndoOpF :: ForeignUndoOpF -> Either String (UndoOpF Unit)
fromForeignUndoOpF = case _ of
  ForeignUndo graphIdStr -> do
    graphId <- parseUUIDEither graphIdStr
    pure $ Undo graphId unit
  ForeignRedo graphIdStr -> do
    graphId <- parseUUIDEither graphIdStr
    pure $ Redo graphId unit
  ForeignConsHistory graphIdStr op -> do
    graphId <- parseUUIDEither graphIdStr
    pure $ ConsHistory graphId op unit
  ForeignConsUndone graphIdStr op -> do
    graphId <- parseUUIDEither graphIdStr
    pure $ ConsUndone graphId op unit
  ForeignSetHistory graphIdStr history -> do
    graphId <- parseUUIDEither graphIdStr
    pure $ SetHistory graphId history unit
  ForeignSetUndone graphIdStr undone -> do
    graphId <- parseUUIDEither graphIdStr
    pure $ SetUndone graphId undone unit


------
-- Serialisation/deserialisation for AppOperation

instance encodeAppOperation' :: Encode (AppOperation Unit) where
  encode = Foreign.unsafeToForeign <<< encodeAppOperation

instance decodeAppOperation' :: Decode (AppOperation Unit) where
  decode = decodeAppOperation

encodeAppOperation :: forall a. AppOperation a -> Foreign
encodeAppOperation (AppOperation graphId op) =
  let
    foreignOp = Run.extract $
      (op # Run.runAccumPure
        (\accumulator -> Run.match
          { graphOp        : Loop <<< lmap (\encodedOp -> accumulator <> [encodedOp]) <<< toForeignGraphOpF
          , uiOp           : Loop <<< lmap (\encodedOp -> accumulator <> [encodedOp]) <<< toForeignUIOpF
          , undoOp         : Loop <<< lmap (\encodedOp -> accumulator <> [encodedOp]) <<< toForeignUndoOpF
          , queryServerOp  : Loop <<< lmap (\encodedOp -> accumulator <> [encodedOp]) <<< toForeignQueryServerOpF
          })
        (\accumulator a -> accumulator)
        [])
  in
    encode { graphIdStr : UUID.toString graphId
           , operations : foreignOp
           }

decodeAppOperation :: Foreign -> Foreign.F (AppOperation Unit)
decodeAppOperation foreignOp =
  let
    decodeUIOp          = map (Run.lift _uiOp)          <<< (decode :: Foreign -> Foreign.F (UIOpF Unit))
    decodeUndoOp        = map (Run.lift _undoOp)        <<< (decode :: Foreign -> Foreign.F (UndoOpF Unit))
    decodeGraphOp       = map (Run.lift _graphOp)       <<< (decode :: Foreign -> Foreign.F (GraphOpF Unit))
    decodeQueryServerOp = map (Run.lift _queryServerOp) <<< (decode :: Foreign -> Foreign.F (QueryServerOpF Unit))
    tryDecode op        = decodeUIOp op <|> decodeUndoOp op <|> decodeGraphOp op <|> decodeQueryServerOp op
  in do
    foreignRec <- decode foreignOp :: Foreign.F { graphIdStr :: String, operations :: Array Foreign }
    decodedOperations <- traverse tryDecode foreignRec.operations
    graphId <- except $ lmap (singleton <<< ForeignError) $ parseUUIDEither foreignRec.graphIdStr
    let operation = foldl bind (pure unit) $ map const decodedOperations
    pure $ AppOperation graphId operation

encodeGraphDataAsAppOperation :: GraphId -> GraphData -> Array (AppOperation Unit) -> Array (AppOperation Unit) -> AppOperation Unit
encodeGraphDataAsAppOperation graphId graphData history undone =
  AppOperation graphId do
    encodeGraphDataAsGraphOp graphData
    encodeGraphViewsAsUIOp graphData.panes
    setHistory graphId history
    setUndone  graphId undone
