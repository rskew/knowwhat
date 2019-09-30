module AppOperation where

import Prelude

import AppOperation.GraphOp (GraphOpF, _graphOp, GRAPHOP, showGraphOp, toForeignGraphOpF)
import AppOperation.UIOp (UIOpF, _uiOp, UIOP, showUIOp, toForeignUIOpF)
import AppOperation.UndoOp (UndoOpF, _undoOp, UNDOOP, showUndoOp, toForeignUndoOpF)
import Control.Alt ((<|>))
import Data.Bifunctor (lmap)
import Data.Newtype (class Newtype, wrap)
import Data.Traversable (traverse, foldl)
import Foreign (Foreign)
import Foreign as Foreign
import Foreign.Class (class Encode, class Decode, decode)
import Run (Run, Step(..))
import Run as Run
import Synth.SynthOp (SynthOpF, _synthOp, SYNTHOP, showSynthOp, toForeignSynthOpF)


appOperationVersion :: String
appOperationVersion = "0.0.0.0.0.1"

------
-- AppOperation DSL for actions that change the graph state and
-- associated state (e.g. synth state) directly

type AppOperationRow =
  ( graphOp :: GRAPHOP
  , uiOp    :: UIOP
  , undoOp  :: UNDOOP
  , synthOp :: SYNTHOP
  )

newtype AppOperation a = AppOperation (Run AppOperationRow a)

derive instance newtypeAppOperation :: Newtype (AppOperation a) _

derive instance functorAppOperation :: Functor AppOperation

instance showAppOperation :: Show (AppOperation a) where
  show (AppOperation op) =
    Run.extract (op # Run.runAccumPure
      (\accumulator -> Run.match
        { graphOp : Loop <<< lmap ((<>) (accumulator <> " ")) <<< showGraphOp
        , synthOp : Loop <<< lmap ((<>) (accumulator <> " ")) <<< showSynthOp
        , uiOp    : Loop <<< lmap ((<>) (accumulator <> " ")) <<< showUIOp
        , undoOp  : Loop <<< lmap ((<>) (accumulator <> " ")) <<< showUndoOp
        })
      (\accumulator a -> accumulator)
      "")

instance eqAppOperation :: Eq a => Eq (AppOperation a) where
  eq opA opB = show opA == show opB

------
-- Serialisation/deserialisation

instance encodeAppOperation' :: Encode (AppOperation Unit) where
  encode = Foreign.unsafeToForeign <<< encodeAppOperation

instance decodeAppOperation' :: Decode (AppOperation Unit) where
  decode = decodeAppOperation

encodeAppOperation :: forall a. AppOperation a -> Array Foreign
encodeAppOperation (AppOperation op) =
  Run.extract $
  (op # Run.runAccumPure
    (\accumulator -> Run.match
      { synthOp : Loop <<< lmap (\encodedOp -> accumulator <> [encodedOp]) <<< toForeignSynthOpF
      , graphOp : Loop <<< lmap (\encodedOp -> accumulator <> [encodedOp]) <<< toForeignGraphOpF
      , uiOp    : Loop <<< lmap (\encodedOp -> accumulator <> [encodedOp]) <<< toForeignUIOpF
      , undoOp  : Loop <<< lmap (\encodedOp -> accumulator <> [encodedOp]) <<< toForeignUndoOpF
      })
    (\accumulator a -> accumulator)
    [])

decodeAppOperation :: Foreign -> Foreign.F (AppOperation Unit)
decodeAppOperation foreignOpArray =
  let
    decodeUIOp    = map (Run.lift _uiOp)    <<< (decode :: Foreign -> Foreign.F (UIOpF Unit))
    decodeUndoOp  = map (Run.lift _undoOp)  <<< (decode :: Foreign -> Foreign.F (UndoOpF Unit))
    decodeSynthOp = map (Run.lift _synthOp) <<< (decode :: Foreign -> Foreign.F (SynthOpF Unit))
    decodeGraphOp = map (Run.lift _graphOp) <<< (decode :: Foreign -> Foreign.F (GraphOpF Unit))
    tryDecode op  = decodeSynthOp op <|> decodeUIOp op <|> decodeUndoOp op <|> decodeGraphOp op
  in do
    arrayForeign <- Foreign.readArray foreignOpArray
    decodedOperations <- traverse tryDecode arrayForeign
    pure $ wrap
      $ foldl bind (pure unit) $ map const decodedOperations
