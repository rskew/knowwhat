module WireData where

import Prelude

import Config as Config
import Control.Monad.Except (except)
import Data.Bifunctor (bimap)
import Data.DateTime.Instant (Instant, fromDateTime, toDateTime)
import Data.Formatter.DateTime (FormatterCommand(..), format, unformat)
import Data.List.Types (NonEmptyList(..))
import Data.List as List
import Data.NonEmpty (singleton)
import Effect (Effect)
import Effect.Now (now)
import Foreign (F, ForeignError(..), fail)
import Foreign.Generic (encodeJSON, decodeJSON)
import MegagraphStateUpdate (MegagraphStateUpdate)

type Metadata =
  { version   :: String
  , timestamp :: String
  }

type WireDataRaw =
  { op        :: String
  , version   :: String
  , timestamp :: String
  }

type WireData =
  { op        :: Array MegagraphStateUpdate
  , version   :: String
  , timestamp :: Instant
  }

encodeWireData :: Array MegagraphStateUpdate -> Effect WireDataRaw
encodeWireData op = do
  timestamp <- now
  pure { op : encodeJSON op
       , version : Config.version
       , timestamp : format (List.singleton UnixTimestamp) $ toDateTime timestamp
       }

-- | Allow parsing values of different versions by switching the parser of the
-- | string blob based on the version value.
decodeWireData :: WireDataRaw -> F WireData
decodeWireData wireDataRaw = case wireDataRaw.version of
  "0.0.0.0.0.0.1" -> do
    decodedOp <- decodeJSON wireDataRaw.op
    timestamp <- wireDataRaw.timestamp
                 # unformat (List.singleton UnixTimestamp)
                 # bimap (NonEmptyList <<< singleton <<< ForeignError) fromDateTime
                 # except
    pure $ wireDataRaw {op = decodedOp, timestamp = timestamp}
  unknown -> fail $ ForeignError $ "Could not decode version: " <> unknown
