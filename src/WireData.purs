module WireData where

import Prelude

import Data.DateTime.Instant (Instant)
import Config as Config
import Effect (Effect)
import Effect.Now (now)
import Foreign (F, ForeignError(..), fail)
import Foreign.Generic (encodeJSON, decodeJSON)
import MegagraphStateUpdate (MegagraphStateUpdate)

type Metadata =
  { version   :: String
  , timestamp :: Instant
  }

type WireDataRaw =
  { op       :: String
  , metadata :: Metadata
  }

type WireData =
  { op       :: Array MegagraphStateUpdate
  , metadata :: Metadata
  }

encodeWireData :: Array MegagraphStateUpdate -> Effect WireDataRaw
encodeWireData op = do
  timestamp <- now
  pure { op : encodeJSON op
       , metadata : { version : Config.version
                    , timestamp : timestamp
                    }
       }

-- | Allow parsing values of different versions by switching the parser of the
-- | string blob based on the version value.
decodeWireData :: WireDataRaw -> F WireData
decodeWireData wireDataRaw = case wireDataRaw.metadata.version of
  "0.0.0.0.0.0.1" -> do
    decodedOp <- decodeJSON wireDataRaw.op
    pure $ wireDataRaw { op = decodedOp }
  unknown -> fail $ ForeignError $ "Could not decode version: " <> unknown
