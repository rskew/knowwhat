module WireData where

import Prelude

import AppOperation (AppOperation)
import Data.DateTime.Instant (Instant)
import Effect.Now (now)
import Effect (Effect)
import Foreign (F, ForeignError(..), fail)
import Foreign.Generic (encodeJSON, decodeJSON)

-- TODO get from Config
version :: String
version = "0.0.0.0.0.0.1"

type Metadata =
  { version   :: String
  , timestamp :: Instant
  }

type WireDataRaw =
  { op       :: String
  , metadata :: Metadata
  }

type WireData =
  { op       :: AppOperation
  , metadata :: Metadata
  }

encodeWireData :: AppOperation -> Effect WireDataRaw
encodeWireData op = do
  timestamp <- now
  pure { op : encodeJSON op
       , metadata : { version : version
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
