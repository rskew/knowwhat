module AppState.Foreign where

import Prelude

import AppOperation (AppOperation)
import AppOperation.Interpreter (encodeGraphDataAsAppOperation)
import AppState (AppState)
import Core (GraphId)
import Data.DateTime.Instant (Instant, instant, unInstant)
import Data.Either (note)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Time.Duration (Milliseconds(..))
import Data.UUID as UUID
import Foreign as Foreign
import Foreign.Generic (decodeJSON, encodeJSON)
import Foreign.Utils (parseUUIDEither, toExceptT)


type Metadata =
  { version   :: String
  , timestamp :: Instant
  }

type ForeignMetadata =
  { version   :: String
  , timestamp :: Number
  }

type SerialisedGraphData =
  { appStateOp :: AppOperation Unit
  , metadata   :: ForeignMetadata
  , graphId    :: String
  , history    :: Array (AppOperation Unit)
  , undone     :: Array (AppOperation Unit)
  }

type DeserialisedGraphData =
  { appStateOp :: AppOperation Unit
  , metadata   :: Metadata
  , graphId    :: GraphId
  , history    :: Array (AppOperation Unit)
  , undone     :: Array (AppOperation Unit)
  }

toForeignMetadata :: Metadata -> ForeignMetadata
toForeignMetadata metadata =
  let
    Milliseconds millis = unInstant metadata.timestamp
  in
    metadata { timestamp = millis }

graphDataToJSON :: GraphId -> AppState -> Metadata -> Maybe String
graphDataToJSON graphId appState metadata = do
  history <- Map.lookup graphId appState.history
  undone  <- Map.lookup graphId appState.undone
  let serialisableGraphData =
        ({ appStateOp : encodeGraphDataAsAppOperation appState.graphData appState.synth.synthParams
         , metadata   : toForeignMetadata metadata
         , graphId    : UUID.toString graphId
         , history    : history
         , undone     : undone
         } :: SerialisedGraphData)
  pure $ encodeJSON serialisableGraphData

fromForeignMetadata :: ForeignMetadata -> Foreign.F Metadata
fromForeignMetadata foreignMeta = toExceptT do
  timestamp <- note "Failed to convert timestamp from foreign"
               $ instant $ Milliseconds foreignMeta.timestamp
  pure $ foreignMeta { timestamp = timestamp }

graphDataFromJSON :: AppState -> String -> Foreign.F DeserialisedGraphData
graphDataFromJSON oldAppState json = do
  decoded  <- (decodeJSON json :: Foreign.F SerialisedGraphData)
  metadata <- fromForeignMetadata decoded.metadata
  graphId  <- toExceptT $ parseUUIDEither decoded.graphId
  pure $ decoded { metadata = metadata
                 , graphId  = graphId
                 }
