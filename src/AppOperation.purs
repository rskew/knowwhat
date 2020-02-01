module AppOperation where

import Prelude

import AppState (GraphState, MappingState, MegagraphState)
import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Foreign.Class (class Encode, class Decode)
import Foreign.Generic (genericEncode, genericDecode, defaultOptions)
import Megagraph (GraphId, MappingId)
import MegagraphOperation (MegagraphUpdate, encodeGraphAsMegagraphUpdate, encodeMappingAsMegagraphUpdate)


data MegagraphElement
  = GraphElement GraphId
  | MappingElement MappingId GraphId GraphId

data HistoryUpdate
  = Insert MegagraphUpdate
  | Pop
  | Replace (Array MegagraphUpdate)
  | NoOp

-- | The AppOperation represents the data needed to update the megagraph and its
-- | history in response to a high-level interaction, e.g. deleting a node
-- | or undoing the last operation.
-- | This type allows flexibly defining how an interaction affects the state, as
-- | these may involve many low-level graph and/or mapping
-- | operations, and some operations may not be stored in the history if they are
-- | not to be undone (e.g. panning around, modifying the graph origin).
newtype AppOperation
  = AppOperation
    { target        :: MegagraphElement
    , op            :: MegagraphUpdate
    , historyUpdate :: HistoryUpdate
    , undoneUpdate  :: HistoryUpdate
    }

derive instance genericMegagraphComponent :: Generic MegagraphElement _
instance decodeMegagraphComponent :: Decode MegagraphElement where
  decode = genericDecode defaultOptions
instance encodeMegagraphComponent :: Encode MegagraphElement where
  encode = genericEncode defaultOptions

derive instance genericHistoryUpdate :: Generic HistoryUpdate _
instance decodeHistoryUpdate :: Decode HistoryUpdate where
  decode = genericDecode defaultOptions
instance encodeHistoryUpdate :: Encode HistoryUpdate where
  encode = genericEncode defaultOptions

derive instance genericAppOperation :: Generic AppOperation _
instance decodeAppOperation :: Decode AppOperation where
  decode = genericDecode defaultOptions
instance encodeAppOperation :: Encode AppOperation where
  encode = genericEncode defaultOptions

instance showMegagraphComponent :: Show MegagraphElement where
  show = case _ of
    GraphElement graphId -> "GraphComponent " <> show graphId
    MappingElement mappingId from to -> "MappingComponent "
                                        <> show mappingId
                                        <> " from: " <> show from
                                        <> " to: " <> show to

instance showHistoryUpdate :: Show HistoryUpdate where
  show = case _ of
    Insert op -> "Insert " <> show op
    Pop -> "Pop"
    Replace megagraphUpdate -> "Replace " <> show megagraphUpdate
    NoOp -> "NoOp"

instance showAppOperation :: Show AppOperation where
  show (AppOperation {target, op, historyUpdate, undoneUpdate}) =
    "AppOperation with target: " <> show target
       <> ", op: " <> show op
       <> ", historyUpdate: " <> show historyUpdate
       <> ", undoneUpdate: " <> show undoneUpdate

encodeGraphStateAsAppOperation :: GraphState -> AppOperation
encodeGraphStateAsAppOperation graphState =
  AppOperation { target : GraphElement graphState.graph.id
               , op : encodeGraphAsMegagraphUpdate graphState.graph
               , historyUpdate : Replace graphState.history
               , undoneUpdate : Replace graphState.undone
               }

encodeMappingStateAsAppOperation :: MappingState -> AppOperation
encodeMappingStateAsAppOperation mappingState =
  AppOperation { target : MappingElement
                            mappingState.mapping.id
                            mappingState.mapping.sourceGraph
                            mappingState.mapping.targetGraph
               , op : encodeMappingAsMegagraphUpdate mappingState.mapping
               , historyUpdate : Replace mappingState.history
               , undoneUpdate : Replace mappingState.undone
               }

encodeMegagraphStateAsAppOperations :: MegagraphState -> Array AppOperation
encodeMegagraphStateAsAppOperations megagraphState =
  (megagraphState.graphs # Map.values >>> Array.fromFoldable >>> map encodeGraphStateAsAppOperation)
  <>
  (megagraphState.mappings # Map.values >>> Array.fromFoldable >>> map encodeMappingStateAsAppOperation)
