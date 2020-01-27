module UIOperation where

import Prelude

import AppState
import Graph
import GraphOperation


data MegagraphComponent
  = GraphComponent GraphId
  | MappingComponent MappingId

data HistoryUpdate = Insert | Pop | NoOp

-- | The UIOperation represents the data needed to update the megagraph and its
-- | history in response to a high-level interaction, e.g. deleting a node
-- | or undoing the last operation.
-- | This type allows flexibly defining how an interaction affects the state, as
-- | these may involve many low-level graph/mapping
-- | operations, and some operations may not be stored in the history if they are
-- | not to be undone.
type UIOperation
  = { target :: MegagraphComponent
    , op :: SubMegagraphUpdate
    , historyUpdate :: HistoryUpdate
    , undoneUpdate :: HistoryUpdate
    }
