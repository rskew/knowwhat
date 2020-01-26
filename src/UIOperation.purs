module UIOperation where

import Prelude

import AppState
import Graph
import GraphOperation


data SubMegagraphUpdate
  = SubGraphUpdate GraphId GraphUpdate
  | SubMappingUpdate MappingId GraphUpdate

data HistoryUpdate
  = Insert SubMegagraphUpdate
  | Pop
  | NoOp

type UIOperation
  = { op :: SubMegagraphUpdate
    , historyUpdate :: HistoryUpdate
    , undoneUpdate :: HistoryUpdate
    }
