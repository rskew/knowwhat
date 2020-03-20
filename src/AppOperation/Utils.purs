module AppOperation.Utils where

import Prelude

import Megagraph (Edge, GraphId, Node)
import MegagraphOperation (MegagraphUpdate, encodeEdgesAsMegagraphUpdate, encodeNodesAsMegagraphUpdate, invertMegagraphUpdate)

------
-- Delete helpers

removeEdgesOp :: GraphId -> Array Edge -> MegagraphUpdate
removeEdgesOp graphId = invertMegagraphUpdate <<< encodeEdgesAsMegagraphUpdate graphId

removeNodesOp :: GraphId -> Array Node -> MegagraphUpdate
removeNodesOp graphId = invertMegagraphUpdate <<< encodeNodesAsMegagraphUpdate graphId
