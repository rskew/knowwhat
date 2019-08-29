module Workflow.UIGraph.ForeignUIGraphOp where

import Point2D (Point2D)
import Workflow.UIGraph (UINode, UIEdge)
import Workflow.UIGraph.ForeignUIGraph (genericEncodeOpts)
import Data.Generic.Rep (class Generic)
import Foreign.Class (class Encode, class Decode)
import Foreign.Generic (genericEncode, genericDecode)

data ForeignUIGraphOp
  = ForeignInsertNode UINode ForeignUIGraphOp
  | ForeignDeleteNode UINode ForeignUIGraphOp
  | ForeignInsertEdge UIEdge ForeignUIGraphOp
  | ForeignDeleteEdge UIEdge ForeignUIGraphOp
  | ForeignMoveNode UINode Point2D Point2D ForeignUIGraphOp
  | ForeignUpdateNodeText UINode String String ForeignUIGraphOp
  | ForeignUpdateEdgeText UIEdge String String ForeignUIGraphOp
  | ForeignLeaf

derive instance genericForeignUIGraphOp :: Generic ForeignUIGraphOp _
instance encodeForeignUIGraphOp :: Encode ForeignUIGraphOp where
  encode x = genericEncode genericEncodeOpts x
--instance decodeForeignUIGraphOp :: Decode ForeignUIGraphOp where
--  decode x = genericDecode genericEncodeOpts x
