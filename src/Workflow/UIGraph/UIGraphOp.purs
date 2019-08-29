module Workflow.UIGraph.UIGraphOp where

import Prelude

import Control.Monad.Free (Free, hoistFree, liftF, resume)
import Data.Collapsable (class Collapsable)
import Data.Either (Either(..))
import Data.Group (class Group)
import Data.Lens (traversed, (^.), (.~))
import Data.Lens.At (at)
import Data.Maybe (Maybe(..))
import Data.Monoid.Action (class Action)
import Data.Tuple (Tuple(..))
import Workflow.Core (_edgeId, _nodeId, _nodes, deleteEdge, deleteNode, insertEdge, insertNode, modifyEdge)
import Point2D (Point2D)
import Workflow.UIGraph (UIGraph, UINode, UIEdge, _pos, _nodeText, _edgeText)
import Workflow.UIGraph.ForeignUIGraph (genericEncodeOpts)
import Workflow.UIGraph.ForeignUIGraphOp (ForeignUIGraphOp(..))
import Foreign.Class (class Encode, class Decode)
import Foreign.Generic (genericEncode, genericDecode)
import Data.Generic.Rep (class Generic)


-- | Free Monad DSL
-- |
-- |Mostly for supporting Undoable

data UIGraphOpF next
  = InsertNode UINode next
  | DeleteNode UINode next
  | InsertEdge UIEdge next
  | DeleteEdge UIEdge next
  | MoveNode UINode Point2D Point2D next
  | UpdateNodeText UINode String String next
  | UpdateEdgeText UIEdge String String next
derive instance functorUIGraphOpF :: Functor UIGraphOpF

type UIGraphOpM = Free UIGraphOpF

newtype UIGraphOp a = UIGraphOp (UIGraphOpM a)

instance semigroupUIGraphOp :: Semigroup a => Semigroup (UIGraphOp a) where
  append (UIGraphOp op1) (UIGraphOp op2) = UIGraphOp $ op1 <> op2

instance monoidUIGraphOp :: Monoid a => Monoid (UIGraphOp a) where
  mempty = UIGraphOp $ mempty

-- | Group instance required by Undoable
instance groupUIGraphOp :: Monoid a => Group (UIGraphOp a) where
  ginverse (UIGraphOp op) = UIGraphOp $ (hoistFree invert) op where
    invert :: UIGraphOpF ~> UIGraphOpF
    invert op' = case op' of
      InsertNode node next             -> DeleteNode node next
      DeleteNode node next             -> InsertNode node next
      InsertEdge edge next             -> DeleteEdge edge next
      DeleteEdge edge next             -> InsertEdge edge next
      MoveNode node from to next       -> MoveNode node to from next
      UpdateNodeText node from to next -> UpdateNodeText node to from next
      UpdateEdgeText edge from to next -> UpdateEdgeText edge to from next

-- | Collapsable instance required by Undoable
instance collapsableUIGraphOp :: Collapsable (UIGraphOp Unit) where
  collapse (UIGraphOp nextOp) (UIGraphOp prevOp) =
    case Tuple (resume nextOp) (resume prevOp) of
      Tuple (Left (MoveNode nextNode  middlePos lastPos    nextNext))
            (Left (MoveNode firstNode firstPos  middlePos' prevNext)) ->
        if nextNode ^. _nodeId == firstNode ^. _nodeId
           &&    middlePos == middlePos'
        then Just $ UIGraphOp $ liftF $ MoveNode firstNode firstPos lastPos unit
        else Nothing
      _ -> Nothing


------
-- Interpreter

interpretUIGraphOp :: forall a. UIGraphOp a -> (UIGraph -> UIGraph)
interpretUIGraphOp (UIGraphOp op) = case resume op of
  Right _ -> identity

  Left (InsertNode node next) ->
    (insertNode node)
    >>> (interpretUIGraphOp $ UIGraphOp next)

  Left (DeleteNode node next) ->
    (deleteNode node)
    >>> (interpretUIGraphOp $ UIGraphOp next)

  Left (InsertEdge edge next) ->
    (insertEdge edge)
    >>> (interpretUIGraphOp $ UIGraphOp next)

  Left (DeleteEdge edge next) ->
    (deleteEdge edge)
    >>> (interpretUIGraphOp $ UIGraphOp next)

  Left (MoveNode node from to next) ->
    (_nodes <<< at (node ^. _nodeId) <<< traversed <<< _pos .~ to)
    >>> (interpretUIGraphOp $ UIGraphOp next)

  Left (UpdateNodeText node from to next) ->
    (_nodes <<< at (node ^. _nodeId) <<< traversed <<< _nodeText .~ to)
    >>> (interpretUIGraphOp $ UIGraphOp next)

  Left (UpdateEdgeText edge from to next) ->
    (modifyEdge (edge ^. _edgeId) (_edgeText .~ to))
    >>> (interpretUIGraphOp $ UIGraphOp next)

-- | Action instance required by Undoable
instance actUIGraphOpUIGraph :: Monoid a => Action (UIGraphOp a) UIGraph where
  act op = interpretUIGraphOp op


------
-- Serialising Interpreter

toForeign :: forall a. UIGraphOp a -> ForeignUIGraphOp
toForeign (UIGraphOp op) = case resume op of
  Right _ -> ForeignLeaf

  Left (InsertNode node next) ->
    ForeignInsertNode node             $ toForeign $ UIGraphOp next

  Left (DeleteNode node next) ->
    ForeignDeleteNode node             $ toForeign $ UIGraphOp next

  Left (InsertEdge edge next) ->
    ForeignInsertEdge edge             $ toForeign $ UIGraphOp next

  Left (DeleteEdge edge next) ->
    ForeignDeleteEdge edge             $ toForeign $ UIGraphOp next

  Left (MoveNode node from to next) ->
    ForeignMoveNode node from to       $ toForeign $ UIGraphOp next

  Left (UpdateNodeText node from to next) ->
    ForeignUpdateNodeText node from to $ toForeign $ UIGraphOp next

  Left (UpdateEdgeText edge from to next) ->
    ForeignUpdateEdgeText edge from to $ toForeign $ UIGraphOp next

derive instance genericUIGraphOp :: Generic a z => Generic (UIGraphOp a) _
instance encodeUIGraphOp :: (Generic a z, Encode a) => Encode (UIGraphOp a) where
  encode x = x # toForeign >>> genericEncode genericEncodeOpts

------
-- Interface

insertNodeOp :: UINode -> UIGraphOp Unit
insertNodeOp node = UIGraphOp $ liftF $
                    InsertNode node unit

deleteNodeOp :: UINode -> UIGraphOp Unit
deleteNodeOp node = UIGraphOp $ liftF $
                    DeleteNode node unit

insertEdgeOp :: UIEdge -> UIGraphOp Unit
insertEdgeOp edge = UIGraphOp $ liftF $
                    InsertEdge edge unit

deleteEdgeOp :: UIEdge -> UIGraphOp Unit
deleteEdgeOp edge = UIGraphOp $ liftF $
                    DeleteEdge edge unit

moveNodeOp :: UINode -> Point2D -> UIGraphOp Unit
moveNodeOp node newPos = UIGraphOp $ liftF $
                       MoveNode node (node ^. _pos) newPos unit

updateNodeTextOp :: UINode -> String -> UIGraphOp Unit
updateNodeTextOp node newText = UIGraphOp $ liftF $
                              UpdateNodeText node (node ^. _nodeText) newText unit

updateEdgeTextOp :: UIEdge -> String -> UIGraphOp Unit
updateEdgeTextOp edge newText = UIGraphOp $ liftF $
                              UpdateEdgeText edge (edge ^. _edgeText) newText unit
