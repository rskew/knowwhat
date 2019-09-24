module Workflow.UIGraph.UIGraphOp where

import Prelude

import Data.Bifunctor (lmap)
import Data.Generic.Rep (class Generic)
import Data.Lens (traversed, (^.), (.~))
import Data.Lens.At (at)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Foreign (Foreign)
import Foreign.Class (class Encode, class Decode)
import Foreign.Generic (genericEncode, genericDecode, defaultOptions)
import Foreign.Unit (ForeignUnit(..))
import Point2D (Point2D)
import Run (Run, FProxy)
import Run as Run
import Workflow.Core (Graph, Node, Edge, _pos, _nodeText, _edgeText, _edgeId, _nodeId, _nodes)
import Workflow.Graph (modifyEdge)


-- | Free Monad DSL
-- |
-- |Mostly for supporting Undoable

data UIGraphOpF next
  = MoveNode Node Point2D Point2D next
  | UpdateNodeText Node String String next
  | UpdateEdgeText Edge String String next
derive instance functorUIGraphOpF :: Functor UIGraphOpF

type UIGRAPHOP = FProxy UIGraphOpF

_uiGraphOp :: SProxy "uiGraphOp"
_uiGraphOp = SProxy

invertUIGraphOp :: UIGraphOpF ~> UIGraphOpF
invertUIGraphOp = case _ of
  MoveNode node from to next         -> MoveNode node to from next
  UpdateNodeText node from to next   -> UpdateNodeText node to from next
  UpdateEdgeText edge from to next   -> UpdateEdgeText edge to from next

-- | Assume that operations being collapsed are single ops, not compound
collapseUIGraphOp :: forall a. UIGraphOpF a -> UIGraphOpF a -> Maybe (UIGraphOpF Unit)
collapseUIGraphOp nextOp prevOp =
  case Tuple nextOp prevOp of
    Tuple (MoveNode nextNode  middlePos lastPos    nextNext)
          (MoveNode firstNode firstPos  middlePos' prevNext) ->
      if nextNode ^. _nodeId == firstNode ^. _nodeId
         &&        middlePos == middlePos'
      then Just $ MoveNode firstNode firstPos lastPos unit
      else Nothing
    Tuple (UpdateNodeText nextNode  middleText lastText    nextNext)
          (UpdateNodeText firstNode firstText  middleText' prevNext) ->
      if nextNode ^. _nodeId == firstNode ^. _nodeId
         &&       middleText == middleText'
      then Just $ UpdateNodeText firstNode firstText lastText unit
      else Nothing
    Tuple (UpdateEdgeText nextEdge  middleText lastText    nextNext)
          (UpdateEdgeText firstEdge firstText  middleText' prevNext) ->
      if nextEdge ^. _edgeId == firstEdge ^. _edgeId
         &&       middleText == middleText'
      then Just $ UpdateEdgeText firstEdge firstText lastText unit
      else Nothing
    _ -> Nothing


------
-- Main interpreter

handleUIGraphOp :: forall a. UIGraphOpF a -> Tuple (Graph -> Graph) a
handleUIGraphOp = case _ of
  MoveNode node from to next -> Tuple
    (_nodes <<< at (node ^. _nodeId) <<< traversed <<< _pos .~ to)
    next

  UpdateNodeText node from to next -> Tuple
    (_nodes <<< at (node ^. _nodeId) <<< traversed <<< _nodeText .~ to)
    next

  UpdateEdgeText edge from to next -> Tuple
    (modifyEdge (edge ^. _edgeId) (_edgeText .~ to))
    next


------
-- Printing interpreter

showUIGraphOp :: forall a. UIGraphOpF a -> Tuple String a
showUIGraphOp = case _  of
  MoveNode node from to next -> Tuple ("MoveNode " <> show (node ^. _nodeId) <> " from " <> show from <> " to " <> show to <> " >>= ") next
  UpdateNodeText node from to next -> Tuple ("UpdateNodeText " <> show (node ^. _nodeId) <> " from \"" <> from <> "\" to \"" <> to <> "\" >>= ") next
  UpdateEdgeText edge from to next -> Tuple ("UpdateEdgeText " <> show (edge ^. _edgeId) <> " from \"" <> from <> "\" to \"" <> to <> "\" >>= ") next


--------
---- Serialisation/deserialisation

derive instance genericUIGraphOpF :: (Generic a z) => Generic (UIGraphOpF a) _
instance encodeUIGraphOpF' :: (Generic a z, Encode a) => Encode (UIGraphOpF a) where
  encode x = x # genericEncode defaultOptions
instance decodeUIGraphOpF' :: (Generic a z, Decode a) => Decode (UIGraphOpF a) where
  decode x = x # genericDecode defaultOptions

encodeUIGraphOpF :: forall a. UIGraphOpF a -> Tuple Foreign a
encodeUIGraphOpF = lmap (genericEncode defaultOptions) <<< case _ of
  MoveNode node from to next -> Tuple (MoveNode node from to ForeignUnit) next
  UpdateNodeText node from to next -> Tuple (UpdateNodeText node from to ForeignUnit) next
  UpdateEdgeText edge from to next -> Tuple (UpdateEdgeText edge from to ForeignUnit) next


--------
---- Interface

moveNodeOp :: forall r. Node -> Point2D -> Run (uiGraphOp :: UIGRAPHOP | r) Unit
moveNodeOp node newPos = Run.lift _uiGraphOp $
                         MoveNode node (node ^. _pos) newPos unit

updateNodeTextOp :: forall r. Node -> String -> Run (uiGraphOp :: UIGRAPHOP | r) Unit
updateNodeTextOp node newText = Run.lift _uiGraphOp $
                                UpdateNodeText node (node ^. _nodeText) newText unit

updateEdgeTextOp :: forall r. Edge -> String -> Run (uiGraphOp :: UIGRAPHOP | r) Unit
updateEdgeTextOp edge newText = Run.lift _uiGraphOp $
                                UpdateEdgeText edge (edge ^. _edgeText) newText unit
