module Workflow.UIGraph.UIGraphOp where

import Prelude

import Run (Run, FProxy, Step(..))
import Run as Run
import Data.Bifunctor (lmap)
import Data.Generic.Rep (class Generic)
import Data.Lens (traversed, (^.), (.~))
import Data.Lens.At (at)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Foreign (Foreign)
import Foreign.Unit (ForeignUnit(..))
import Workflow.Core (_edgeId, _nodeId, _nodes, deleteEdge, deleteNode, insertEdge, insertNode, modifyEdge)
import Point2D (Point2D)
import Workflow.UIGraph.Types (UIGraph, UINode, UIEdge, _pos, _nodeText, _edgeText)
import Foreign.Class (class Encode, class Decode)
import Foreign.Generic (genericEncode, genericDecode, defaultOptions)


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

type UIGRAPHOP = FProxy UIGraphOpF

_uiGraphOp :: SProxy "uiGraphOp"
_uiGraphOp = SProxy

invertUIGraphOp :: UIGraphOpF ~> UIGraphOpF
invertUIGraphOp = case _ of
  InsertNode node next             -> DeleteNode node next
  DeleteNode node next             -> InsertNode node next
  InsertEdge edge next             -> DeleteEdge edge next
  DeleteEdge edge next             -> InsertEdge edge next
  MoveNode node from to next       -> MoveNode node to from next
  UpdateNodeText node from to next -> UpdateNodeText node to from next
  UpdateEdgeText edge from to next -> UpdateEdgeText edge to from next

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

handleUIGraphOp :: forall a. UIGraphOpF a -> Tuple (UIGraph -> UIGraph) a
handleUIGraphOp = case _ of
  InsertNode node next ->
    Tuple (insertNode node) next

  DeleteNode node next -> Tuple (deleteNode node) next

  InsertEdge edge next -> Tuple (insertEdge edge) next

  DeleteEdge edge next -> Tuple (deleteEdge edge) next

  MoveNode node from to next -> Tuple
    (_nodes <<< at (node ^. _nodeId) <<< traversed <<< _pos .~ to)
    next

  UpdateNodeText node from to next -> Tuple
    (_nodes <<< at (node ^. _nodeId) <<< traversed <<< _nodeText .~ to)
    next

  UpdateEdgeText edge from to next -> Tuple
    (modifyEdge (edge ^. _edgeId) (_edgeText .~ to))
    next

interpretUIGraphOp :: forall r a. Run (uiGraphOp :: UIGRAPHOP | r) a -> Run r (UIGraph -> UIGraph)
interpretUIGraphOp =
  Run.runAccumPure
  (\accumulator -> Run.on _uiGraphOp (Loop <<< handleUIGraphOp) Done)
  (\accumulator a -> accumulator)
  identity


------
-- Printing interpreter

showUIGraphOp :: forall a. UIGraphOpF a -> Tuple String a
showUIGraphOp = case _  of
  InsertNode node next -> Tuple ("InsertNode " <> show (node ^. _nodeId) <> " >>= ") next
  DeleteNode node next -> Tuple ("DeleteNode " <> show (node ^. _nodeId) <> " >>= ") next
  InsertEdge edge next -> Tuple ("InsertEdge " <> show (edge ^. _edgeId) <> " >>= ") next
  DeleteEdge edge next -> Tuple ("DeleteEdge " <> show (edge ^. _edgeId) <> " >>= ") next
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
  InsertNode node next -> Tuple (InsertNode node ForeignUnit) next
  DeleteNode node next -> Tuple (DeleteNode node ForeignUnit) next
  InsertEdge edge next -> Tuple (InsertEdge edge ForeignUnit) next
  DeleteEdge edge next -> Tuple (DeleteEdge edge ForeignUnit) next
  MoveNode node from to next -> Tuple (MoveNode node from to ForeignUnit) next
  UpdateNodeText node from to next -> Tuple (UpdateNodeText node from to ForeignUnit) next
  UpdateEdgeText edge from to next -> Tuple (UpdateEdgeText edge from to ForeignUnit) next


--------
---- Interface

insertNodeOp :: forall r. UINode -> Run (uiGraphOp :: UIGRAPHOP | r) Unit
insertNodeOp node = Run.lift _uiGraphOp $
                    InsertNode node unit

deleteNodeOp :: forall r. UINode -> Run (uiGraphOp :: UIGRAPHOP | r) Unit
deleteNodeOp node = Run.lift _uiGraphOp $
                    DeleteNode node unit

insertEdgeOp :: forall r. UIEdge -> Run (uiGraphOp :: UIGRAPHOP | r) Unit
insertEdgeOp edge = Run.lift _uiGraphOp $
                    InsertEdge edge unit

deleteEdgeOp :: forall r. UIEdge -> Run (uiGraphOp :: UIGRAPHOP | r) Unit
deleteEdgeOp edge = Run.lift _uiGraphOp $
                    DeleteEdge edge unit

moveNodeOp :: forall r. UINode -> Point2D -> Run (uiGraphOp :: UIGRAPHOP | r) Unit
moveNodeOp node newPos = Run.lift _uiGraphOp $
                         MoveNode node (node ^. _pos) newPos unit

updateNodeTextOp :: forall r. UINode -> String -> Run (uiGraphOp :: UIGRAPHOP | r) Unit
updateNodeTextOp node newText = Run.lift _uiGraphOp $
                                UpdateNodeText node (node ^. _nodeText) newText unit

updateEdgeTextOp :: forall r. UIEdge -> String -> Run (uiGraphOp :: UIGRAPHOP | r) Unit
updateEdgeTextOp edge newText = Run.lift _uiGraphOp $
                                UpdateEdgeText edge (edge ^. _edgeText) newText unit


------
-- Free monad implementation
--
-- Must be defined here to define Encode and Decode instances for UIGraphOp

--data Free f a
--  = Pure a
--  | Free (f (Free f a))
--
--instance functorFree :: Functor f => Functor (Free f) where
--  map g (Pure a) = Pure $ g a
--  map g (Free op) = Free $ map (map g) op
--
--instance applicativeFree :: Functor f => Applicative (Free f) where
--  pure = Pure
--
--instance applyFree :: Functor f => Apply (Free f) where
--  apply (Pure g) (Pure a) = Pure $ g a
--  apply (Pure g) (Free bs) = map g (Free bs)
--  apply (Free gs) bs = Free $ (\g -> apply g bs) <$> gs
--
--instance bindFree :: Functor f => Bind (Free f) where
--  bind :: forall f a b. Functor f => Free f a -> (a -> Free f b) -> Free f b
--  bind (Pure a) g = g a
--  bind (Free op) g = Free $ map (flip bind g) op
--
--instance monadFree :: Functor f => Monad (Free f)
--
--instance semigroupFree :: Functor f => Semigroup (Free f a) where
--  append op1 op2 = op1 >>= \_ -> op2
--
--instance monoidFree :: (Functor f, Monoid a) => Monoid (Free f a) where
--  mempty = Pure mempty
--
--liftF :: forall f. Functor f => f ~> Free f
--liftF f = Free $ map Pure f
--
--hoistFree :: forall f g. Functor f => Functor g =>
--             (f ~> g) -> (Free f ~> Free g)
--hoistFree nat = case _ of
--  Pure a -> Pure a
--  Free f -> Free $ nat f <#> hoistFree nat
