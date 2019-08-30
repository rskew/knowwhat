module Workflow.UIGraph.UIGraphOp where

import Prelude

import Data.Collapsable (class Collapsable)
import Data.Generic.Rep (class Generic)
import Data.Group (class Group)
import Data.Lens (traversed, (^.), (.~))
import Data.Lens.At (at)
import Data.Maybe (Maybe(..))
import Data.Monoid.Action (class Action)
import Data.Tuple (Tuple(..))
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

newtype UIGraphOp a = UIGraphOp (Free UIGraphOpF a)

derive newtype instance functorUIGraphOp :: Functor UIGraphOp
derive newtype instance applyUIGraphOp :: Apply UIGraphOp
derive newtype instance applicativeUIGraphOp :: Applicative UIGraphOp
derive newtype instance bindUIGraphOp :: Bind UIGraphOp
derive newtype instance monadUIGraphOp :: Monad UIGraphOp

derive newtype instance semigroupUIGraphOp :: Semigroup (UIGraphOp a)
derive newtype instance monoidUIGraphOp :: Monoid a => Monoid (UIGraphOp a)

-- | Group instance required by Undoable
instance groupUIGraphOp :: (Monoid a) => Group (UIGraphOp a) where
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
    case Tuple nextOp prevOp of
      Tuple (Free (MoveNode nextNode  middlePos lastPos    nextNext))
            (Free (MoveNode firstNode firstPos  middlePos' prevNext)) ->
        if nextNode ^. _nodeId == firstNode ^. _nodeId
           &&        middlePos == middlePos'
        then Just $ UIGraphOp $ liftF $ MoveNode firstNode firstPos lastPos unit
        else Nothing
      Tuple (Free (UpdateNodeText nextNode  middleText lastText    nextNext))
            (Free (UpdateNodeText firstNode firstText  middleText' prevNext)) ->
        if nextNode ^. _nodeId == firstNode ^. _nodeId
           &&       middleText == middleText'
        then Just $ UIGraphOp $ liftF $ UpdateNodeText firstNode firstText lastText unit
        else Nothing
      Tuple (Free (UpdateEdgeText nextEdge  middleText lastText    nextNext))
            (Free (UpdateEdgeText firstEdge firstText  middleText' prevNext)) ->
        if nextEdge ^. _edgeId == firstEdge ^. _edgeId
           &&       middleText == middleText'
        then Just $ UIGraphOp $ liftF $ UpdateEdgeText firstEdge firstText lastText unit
        else Nothing
      _ -> Nothing


------
-- Main interpreter

interpretUIGraphOp :: forall a. UIGraphOp a -> (UIGraph -> UIGraph)
interpretUIGraphOp (UIGraphOp op) = interpretUIGraphOpM op where
  interpretUIGraphOpM = case _ of
    Pure _ -> identity

    Free (InsertNode node next) ->
      (insertNode node)
      >>> (interpretUIGraphOpM next)

    Free (DeleteNode node next) ->
      (deleteNode node)
      >>> (interpretUIGraphOpM next)

    Free (InsertEdge edge next) ->
      (insertEdge edge)
      >>> (interpretUIGraphOpM next)

    Free (DeleteEdge edge next) ->
      (deleteEdge edge)
      >>> (interpretUIGraphOpM next)

    Free (MoveNode node from to next) ->
      (_nodes <<< at (node ^. _nodeId) <<< traversed <<< _pos .~ to)
      >>> (interpretUIGraphOpM next)

    Free (UpdateNodeText node from to next) ->
      (_nodes <<< at (node ^. _nodeId) <<< traversed <<< _nodeText .~ to)
      >>> (interpretUIGraphOpM next)

    Free (UpdateEdgeText edge from to next) ->
      (modifyEdge (edge ^. _edgeId) (_edgeText .~ to))
      >>> (interpretUIGraphOpM next)

-- | Action instance required by Undoable
instance actUIGraphOpUIGraph :: Monoid a => Action (UIGraphOp a) UIGraph where
  act op = interpretUIGraphOp op


------
-- Printing interpreter

instance showUIGraphOp :: Show (UIGraphOp a) where
  show (UIGraphOp op) = showM op where
    showM = case _  of
      Pure _ -> "Pure"
      Free (InsertNode node next) -> "InsertNode " <> show (node ^. _nodeId) <> " >>= " <> showM next
      Free (DeleteNode node next) -> "DeleteNode " <> show (node ^. _nodeId) <> " >>= " <> showM next
      Free (InsertEdge edge next) -> "InsertEdge " <> show (edge ^. _edgeId) <> " >>= " <> showM next
      Free (DeleteEdge edge next) -> "DeleteEdge " <> show (edge ^. _edgeId) <> " >>= " <> showM next
      Free (MoveNode node from to next) -> "MoveNode " <> show (node ^. _nodeId) <> " from " <> show from <> " to " <> show to <> " >>= " <> showM next
      Free (UpdateNodeText node from to next) -> "UpdateNodeText " <> show (node ^. _nodeId) <> " from \"" <> from <> "\" to \"" <> to <> "\" >>= " <> showM next
      Free (UpdateEdgeText edge from to next) -> "UpdateEdgeText " <> show (edge ^. _edgeId) <> " from \"" <> from <> "\" to \"" <> to <> "\" >>= " <> showM next


--------
---- Serialisation/deserialisation instances

derive instance genericUIGraphOpF :: (Generic a z) => Generic (UIGraphOpF a) _
instance encodeUIGraphOpF :: (Generic a z, Encode a) => Encode (UIGraphOpF a) where
  encode x = x # genericEncode defaultOptions
instance decodeUIGraphOpF :: (Generic a z, Decode a) => Decode (UIGraphOpF a) where
  decode x = x # genericDecode defaultOptions

derive instance genericFree :: Generic (Free UIGraphOpF a) _
instance encodeFree :: (Encode a) => Encode (Free UIGraphOpF a) where
  encode x = x # genericEncode defaultOptions
instance decodeFree :: (Decode a) => Decode (Free UIGraphOpF a) where
  decode x = x # genericDecode defaultOptions

derive instance genericUIGraphOp :: Generic (UIGraphOp a) _
instance encodeUIGraphOp :: Encode a => Encode (UIGraphOp a) where
  encode x = x # genericEncode defaultOptions
instance decodeUIGraphOp :: (Generic a z, Decode a) => Decode (UIGraphOp a) where
  decode x = x # genericDecode defaultOptions


--------
---- Interface

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


------
-- Free monad implementation
--
-- Must be defined here to define Encode and Decode instances for UIGraphOp

data Free f a
  = Pure a
  | Free (f (Free f a))

instance functorFree :: Functor f => Functor (Free f) where
  map g (Pure a) = Pure $ g a
  map g (Free op) = Free $ map (map g) op

instance applicativeFree :: Functor f => Applicative (Free f) where
  pure = Pure

instance applyFree :: Functor f => Apply (Free f) where
  apply (Pure g) (Pure a) = Pure $ g a
  apply (Pure g) (Free bs) = map g (Free bs)
  apply (Free gs) bs = Free $ (\g -> apply g bs) <$> gs

instance bindFree :: Functor f => Bind (Free f) where
  bind :: forall f a b. Functor f => Free f a -> (a -> Free f b) -> Free f b
  bind (Pure a) g = g a
  bind (Free op) g = Free $ map (flip bind g) op

instance monadFree :: Functor f => Monad (Free f)

instance semigroupFree :: Functor f => Semigroup (Free f a) where
  append op1 op2 = op1 >>= \_ -> op2

instance monoidFree :: (Functor f, Monoid a) => Monoid (Free f a) where
  mempty = Pure mempty

liftF :: forall f. Functor f => f ~> Free f
liftF f = Free $ map Pure f

hoistFree :: forall f g. Functor f => Functor g =>
             (f ~> g) -> (Free f ~> Free g)
hoistFree nat = case _ of
  Pure a -> Pure a
  Free f -> Free $ nat f <#> hoistFree nat
