module Workflow.Graph.GraphOp where

import Prelude
import Workflow.Core (Graph, Node, Edge, _nodes, _nodeId)
import Workflow.Graph (lookupNodeEdges, insertEdgeImpl, deleteEdgeImpl)

import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Lens ((^.), (.~), (?~))
import Data.Lens.At (at)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Foreign (Foreign)
import Foreign.Class (class Encode, class Decode)
import Foreign.Generic (genericEncode, genericDecode, defaultOptions)
import Foreign.Unit (ForeignUnit(..))
import Run (Run, FProxy, Step(..))
import Run as Run

------
-- GraphOp DSL

data GraphOpF next
  = InsertNode Node next
  | DeleteNode Node next
  | InsertEdge Edge next
  | DeleteEdge Edge next

derive instance functorGraphOpF :: Functor GraphOpF

invertGraphOp :: GraphOpF ~> GraphOpF
invertGraphOp = case _ of
  InsertNode node next -> DeleteNode node next
  DeleteNode node next -> InsertNode node next
  InsertEdge edge next -> DeleteEdge edge next
  DeleteEdge edge next -> InsertEdge edge next

type GRAPHOP = FProxy GraphOpF

_graphOp :: SProxy "graphOp"
_graphOp = SProxy

handleGraphOp :: forall a. GraphOpF a -> Tuple (Graph -> Graph) a
handleGraphOp = case _ of
  InsertNode node next ->
    Tuple ((_nodes <<< at (node ^. _nodeId)) ?~ node) next
  DeleteNode node next ->
    Tuple ((_nodes <<< at (node ^. _nodeId)) .~ Nothing) next
  InsertEdge edge next ->
    Tuple (insertEdgeImpl edge) next
  DeleteEdge edge next ->
    Tuple (deleteEdgeImpl edge) next

interpretGraphOp :: forall r a. Run (graphOp :: GRAPHOP | r) a
                    -> Run r (Tuple (Graph -> Graph) a)
interpretGraphOp =
  Run.runAccumPure
  (\accumulator ->
    Run.on _graphOp (Loop <<< lmap ((>>>) accumulator) <<< handleGraphOp) Done)
  (\accumulator a -> Tuple accumulator a)
  identity

showGraphOp :: forall a. GraphOpF a -> Tuple String a
showGraphOp = case _ of
  InsertNode node next ->
    Tuple ("InsertNode " <> show node) next
  DeleteNode node next ->
    Tuple ("DeleteNode " <> show node) next
  InsertEdge edge next ->
    Tuple ("InsertEdge " <> show edge) next
  DeleteEdge edge next ->
    Tuple ("DeleteEdge " <> show edge) next


------
-- Interface

insertNode :: forall r. Node -> Graph -> Run (graphOp :: GRAPHOP | r) Unit
insertNode node graph =
  let
    insertNodeOp = Run.lift _graphOp $ InsertNode node unit
    insertEdgeOp = \edge -> Run.lift _graphOp $ InsertEdge edge unit
    insertEdgeOps = insertEdgeOp <$> (Array.fromFoldable $ lookupNodeEdges graph node)
  in
    foldl bind insertNodeOp $ const <$> insertEdgeOps

deleteNode :: forall r. Node -> Graph -> Run (graphOp :: GRAPHOP | r) Unit
deleteNode node graph =
  let
    deleteNodeOp = Run.lift _graphOp $ DeleteNode node unit
    deleteEdgeOp = \edge -> Run.lift _graphOp $ DeleteEdge edge unit
    deleteEdgeOps = (const <<< deleteEdgeOp) <$> (Array.fromFoldable $ lookupNodeEdges graph node)
  in
    foldl bind deleteNodeOp deleteEdgeOps

insertEdge :: forall r. Edge -> Run (graphOp :: GRAPHOP | r) Unit
insertEdge edge = Run.lift _graphOp $ InsertEdge edge unit

deleteEdge :: forall r. Edge -> Run (graphOp :: GRAPHOP | r) Unit
deleteEdge edge = Run.lift _graphOp $ DeleteEdge edge unit

--------
---- Serialisation/deserialisation

derive instance genericGraphOpF :: (Generic a z) => Generic (GraphOpF a) _

instance encodeGraphOpF' :: (Generic a z, Encode a) => Encode (GraphOpF a) where
  encode x = x # genericEncode defaultOptions

instance decodeGraphOpF' :: (Generic a z, Decode a) => Decode (GraphOpF a) where
  decode x = x # genericDecode defaultOptions

encodeGraphOpF :: forall a. GraphOpF a -> Tuple Foreign a
encodeGraphOpF = lmap (genericEncode defaultOptions) <<< case _ of
  InsertNode node next -> Tuple (InsertNode node ForeignUnit) next
  DeleteNode node next -> Tuple (DeleteNode node ForeignUnit) next
  InsertEdge edge next -> Tuple (InsertEdge edge ForeignUnit) next
  DeleteEdge edge next -> Tuple (DeleteEdge edge ForeignUnit) next

