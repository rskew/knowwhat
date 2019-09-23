module Workflow.GraphOp where

import Prelude
import Workflow.Core

import Control.Alt ((<|>))
import Data.Bifunctor (lmap)
import Data.Foldable (foldl, foldr, foldMap)
import Data.Lens (Lens', Traversal', traversed, view, (^.), (^?), (.~), (%~), (?~))
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Map as Map
import Data.Set (Set)
import Data.Set as Set
import Data.Maybe (Maybe(..))
import Data.UUID (UUID)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Run (Run(..), FProxy, Step(..))
import Run as Run

--------
---- GraphOp DSL
--
--data GraphOpF next
--  = InsertNode Node next
--  | DeleteNode Node next
--  | InsertEdge Edge next
--  | DeleteEdge Edge next
--
--derive instance functorGraphOpF :: Functor GraphOpF
--
--type GRAPHOP = FProxy GraphOpF
--
--_graphOp :: SProxy "graphOp"
--_graphOp = SProxy
--
--handleGraphOp :: forall a. GraphOpF a -> Tuple (Graph -> Graph) a
--handleGraphOp = case _ of
--  InsertNode node next ->
--    (_nodes <<< at (node ^. _nodeId)) ?~ node
--  DeleteNode node next ->
--    (_nodes <<< at (node ^. _nodeId)) .~ Nothing
--  InsertEdge edge next ->
--    insertEdgeImpl edge
--  DeleteEdge edge next ->
--    deleteEdgeImpl edge
--
--interpretGraphOp :: forall r a. Run (graphOp :: GRAPHOP | r) a
--                    -> Run r (Tuple (Graph -> Graph) a)
--interpretGraphOp =
--  Run.runAccumPure
--  (\accumulator ->
--    Run.on _graphOp (Loop <<< lmap ((>>>) accumulator) <<< handleGraphOp) Done)
--  (\accumulator a -> Tuple accumulator a)
--  identity
--
--
--------
---- Interface
--
--insertNode :: Node -> Graph -> Run (graphOp :: GRAPHOP) Unit
--insertNode node graph =
--  let
--    insertNodeOp = Run.lift _graphOp $ InsertNode node unit
--    insertEdgeOp = \edge -> Run.lift _graphOp $ InsertEdge edge unit
--    insertEdgeOps = insertEdgeOp <$> lookupNodeEdges graph node
--  in
--    foldl bind insertNodeOp insertEdgeOps
--
--deleteNode :: Node -> Run (graphOp :: GRAPHOP) Unit
--deleteNode node graph =
--  let
--    deleteNodeOp = Run.lift _graphOp $ DeleteNode node unit
--    deleteEdgeOp = \edge -> Run.lift _graphOp $ DeleteEdge edge unit
--    deleteEdgeOps = deleteEdgeOp <$> lookupNodeEdges graph node
--  in
--    foldl bind deleteNodeOp deleteEdgeOps
