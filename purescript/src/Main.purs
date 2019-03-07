module Main where

-- | Simple graph representation with undo-able operations.

------
-- TODO
-- - simplest implementation
-- - ffi for d3 view code to use this
--
--
------
-- data structures
--
-- no attempt to make more general versions of the data structures yet
--
------
-- optimisations:
-- - combining sequential node moves into a single move (think dragging)
-- - ST monad for mutable updates to graph
-- - truncating undo/redo history to stop the memory leak
--
------
-- simplest implementation
--
-- the graph is literally kept as the list of grounded graph operations applied so far.
-- an undo or redo is a grounded graph op
-- an undo is interpreted as removing the last non-undo graph op
-- a redo is unterpreted as undoing the last redo
-- when called, the interpreter walks backwards applying redos and undos first,
--
-- This only needs the ability to apply an op to the graph to be fully implemented.
-- Keeping the full graph would require:
-- - storing node body information when removing
-- - handling missing edges on redos

import Prelude (Unit, unit, ($))
import Effect (Effect)
import Effect.Console (log)
import Data.Maybe (Maybe(..))
import Foreign.Object as Object
import Foreign.Object (Object)

main :: Effect Unit
main = do
  log "Hello :|"


type NodeId = String

type NodeIdSet = Object Unit

insert :: NodeId -> NodeIdSet -> NodeIdSet
insert nodeId = Object.insert nodeId unit

delete :: NodeId -> NodeIdSet -> NodeIdSet
delete = Object.delete

singletonNodeIdSet :: NodeId -> NodeIdSet
singletonNodeIdSet nodeId = Object.singleton nodeId unit

emptyNodeIdSet :: NodeIdSet
emptyNodeIdSet = Object.empty

newtype Graph = Graph
  { nodes :: Object GraphNode
  , focusNode :: NodeId
  , highlightedNodes :: NodeIdSet
  }

emptyGraph :: Graph
emptyGraph = Graph
  { nodes: Object.empty
  , focusNode: ""
  , highlightedNodes: emptyNodeIdSet
  }

newtype GraphNode = GraphNode
  { text :: String
  , id :: NodeId
  , x :: Number
  , y :: Number
  , children :: NodeIdSet
  , parents :: NodeIdSet
  , subgraphNodes :: Object GraphNode
  }

newtype Point2D = Point2D {x :: Number, y :: Number}

data GraphOp =
    AddNode NodeId GraphNode
  | RemoveNode NodeId
  | MoveNode NodeId Point2D
  | EndMovement
  | AddEdge { from :: NodeId,
              to :: NodeId }
  | RemoveEdge { from :: NodeId,
                 to :: NodeId }
  | UpdateFocus NodeId
  | Highlight NodeId
  | UnHighlight NodeId


applyGraphOp :: Graph -> GraphOp -> Graph
applyGraphOp (Graph g) (AddNode nodeId nodeBody) =
  Graph $ _ { nodes = _ } g $ Object.insert nodeId nodeBody g.nodes
applyGraphOp (Graph g) (RemoveNode nodeId) =
  Graph $ _ { nodes = _ } g $ Object.delete nodeId g.nodes
-- some lenses might help here
applyGraphOp (Graph g) (MoveNode nodeId (Point2D pos)) =
  Graph $ g {nodes = _ } $ Object.update mover nodeId g.nodes
    where
      mover (GraphNode gn) = Just $ GraphNode $ gn { x = pos.x, y = pos.y }
applyGraphOp g EndMovement = g
applyGraphOp (Graph g) (AddEdge edge) =
  Graph $ g { nodes = _ }
        $ (\nodes -> case (Object.lookup edge.from nodes) of
                   Just (GraphNode node) ->
                     Object.insert edge.from (GraphNode (node { children = _ } (insert edge.to node.children))) nodes
                   Nothing -> nodes)
        $ (\nodes -> case (Object.lookup edge.to nodes) of
                   Just (GraphNode node) ->
                     Object.insert edge.to (GraphNode (node { parents = _ } (insert edge.from node.parents))) nodes
                   Nothing -> nodes)
        $ g.nodes
applyGraphOp (Graph g) (RemoveEdge edge) =
  Graph $ g { nodes = _ }
        $ (\nodes -> case (Object.lookup edge.from nodes) of
                       Just (GraphNode node) ->
                         Object.insert edge.from (GraphNode (node { children = _ } (delete edge.to node.children))) nodes
                       Nothing -> nodes)
        $ (\nodes -> case (Object.lookup edge.to nodes) of
                       Just (GraphNode node) ->
                         Object.insert edge.to (GraphNode (node { parents = _ } (delete edge.from node.parents))) nodes
                       Nothing -> nodes)
        $ g.nodes
applyGraphOp (Graph g) (UpdateFocus nodeId) = Graph $ _ { focusNode = nodeId} g
applyGraphOp (Graph g) (Highlight nodeId) =
  Graph $ _ { highlightedNodes = _ } g $ insert nodeId g.highlightedNodes
applyGraphOp (Graph g) (UnHighlight nodeId) =
  Graph $ _ { highlightedNodes = _ } g $ delete nodeId g.highlightedNodes


demo :: Object GraphNode
demo = Object.fromFoldable $ (\(Graph asdf) -> ((Object.toUnfoldable asdf.nodes) :: Array _))
       $ applyGraphOp goober $ AddNode "thingo" $ GraphNode
  { text: "thingo"
  , id : "thingo"
  , x : 205.0
  , y : 100.0
  , children : emptyNodeIdSet
  , parents : singletonNodeIdSet "goofus"
  , subgraphNodes : Object.empty
  }
  where
    goober = applyGraphOp emptyGraph $ AddNode "goofus" $ GraphNode
      { text: "goofus"
      , id : "goofus"
      , x: 455.0
      , y: 100.0
      , children : singletonNodeIdSet "thingo"
      , parents : emptyNodeIdSet
      , subgraphNodes : Object.empty
      }

--type UndoableGraph = List (OpOrUndo GraphOp)
--
--
--addGraphOp :: OpOrUndo GraphOp -> UndoableGraph -> UndoableGraph
---- | Combine multiple sequential moves of the same node
--addGraphOp (OpOrUndo (MoveNode k pos)) ((OpOrUndo (MoveNode k prevPos)) : ops) =
--  OpOrUndo (MoveNode k pos) : ops
--addGraphOp op ops = op : ops
--
--
--buildGraph :: UndoableGraph -> Graph
---- | Apply Redos and Undos reversing list of applicable ops along the way
---- | Apply graph ops with a fold
---- |
---- | Undos nullify the operation that comes directly after them, and they
---- | stack up in sequence.
---- | Redos nullify the Undo that comes after them.
---- |
---- | After any op, Undo or Redo, this function can be run to recompute the
---- | current graph state, with operations changing their behaviour according
---- | to Undos and Redos ahead of them in the list. This way, the effect
---- | of a weird sequence of Undos/Redos can't leave the graph in an invalid state
---- | as a simple sequence of grap ops (without Undo/Redo) must produce a valid graph.
--buildGraph ops = foldl applyGraphOp empty $ undoRedoReverse ops
--  where
--    undoRedoReverse ops = urr 0 0 ops
--    urr 0     redos ( OpOrUndo op : ops ) = urr 0 redos ops <> [op]
--    urr undos redos ( OpOrUndo op : ops ) = urr (undos - 1) redos ops
--    urr 0     redos ( Redo        : ops ) = urr 0 (redos + 1) ops
--    urr undos redos ( Redo        : ops ) = urr (undos - 1) redos ops
--    urr undos 0     ( Undo        : ops ) = urr (undos + 1) 0 ops
--    urr undos redos ( Undo        : ops ) = urr undos (redos - 1) ops
--    urr _     _     _                     = []
