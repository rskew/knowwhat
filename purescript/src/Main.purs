module Main where

-- | Simple graph representation with undo-able operations.

------
-- todo optimisations:
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

import Prelude (Unit, unit, ($), (<<<), identity, map)
import Data.Maybe (Maybe(..))
import Foreign.Object as Object
import Foreign.Object (Object)
import Data.Lens (Lens', lens, over, set, setJust)
import Data.Symbol (SProxy(..))
import Data.Lens.Record (prop)
import Data.Lens.At (at)

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

type Point2D = {x :: Number, y :: Number}

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


------
-- Lens boilerplate

_Graph :: Lens' Graph {nodes :: Object GraphNode,
                       focusNode :: NodeId,
                       highlightedNodes :: NodeIdSet}
_Graph = lens (\(Graph g) -> g) (\_ -> Graph)

_nodes :: forall r. Lens' { nodes :: Object GraphNode | r } (Object GraphNode)
_nodes = prop (SProxy :: SProxy "nodes")

_highlightedNodes :: forall r. Lens' { highlightedNodes :: NodeIdSet | r } NodeIdSet
_highlightedNodes = prop (SProxy :: SProxy "highlightedNodes")

_GraphNode :: Lens' GraphNode { text :: String
                              , id :: NodeId
                              , x :: Number
                              , y :: Number
                              , children :: NodeIdSet
                              , parents :: NodeIdSet
                              , subgraphNodes :: Object GraphNode
                              }
_GraphNode = lens (\(GraphNode n) -> n) (\_ -> GraphNode)

_parents :: forall r. Lens' { parents :: NodeIdSet | r } NodeIdSet
_parents = prop (SProxy :: SProxy "parents")

_children :: forall r. Lens' { children :: NodeIdSet | r } NodeIdSet
_children = prop (SProxy :: SProxy "children")

_x :: forall r. Lens' { x :: Number | r } Number
_x = prop (SProxy :: SProxy "x")

_y :: forall r. Lens' { y :: Number | r } Number
_y = prop (SProxy :: SProxy "y")


------
-- How GraphNode and Graph actually work

addParent :: NodeId -> GraphNode -> GraphNode
addParent nodeId = over (_GraphNode <<< _parents) $ insert nodeId

deleteParent :: NodeId -> GraphNode -> GraphNode
deleteParent nodeId = over (_GraphNode <<< _parents) $ delete nodeId

addChild :: NodeId -> GraphNode -> GraphNode
addChild nodeId = over (_GraphNode <<< _children) $ insert nodeId

deleteChild :: NodeId -> GraphNode -> GraphNode
deleteChild nodeId = over (_GraphNode <<< _children) $ delete nodeId

moveNode :: Point2D -> GraphNode -> GraphNode
moveNode pos = set (_GraphNode <<< _x) pos.x <<<
               set (_GraphNode <<< _y) pos.y


applyOp :: GraphOp -> Graph -> Graph
applyOp (AddNode nodeId nodeBody) =
  setJust (_Graph <<< _nodes <<< (at nodeId)) nodeBody
applyOp (RemoveNode nodeId) =
  set (_Graph <<< _nodes <<< (at nodeId)) Nothing
applyOp (MoveNode nodeId pos) =
  over (_Graph <<< _nodes <<< (at nodeId)) $ map $ moveNode pos
applyOp EndMovement = identity
applyOp (AddEdge edge) =
  over (_Graph <<< _nodes <<< (at edge.to)) (map (addParent edge.from))
  <<<
  over (_Graph <<< _nodes <<< (at edge.from)) (map (addChild edge.to))
applyOp (RemoveEdge edge) =
  over (_Graph <<< _nodes <<< (at edge.to)) (map (deleteParent edge.from))
  <<<
  over (_Graph <<< _nodes <<< (at edge.from)) (map (deleteChild edge.to))
applyOp (UpdateFocus nodeId) =
  over _Graph (_ { focusNode = nodeId})
applyOp (Highlight nodeId) =
  over (_Graph <<< _highlightedNodes) (insert nodeId)
applyOp (UnHighlight nodeId) =
  over (_Graph <<< _highlightedNodes) (delete nodeId)




demo :: Graph
demo = applyOp (UpdateFocus "goofus")
       $ applyOp (Highlight "thingo")
       $ applyOp (AddNode "thingo" $ GraphNode
           { text: "thingo"
           , id : "thingo"
           , x : 205.0
           , y : 100.0
           , children : emptyNodeIdSet
           , parents : singletonNodeIdSet "goofus"
           , subgraphNodes : Object.empty
           })
       $ applyOp (AddNode "goofus" $ GraphNode
           { text: "goofus"
           , id : "goofus"
           , x: 455.0
           , y: 100.0
           , children : singletonNodeIdSet "thingo"
           , parents : emptyNodeIdSet
           , subgraphNodes : Object.empty
           }) emptyGraph

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
--buildGraph ops = foldl applyOp empty $ undoRedoReverse ops
--  where
--    undoRedoReverse ops = urr 0 0 ops
--    urr 0     redos ( OpOrUndo op : ops ) = urr 0 redos ops <> [op]
--    urr undos redos ( OpOrUndo op : ops ) = urr (undos - 1) redos ops
--    urr 0     redos ( Redo        : ops ) = urr 0 (redos + 1) ops
--    urr undos redos ( Redo        : ops ) = urr (undos - 1) redos ops
--    urr undos 0     ( Undo        : ops ) = urr (undos + 1) 0 ops
--    urr undos redos ( Undo        : ops ) = urr undos (redos - 1) ops
--    urr _     _     _                     = []
