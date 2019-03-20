module Main where

-- | Simple graph representation with undo-able operations.

------
-- todo optimisations:
-- - ST monad for mutable updates to graph and/or
--   truncating undo/redo history
--
------
-- simplest implementation
--
-- The graph is kept as the list of UI operations applied so far in a
-- free-monad-like way.
-- An undo or redo is a grounded graph op.
-- An undo is interpreted as removing the last non-undo graph op.
-- A redo is unterpreted as undoing the last redo.
-- The interpreter walks backwards applying redos and undos first, before
-- applying the remaining operations in sequence.
--
-- Storing the full, computed graph to support faster operations
-- will require some interplay between the graph and the list of operations
-- for undo/redo.

import Prelude (Unit, unit, ($), (<<<), map, (-), (+), flip, (==), compare, Ordering, append, (<>), (<), (<=))
import Data.String (Pattern(..), split)
import Data.Foldable (foldl, maximumBy, foldMap, all)
import Data.Array (mapMaybe, filter, sortWith, partition, reverse, (!!))
import Data.Maybe (Maybe(..), isJust)
import Data.Maybe as Maybe
import Foreign.Object as Object
import Foreign.Object (Object, keys, size)
import Data.Lens (Lens', lens, view, over, set, setJust)
import Data.Symbol (SProxy(..))
import Data.Lens.Record (prop)
import Data.Lens.At (at)
import Data.List (List(..), (:), length)


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
  , focus :: Focus
  , highlighted :: NodeIdSet
  }

emptyGraph :: Graph
emptyGraph = Graph
  { nodes: Object.empty
  , focus: NoFocus
  , highlighted: emptyNodeIdSet
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

type Point2D = { x :: Number, y :: Number }

type Edge = { source :: NodeId, target :: NodeId }

type EdgeId = String

computeEdgeId :: Edge -> String
computeEdgeId edge = edge.source <> "." <> edge.target

data Focus =
  FocusNode String
  | FocusEdge String
  | NoFocus

data GraphOp =
  AddNode GraphNode
  | RemoveNode GraphNode
  --| UpdatePosition GraphNode Point2D
  | MoveNode NodeId Point2D
  | AddParent NodeId NodeId
  | RemoveParent NodeId NodeId
  | AddChild NodeId NodeId
  | RemoveChild NodeId NodeId
  | AddEdge Edge
  | RemoveEdge Edge
  | UpdateText NodeId String
  | UpdateSubgraphNodes NodeId (Object GraphNode)
  | UpdateFocus Focus
  | Highlight NodeId
  | UnHighlight NodeId

------
-- Lens boilerplate

_Graph :: Lens' Graph {nodes :: Object GraphNode,
                       focus :: Focus,
                       highlighted :: NodeIdSet}
_Graph = lens (\(Graph g) -> g) (\_ -> Graph)

_nodes :: forall r. Lens' { nodes :: Object GraphNode | r } (Object GraphNode)
_nodes = prop (SProxy :: SProxy "nodes")

_highlighted :: forall r. Lens' { highlighted :: NodeIdSet | r } NodeIdSet
_highlighted = prop (SProxy :: SProxy "highlighted")

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

_text :: forall r. Lens' { text :: String | r } String
_text = prop (SProxy :: SProxy "text")

_id :: forall r. Lens' { id :: String | r } String
_id = prop (SProxy :: SProxy "id")

_subgraphNodes :: forall r. Lens' { subgraphNodes :: Object GraphNode | r } (Object GraphNode)
_subgraphNodes = prop (SProxy :: SProxy "subgraphNodes")

_focus :: forall r. Lens' { focus :: Focus | r } Focus
_focus = prop (SProxy :: SProxy "focus")


------
-- Graph logic

addParent :: NodeId -> GraphNode -> GraphNode
addParent nodeId = over (_GraphNode <<< _parents) $ insert nodeId

removeParent :: NodeId -> GraphNode -> GraphNode
removeParent nodeId = over (_GraphNode <<< _parents) $ delete nodeId

addChild :: NodeId -> GraphNode -> GraphNode
addChild nodeId = over (_GraphNode <<< _children) $ insert nodeId

removeChild :: NodeId -> GraphNode -> GraphNode
removeChild nodeId = over (_GraphNode <<< _children) $ delete nodeId

moveNode :: Point2D -> GraphNode -> GraphNode
moveNode pos = set (_GraphNode <<< _x) pos.x <<<
               set (_GraphNode <<< _y) pos.y

updateText :: String -> GraphNode -> GraphNode
updateText = set (_GraphNode <<< _text)

updateSubgraphNodes :: Object GraphNode -> GraphNode -> GraphNode
updateSubgraphNodes = set (_GraphNode <<< _subgraphNodes)


applyGraphOp :: GraphOp -> Graph -> Graph
applyGraphOp (AddNode (GraphNode nodeBody)) =
  setJust (_Graph <<< _nodes <<< (at nodeBody.id)) (GraphNode nodeBody)
applyGraphOp (RemoveNode (GraphNode nodeBody)) =
  set (_Graph <<< _nodes <<< (at nodeBody.id)) Nothing
applyGraphOp (MoveNode nodeId pos) =
  over (_Graph <<< _nodes <<< (at nodeId)) $ map $ moveNode pos
applyGraphOp (AddParent nodeId parentId) =
  over (_Graph <<< _nodes <<< (at nodeId)) $ map $ addParent parentId
applyGraphOp (RemoveParent nodeId parentId) =
  over (_Graph <<< _nodes <<< (at nodeId)) $ map $ removeParent parentId
applyGraphOp (AddChild nodeId childId) =
  over (_Graph <<< _nodes <<< (at nodeId)) $ map $ addChild childId
applyGraphOp (RemoveChild nodeId childId) =
  over (_Graph <<< _nodes <<< (at nodeId)) $ map $ removeChild childId
applyGraphOp (AddEdge edge) =
  applyGraphOp (AddParent edge.target edge.source)
  <<<
  applyGraphOp (AddChild edge.source edge.target)
applyGraphOp (RemoveEdge edge) =
  applyGraphOp (RemoveParent edge.target edge.source)
  <<<
  applyGraphOp (RemoveChild edge.source edge.target)
applyGraphOp (UpdateText nodeId newText) =
  over (_Graph <<< _nodes <<< (at nodeId)) $ map $ updateText newText
applyGraphOp (UpdateSubgraphNodes nodeId newSubgraphNodes) =
  over (_Graph <<< _nodes <<< (at nodeId)) $ map $ updateSubgraphNodes newSubgraphNodes
applyGraphOp (UpdateFocus newFocus) =
  over _Graph (_ { focus = newFocus})
applyGraphOp (Highlight nodeId) =
  over (_Graph <<< _highlighted) (insert nodeId)
applyGraphOp (UnHighlight nodeId) =
  over (_Graph <<< _highlighted) (delete nodeId)




demo :: Graph
demo = buildGraph
       ( Op (UpdateFocus (FocusNode "goofus"))
       : Op (Highlight "thingo")
       : Op (AddNode $ GraphNode
           { text: "thingo"
           , id : "thingo"
           , x : 205.0
           , y : 100.0
           , children : emptyNodeIdSet
           , parents : singletonNodeIdSet "goofus"
           , subgraphNodes : Object.empty
           })
       : Op (AddNode $ GraphNode
           { text: "goofus"
           , id : "goofus"
           , x: 455.0
           , y: 100.0
           , children : singletonNodeIdSet "thingo"
           , parents : emptyNodeIdSet
           , subgraphNodes : Object.empty
           })
       : Nil
       )

oppy :: GraphOp
oppy = AddNode $ GraphNode
   { text: "goofus"
   , id : "goofus"
   , x: 455.0
   , y: 100.0
   , children : singletonNodeIdSet "thingo"
   , parents : emptyNodeIdSet
   , subgraphNodes : Object.empty
   }

graphLength :: UndoableGraph -> Int
graphLength graph = length graph

emptyUndoableGraph :: UndoableGraph
emptyUndoableGraph = Nil


data OpOrUndo a = Op a | Undo | Redo

type UndoableGraph = List (OpOrUndo GraphOp)

-- | Group successive movements into a single movement
addOp :: OpOrUndo GraphOp -> UndoableGraph -> UndoableGraph
addOp op ops = case op of
  Op (MoveNode newNode newPos) -> case ops of
    (Op (MoveNode prevNode prevPos) : ops') ->
      if newNode == prevNode
         then op : ops'
         else op : ops
    _ -> op : ops
  _ -> op : ops


------
-- Graph Interactions

--| Whereas a GraphOp represents a fundamental graph operation,
--| an GraphInteraction represents an abstract relation between
--| the before and after affect of the operation.
--| An GraphInteraction is used as a record of an UI operation having happened,
--| rather than a description of an operation which is to be applied.
--| For example, when grouping a set of nodes, the group node which is
--| then constructed is an output of the operation, but it is recorded
--| in the Group data so that the operation can be undone.
--data GraphInteraction =
  --NewNode GraphNode
  --| AddEdge Edge
  --| RemoveEdge Edge
  --| MoveNode GraphNode Point2D
  --| AddHighlight NodeId
  --| RemoveHighlight NodeId
  --| Group (Object GraphNode) GraphNode
  --| UnGroup GraphNode (Object GraphNode)
  --| UngroupNode


removeFocus :: Graph -> UndoableGraph -> UndoableGraph
removeFocus g ops = case view (_Graph <<< _focus) g of
  NoFocus -> ops
  FocusNode nodeId -> case lookupNode g nodeId of
    Nothing -> ops
    Just (GraphNode focusNode) ->
      let
        newFocus currentFocus = case size focusNode.parents of
          0 -> case (keys focusNode.children) !! 0 of
            Nothing -> NoFocus
            Just childId -> FocusNode childId
          otherwise -> case (keys focusNode.parents) !! 0 of
            Nothing -> NoFocus
            Just parentId -> FocusNode parentId
      in doOp
        (UpdateFocus (newFocus focusNode))
        $ doOp (RemoveNode (GraphNode focusNode)) ops
  FocusEdge edgeId -> case lookupEdge g edgeId of
    Nothing -> ops
    Just edge -> doOp (UpdateFocus (FocusNode edge.source))
      $ doOp (RemoveEdge edge) ops


------
-- Traversal

traverseUp :: Graph -> UndoableGraph -> UndoableGraph
traverseUp g ops = case getFocusNode g of
  Just (GraphNode node) -> case (keys node.parents) !! 0 of
    Nothing -> ops
    Just parentId -> doOp (UpdateFocus (FocusNode parentId)) ops
  Nothing -> ops

traverseDown :: Graph -> UndoableGraph -> UndoableGraph
traverseDown g ops = case getFocusNode g of
  Just (GraphNode node) -> case (keys node.children) !! 0 of
    Nothing -> ops
    Just childId -> doOp (UpdateFocus (FocusNode childId)) ops
  Nothing -> ops

siblings :: Graph -> GraphNode -> NodeIdSet
siblings g node = foldMap (view (_GraphNode <<< _children)) parents
  where
    parents = lookupNodes g parentIds
    parentIds = view (_GraphNode <<< _parents) node

coparents :: Graph -> GraphNode -> NodeIdSet
coparents g node = foldMap (view (_GraphNode <<< _parents)) children
  where
    children = lookupNodes g childIds
    childIds = view (_GraphNode <<< _children) node

nodeOnLeft :: Graph -> GraphNode -> GraphNode
nodeOnLeft g node = fromMaybe node $ toTheLeft !! 0
  where
    siblingsAndCoparents = lookupNodes g $ siblings g node <> coparents g node
    xSortedSibCops = sortWith viewX siblingsAndCoparents
    viewX = view (_GraphNode <<< _x)
    splitSortedSibCops = partition (\n -> viewX n < viewX node) xSortedSibCops
    toTheLeft = reverse $ splitSortedSibCops.no <> splitSortedSibCops.yes

traverseLeft :: Graph -> UndoableGraph -> UndoableGraph
traverseLeft g ops = case getFocusNode g of
  Nothing -> ops
  Just graphNode -> doOp (UpdateFocus (FocusNode (view (_GraphNode <<< _id) (nodeOnLeft g graphNode)))) ops

nodeOnRight :: Graph -> GraphNode -> GraphNode
nodeOnRight g node = fromMaybe node $ toTheRight !! 0
  where
    siblingsAndCoparents = lookupNodes g $ siblings g node <> coparents g node
    xSortedSibCops = sortWith viewX siblingsAndCoparents
    viewX = view (_GraphNode <<< _x)
    splitSortedSibCops = partition (\n -> viewX n <= viewX node) xSortedSibCops
    toTheRight = splitSortedSibCops.no <> splitSortedSibCops.yes

traverseRight :: Graph -> UndoableGraph -> UndoableGraph
traverseRight g ops = case getFocusNode g of
  Nothing -> ops
  Just graphNode -> doOp (UpdateFocus (FocusNode (view (_GraphNode <<< _id) (nodeOnRight g graphNode)))) ops

------
-- Wrap purs stuff for easy use from JS

doOp :: GraphOp -> UndoableGraph -> UndoableGraph
doOp op ops = addOp (Op op) ops

undo :: UndoableGraph -> UndoableGraph
undo ops = addOp Undo ops

redo :: UndoableGraph -> UndoableGraph
redo ops = addOp Redo ops


-- | Apply Redos and Undos reversing list of applicable ops along the way
-- | Apply graph ops with a fold
-- |
-- | Undos nullify the operation that comes directly after them, and they
-- | stack up in sequence.
-- | Redos nullify the Undo that comes after them.
-- |
-- | After any op, Undo or Redo, this function can be run to recompute the
-- | current graph state, with operations changing their behaviour according
-- | to Undos and Redos ahead of them in the list. This way, the effect
-- | of a weird sequence of Undos/Redos can't leave the graph in an invalid state
-- | as a simple sequence of grap ops (without Undo/Redo) must produce a valid graph.
buildGraph :: UndoableGraph -> Graph
buildGraph ops = foldl (flip applyGraphOp) emptyGraph $ undoRedoReverse ops
  where
    undoRedoReverse ops' = urr 0 0 Nil ops'
    urr :: Int -> Int -> List GraphOp -> UndoableGraph -> List GraphOp
    urr 0     redos rev ( Op op : ops' ) = urr 0 redos (op : rev) ops'
    urr undos redos rev ( Op op : ops' ) = urr (undos - 1) redos rev ops'
    urr 0     redos rev ( Redo  : ops' ) = urr 0 (redos + 1) rev ops'
    urr undos redos rev ( Redo  : ops' ) = urr (undos - 1) redos rev ops'
    urr undos 0     rev ( Undo  : ops' ) = urr (undos + 1) 0 rev ops'
    urr undos redos rev ( Undo  : ops' ) = urr undos (redos - 1) rev ops'
    urr _     _     rev _                = rev



------
-- Utilities

lookupNode :: Graph -> NodeId -> Maybe GraphNode
lookupNode g nodeId = view (_Graph <<< _nodes <<< at nodeId) g

lookupNodes :: Graph -> NodeIdSet -> Array GraphNode
lookupNodes g nodeIds = mapMaybe (lookupNode g) $ keys nodeIds

getFocusNode :: Graph -> Maybe GraphNode
getFocusNode g = case view (_Graph <<< _focus) g of
  FocusNode nodeId -> lookupNode g nodeId
  otherwise -> Nothing

lookupEdge :: Graph -> EdgeId -> Maybe Edge
lookupEdge g edgeId =
  let edgeNodeIds = split (Pattern ".") edgeId
      sourceId = fromMaybe "" $ edgeNodeIds !! 0
      targetId = fromMaybe "" $ edgeNodeIds !! 1
  in
  case all isJust [lookupNode g sourceId, lookupNode g targetId] of
    false -> Nothing
    true -> Just { source : sourceId, target: targetId }

------
-- Graph Queries

terminalestNode :: Array GraphNode -> Maybe GraphNode
terminalestNode nodes = case filter isTerminal nodes of
  [] -> maximumBy lower nodes
  terminalNodes -> maximumBy mostParentsLowest terminalNodes
  where
    mostParentsLowest :: GraphNode -> GraphNode -> Ordering
    mostParentsLowest a b = compare (nParents a) (nParents b) `append` lower a b
    lower :: GraphNode -> GraphNode -> Ordering
    lower a b = compare (getY a) (getY b)
    isTerminal :: GraphNode -> Boolean
    isTerminal node = nChildren node == 0
    nChildren = size <<< view (_GraphNode <<< _children)
    nParents :: GraphNode -> Int
    nParents = size <<< view (_GraphNode <<< _parents)
    getY :: GraphNode -> Number
    getY = view (_GraphNode <<< _y)

-- TODO: re-export to JS using module system properly
fromMaybe :: forall a. a -> Maybe a -> a
fromMaybe = Maybe.fromMaybe

maybe :: forall a. a -> Maybe a
maybe = Just

fromFocus :: Focus -> String
fromFocus NoFocus = ""
fromFocus (FocusNode nodeId) = nodeId
fromFocus (FocusEdge edgeId) = edgeId
