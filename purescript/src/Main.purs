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

import Prelude (Unit, unit, ($), (<<<), (>>>), map, flip, (==), compare, Ordering, append, (<>), (<), (<=), bind, pure, (>>=))
import Data.String (Pattern(..), split, contains, stripPrefix)
import Data.Foldable (foldl, maximumBy, foldMap, all)
import Data.Array (mapMaybe, filter, sortWith, partition, (!!))
import Data.Array as Array
import Data.Maybe (Maybe(..), isJust)
import Data.Maybe as Maybe
import Data.Lens (Lens', lens, view, over, set, setJust)
import Data.Symbol (SProxy(..))
import Data.Lens.Record (prop)
import Data.Lens.At (at)
import Data.List (List(..), (:))
import Data.List as List
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity)
import Data.List.NonEmpty (NonEmptyList)
import Data.Tuple (Tuple(..))
import Control.Monad.Except.Trans (ExceptT)
import Foreign.Object as Object
import Foreign.Object (Object, keys, values, size)
import Foreign.Generic (defaultOptions, genericEncode, genericDecode, genericDecodeJSON, genericEncodeJSON)
import Foreign.Generic.Types (SumEncoding)
import Foreign.Class (class Encode, class Decode)
import Foreign (ForeignError)


version :: String
version = "0.0"

genericEncodeOpts ::
  { unwrapSingleConstructors :: Boolean
  , fieldTransform :: String -> String
  , sumEncoding :: SumEncoding
  , unwrapSingleArguments :: Boolean
  }
genericEncodeOpts = defaultOptions { unwrapSingleConstructors = true }

type NodeId = String

type NodeIdSet = Object Unit

insert :: NodeId -> NodeIdSet -> NodeIdSet
insert nodeId = Object.insert nodeId unit

delete :: NodeId -> NodeIdSet -> NodeIdSet
delete = Object.delete

nodeIdSetFromArray :: Array NodeId -> NodeIdSet
nodeIdSetFromArray nodeIdArr = Object.fromFoldable $ map (\nodeId -> (Tuple nodeId unit)) nodeIdArr

emptyNodeIdSet :: NodeIdSet
emptyNodeIdSet = Object.empty

newtype Graph = Graph
  { nodes :: Object GraphNode
  , focus :: Focus
  , highlighted :: NodeIdSet
  }
derive instance genericGraph :: Generic Graph _

instance encodeGraph :: Encode Graph where
  encode = genericEncode genericEncodeOpts

instance decodeGraph :: Decode Graph where
  decode = genericDecode genericEncodeOpts

graphToJSON :: List GraphOp -> String
graphToJSON ops =
  let (Graph g) = buildGraph ops in
  genericEncodeJSON genericEncodeOpts (Graph g)

graphFromJSON :: String -> ExceptT (NonEmptyList ForeignError) Identity Graph
graphFromJSON graphJSON = genericDecodeJSON genericEncodeOpts graphJSON


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
derive instance genericGraphNode :: Generic GraphNode _

instance encodeGraphNode :: Encode GraphNode where
  encode node = genericEncode genericEncodeOpts node

instance decodeGraphNode :: Decode GraphNode where
  decode node = genericDecode genericEncodeOpts node


type Point2D = { x :: Number, y :: Number }

type Edge = { source :: NodeId, target :: NodeId }

type EdgeId = String

computeEdgeId :: Edge -> String
computeEdgeId edge = edge.source <> "." <> edge.target

data Focus =
  FocusNode String
  | FocusEdge Edge
  | NoFocus
derive instance genericFocus :: Generic Focus _

instance encodeFocus :: Encode Focus where
  encode = genericEncode genericEncodeOpts

instance decodeFocus :: Decode Focus where
  decode = genericDecode genericEncodeOpts


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
demo = buildGraph $
       UpdateFocus (FocusNode "goofus")
       : AddNode (GraphNode
           { text: "Title: Workflow"
           , id : "title"
           , x : 205.0
           , y : 150.0
           , children : emptyNodeIdSet
           , parents : nodeIdSetFromArray ["goofus"]
           , subgraphNodes : Object.empty
           })
       : Highlight "thingo"
       : AddNode (GraphNode
           { text: "thingo"
           , id : "thingo"
           , x : 205.0
           , y : 100.0
           , children : emptyNodeIdSet
           , parents : nodeIdSetFromArray ["goofus"]
           , subgraphNodes : Object.empty
           })
       : AddNode (GraphNode
           { text: "goofus"
           , id : "goofus"
           , x: 255.0
           , y: 270.0
           , children : nodeIdSetFromArray [ "thingo", "title" ]
           , parents : emptyNodeIdSet
           , subgraphNodes : Object.empty
           })
       : Nil

-- | Group successive movements into a single movement
addOp :: GraphOp -> List GraphOp -> List GraphOp
addOp op ops = case op of
  MoveNode newNode newPos -> case ops of
    MoveNode prevNode prevPos : ops' ->
      if newNode == prevNode
         then op : ops'
         else op : ops
    _ -> op : ops
  _ -> op : ops

buildGraph :: List GraphOp -> Graph
buildGraph ops = foldl (flip applyGraphOp) emptyGraph $ List.reverse ops


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


deleteNode :: GraphNode -> List GraphOp -> List GraphOp
deleteNode (GraphNode node) ops =
  addOp (RemoveNode (GraphNode node))
  $ foldl (flip addOp) ops $ List.fromFoldable $ removeParentEdges <> removeChildEdges
  where
    removeParentEdges = map (\parentId -> RemoveEdge { source: parentId, target: node.id }) $ keys node.parents
    removeChildEdges = map (\childId -> RemoveEdge { source: node.id, target: childId }) $ keys node.children

removeFocus :: List GraphOp -> List GraphOp
removeFocus ops =
  let g = buildGraph ops in
  case view (_Graph <<< _focus) g of
    NoFocus -> ops
    FocusNode nodeId -> case lookupNode g nodeId of
      Nothing -> ops
      Just (GraphNode focusNode) ->
        let
          newFocus = case size focusNode.parents of
            0 -> case (keys focusNode.children) !! 0 of
              Nothing -> NoFocus
              Just childId -> FocusNode childId
            otherwise -> case (keys focusNode.parents) !! 0 of
              Nothing -> NoFocus
              Just parentId -> FocusNode parentId
        in addOp
          (UpdateFocus newFocus)
          $ deleteNode (GraphNode focusNode) ops
    FocusEdge edge -> addOp (UpdateFocus (FocusNode edge.source))
        $ addOp (RemoveEdge edge) ops


------
-- Highlighting

toggleHighlightFocus :: List GraphOp -> List GraphOp
toggleHighlightFocus ops =
  let g = buildGraph ops in
  case view (_Graph <<< _focus) g of
    FocusNode nodeId -> case Object.member nodeId (view (_Graph <<< _highlighted) g) of
      true -> addOp (UnHighlight nodeId) ops
      false -> addOp (Highlight nodeId) ops
    _ -> ops

------
-- Traversal

traverseUp :: List GraphOp -> List GraphOp
traverseUp ops =
  let g = buildGraph ops in
  case view (_Graph <<< _focus) g of
    FocusNode nodeId -> fromMaybe ops do
      graphNode <- lookupNode g nodeId
      let node = view _GraphNode graphNode
      parentId <- (keys node.parents) !! 0
      pure $ addOp (UpdateFocus (FocusEdge { source: parentId, target: node.id })) ops
    FocusEdge edge -> addOp (UpdateFocus (FocusNode edge.source)) ops
    NoFocus -> ops

traverseDown :: List GraphOp -> List GraphOp
traverseDown ops =
  let g = buildGraph ops in
  case view (_Graph <<< _focus) g of
    FocusNode nodeId -> fromMaybe ops do
      graphNode <- lookupNode g nodeId
      let node = view _GraphNode graphNode
      childId <- (keys node.children) !! 0
      pure $ addOp (UpdateFocus (FocusEdge { source: node.id, target: childId })) ops
    FocusEdge edge -> addOp (UpdateFocus (FocusNode edge.target)) ops
    NoFocus -> ops

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
    toTheLeft = Array.reverse $ splitSortedSibCops.no <> splitSortedSibCops.yes

traverseLeft :: List GraphOp -> List GraphOp
traverseLeft ops =
  let g = buildGraph ops in
  case view (_Graph <<< _focus) g of
    FocusNode nodeId -> case lookupNode g nodeId of
      Nothing -> ops
      Just graphNode -> addOp (UpdateFocus (FocusNode (view (_GraphNode <<< _id) (nodeOnLeft g graphNode)))) ops
    FocusEdge edge -> traverseDown >>> traverseLeft >>> traverseUp $ ops
    NoFocus -> ops

nodeOnRight :: Graph -> GraphNode -> GraphNode
nodeOnRight g node = fromMaybe node $ toTheRight !! 0
  where
    siblingsAndCoparents = lookupNodes g $ siblings g node <> coparents g node
    xSortedSibCops = sortWith viewX siblingsAndCoparents
    viewX = view (_GraphNode <<< _x)
    splitSortedSibCops = partition (\n -> viewX n <= viewX node) xSortedSibCops
    toTheRight = splitSortedSibCops.no <> splitSortedSibCops.yes

traverseRight :: List GraphOp -> List GraphOp
traverseRight ops =
  let g = buildGraph ops in
  case view (_Graph <<< _focus) g of
    FocusNode nodeId -> case lookupNode g nodeId of
      Nothing -> ops
      Just graphNode -> addOp (UpdateFocus (FocusNode (view (_GraphNode <<< _id) (nodeOnRight g graphNode)))) ops
    FocusEdge edge -> traverseDown >>> traverseRight >>> traverseUp $ ops
    NoFocus -> ops


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
fromFocus (FocusEdge edge) = computeEdgeId edge

emptyListGraphOp :: List GraphOp
emptyListGraphOp = Nil

graphLength :: List GraphOp -> Int
graphLength = List.length

graphTitle :: List GraphOp -> Maybe String
graphTitle ops =
  titles !! 0 >>= stripPrefix (Pattern "Title: ")
  where
    (Graph g) = buildGraph ops
    nodeTextArr = map (view (_GraphNode <<< _text)) $ values g.nodes
    isTitle nodeText =
      contains (Pattern "Title:") nodeText
    titles = filter isTitle nodeTextArr

listOpsFromGraph :: Graph -> List GraphOp
listOpsFromGraph (Graph g) =
  List.fromFoldable $ addNodes <> addEdges <> addHighlight <> [addFocus]
  where
    addNodes = map (\node -> AddNode node) $ values g.nodes
    addEdges = foldMap (\(GraphNode node) -> map (\parentId -> AddEdge { source: node.id, target: parentId}) (keys node.parents)) $ values g.nodes
    addHighlight = map (\highlightId -> Highlight highlightId) $ keys g.highlighted
    addFocus = UpdateFocus g.focus
