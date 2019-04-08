module Main where


import Control.Monad.Except.Trans (ExceptT)
import Control.Plus (empty)
import Data.Array (mapMaybe, filter, sortWith, (!!), (..), concatMap)
import Data.Array as Array
import Data.Eq (class Eq)
import Data.Foldable (class Foldable, all, elem, foldMap, foldl, foldr, length, maximumBy)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity)
import Data.Lens (Lens', lens, view, over, set, setJust)
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.List (List(..), (:))
import Data.List as List
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(..), isJust)
import Data.Maybe as Maybe
import Data.Ord (class Ord, comparing)
import Data.String (Pattern(..), split, contains, stripPrefix, trim)
import Data.Symbol (SProxy(..))
import Data.Traversable (class Traversable, traverse, sequence)
import Data.Tuple (Tuple(..))
import Foreign (ForeignError)
import Foreign.Class (class Encode, class Decode)
import Foreign.Generic (defaultOptions, genericEncode, genericDecode, genericDecodeJSON, genericEncodeJSON)
import Foreign.Generic.Types (SumEncoding)
import Foreign.Object (Object, keys, values, size)
import Foreign.Object as Object
import Prelude (Unit, unit, ($), (<<<), map, flip, (==), compare, Ordering, append, (<>), bind, pure, (>>=), (+), negate, (<$>), class Functor, class Apply, apply, class Applicative, (<*>), class Monoid, mempty, (#), (>), (/=), mod, (<), (-))


version :: String
version = "0.0"

newNodeXOffset :: Number
newNodeXOffset = 100.0

newParentYOffset :: Number
newParentYOffset = -100.0


newNodeInitialPos :: Point2D
newNodeInitialPos = { x: 100.0, y: 100.0 }

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

graphToJSON :: Graph -> String
graphToJSON g =
  genericEncodeJSON genericEncodeOpts g

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
derive instance eqGraphNode :: Eq GraphNode

instance encodeGraphNode :: Encode GraphNode where
  encode node = genericEncode genericEncodeOpts node

instance decodeGraphNode :: Decode GraphNode where
  decode node = genericDecode genericEncodeOpts node


type Point2D = { x :: Number, y :: Number }

newtype Edge = Edge { source :: NodeId, target :: NodeId }
derive instance genericEdge :: Generic Edge _
derive instance eqEdge :: Eq Edge
instance encodeEdge :: Encode Edge where
  encode x = genericEncode genericEncodeOpts x
instance decodeEdge :: Decode Edge where
  decode x = genericDecode genericEncodeOpts x

type EdgeId = String

computeEdgeId :: Edge -> String
computeEdgeId (Edge edge) = edge.source <> "." <> edge.target

data Focus =
  FocusNode String
  | FocusEdge Edge (Array Edge)
  | NoFocus
derive instance genericFocus :: Generic Focus _
derive instance eqFocus :: Eq Focus

instance encodeFocus :: Encode Focus where
  encode x = genericEncode genericEncodeOpts x

instance decodeFocus :: Decode Focus where
  decode x = genericDecode genericEncodeOpts x


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

_Graph :: Lens' Graph { nodes :: Object GraphNode
                      ,  focus :: Focus
                      ,  highlighted :: NodeIdSet}
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

viewX :: GraphNode -> Number
viewX = view (_GraphNode <<< _x)

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
applyGraphOp (AddEdge (Edge edge)) =
  applyGraphOp (AddParent edge.target edge.source)
  <<<
  applyGraphOp (AddChild edge.source edge.target)
applyGraphOp (RemoveEdge (Edge edge)) =
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
demo = foldl (flip applyGraphOp) emptyGraph $
       UpdateFocus (FocusEdge (Edge { source: "title"
                                    , target: "goofus"})
                              [Edge {"source": "title", "target": "goofus"},
                               Edge {"source": "thingo", "target": "goofus"}])
       : AddNode (GraphNode
           { text: "Title: Workflow"
           , id : "title"
           , x : 205.0
           , y : 150.0
           , parents : emptyNodeIdSet
           , children : nodeIdSetFromArray ["goofus"]
           , subgraphNodes : Object.empty
           })
       : Highlight "thingo"
       : AddNode (GraphNode
           { text: "thingo"
           , id : "thingo"
           , x : 205.0
           , y : 100.0
           , parents : emptyNodeIdSet
           , children : nodeIdSetFromArray ["goofus"]
           , subgraphNodes : Object.empty
           })
       : AddNode (GraphNode
           { text: "asdf"
           , id : "goofus"
           , x: 450.0
           , y: 270.0
           , parents : nodeIdSetFromArray [ "thingo", "title" ]
           , children : emptyNodeIdSet
           , subgraphNodes : Object.empty
           })
       : Nil


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


deleteNode :: GraphNode -> Graph -> Graph
deleteNode (GraphNode node) g =
  applyGraphOp (RemoveNode (GraphNode node))
  $ foldl (flip applyGraphOp) g $ List.fromFoldable $ removeParentEdges <> removeChildEdges
  where
    removeParentEdges = (\parentId -> RemoveEdge (Edge { source: parentId, target: node.id })) <$> keys node.parents
    removeChildEdges = (\childId -> RemoveEdge (Edge { source: node.id, target: childId })) <$> keys node.children

removeFocus :: Graph -> Graph
removeFocus g =
  case view (_Graph <<< _focus) g of
    NoFocus -> g
    FocusNode nodeId -> case lookupNode g nodeId of
      Nothing -> g
      Just (GraphNode focusNode) ->
        let
          newFocus = case size focusNode.parents of
            0 -> case (keys focusNode.children) !! 0 of
              Nothing -> NoFocus
              Just childId -> FocusNode childId
            otherwise -> case (keys focusNode.parents) !! 0 of
              Nothing -> NoFocus
              Just parentId -> FocusNode parentId
        in applyGraphOp
          (UpdateFocus newFocus)
          $ applyGraphOp (UnHighlight focusNode.id)
          $ deleteNode (GraphNode focusNode) g
    FocusEdge (Edge edge) _ -> applyGraphOp (UpdateFocus (FocusNode edge.source))
        $ applyGraphOp (RemoveEdge (Edge edge)) g


------
-- Highlighting

toggleHighlightFocus :: Graph -> Graph
toggleHighlightFocus g =
  case view (_Graph <<< _focus) g of
    FocusNode nodeId -> case Object.member nodeId (view (_Graph <<< _highlighted) g) of
      true -> applyGraphOp (UnHighlight nodeId) g
      false -> applyGraphOp (Highlight nodeId) g
    FocusEdge edge _ -> applyGraphOp (UpdateFocus (FocusEdge edge [])) g
    _ -> g


------
-- Traversal

traverseUp :: Graph -> Graph
traverseUp g =
  case view (_Graph <<< _focus) g of
    FocusNode nodeId -> fromMaybe g do
      GraphNode node <- lookupNode g nodeId
      let upEdges' = Edge <$> { source: _, target: node.id } <$> keys node.parents
      newFocus <- upEdges' !! 0
      -- If the focus group has a single element then collapse the focus
      let upEdges = if length upEdges' == 1 then [] else upEdges'
      pure $ applyGraphOp (UpdateFocus (FocusEdge newFocus upEdges)) g
    FocusEdge (Edge edge) _ -> applyGraphOp (UpdateFocus (FocusNode edge.source)) g
    NoFocus -> g

-- TODO: remove duplicate code
traverseDown :: Graph -> Graph
traverseDown g =
  case view (_Graph <<< _focus) g of
    FocusNode nodeId -> fromMaybe g do
      GraphNode node <- lookupNode g nodeId
      let downEdges' = Edge <$> { source: node.id, target: _ } <$> keys node.children
      newFocus <- downEdges' !! 0
      -- If the focus group has a single element then collapse the focus
      let downEdges = if length downEdges' == 1 then [] else downEdges'
      pure $ applyGraphOp (UpdateFocus (FocusEdge newFocus downEdges)) g
    FocusEdge (Edge edge) _ -> applyGraphOp (UpdateFocus (FocusNode edge.target)) g
    NoFocus -> g

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

-- | Order array elements to the right of a given element,
-- | not including the given element.
toRightWrapOrdering :: forall a b. Ord b => a -> (a -> b) -> Array a -> Array a
toRightWrapOrdering x getPosition xs = xsToRight <> xsToLeft
  where
    isToRight = x # (<) `on` getPosition
    xsToRight = sortWith getPosition $ filter isToRight xs
    isToLeft = x # (>) `on` getPosition
    xsToLeft = sortWith getPosition $ filter isToLeft xs

data DirectionLR = Left | Right

-- | Gives next element in array to the left or right of a given element,
-- | wrapping back around.
nextElemWrap :: forall a b. Ord b => DirectionLR -> a -> (a -> b) -> Array a -> a
nextElemWrap Left x getPosition xs = fromMaybe x $ toTheLeft !! 0
  where
    toTheLeft = Array.reverse $ [x] <> toRightWrapOrdering x getPosition xs
nextElemWrap Right x getPosition xs = fromMaybe x $ xsOrderedToTheRight !! 0
  where
    xsOrderedToTheRight = toRightWrapOrdering x getPosition xs <> [x]

nextEdgeGroup :: DirectionLR -> Graph -> Edge -> Array Edge
nextEdgeGroup dir g (Edge edge) =
  case lookupNode g edge.source of
    Nothing -> []
    Just (GraphNode source) ->
      case lookupNode g edge.target of
        Nothing -> []
        Just (GraphNode target) ->
          let
            (GraphNode leftParentOfTarget) = nextElemWrap dir (GraphNode source) viewX $ lookupNodes g target.parents
            (GraphNode leftChildOfSource) = nextElemWrap dir (GraphNode target) viewX $ lookupNodes g source.children
            newSourceEdge = Edge { source: leftParentOfTarget.id
                                 , target: target.id }
            newTargetEdge = Edge { source: source.id
                                 , target: leftChildOfSource.id }
          in
          filter ((/=) (Edge edge)) [newSourceEdge, newTargetEdge]

nextInGroup :: forall a. Eq a => DirectionLR -> a -> Array a -> Maybe a
nextInGroup Left x xs =
  Array.elemIndex x xs >>= \xIndex -> xs !! (xIndex + 1) `mod` length xs
nextInGroup Right x xs =
  Array.elemIndex x xs >>= \xIndex -> xs !! (xIndex - 1) `mod` length xs

nextNodeWrap :: DirectionLR -> Graph -> GraphNode -> GraphNode
nextNodeWrap dir g node =
  nextElemWrap dir node viewX siblingsAndCoparents
  where
    siblingsAndCoparents = lookupNodes g $ siblings g node <> coparents g node

edgePosition :: Graph -> Edge -> Number
edgePosition g (Edge edge) = fromMaybe 0.0 do
  GraphNode source <- lookupNode g edge.source
  GraphNode target <- lookupNode g edge.target
  pure $ source.x + target.x

changeFocusLeftRight :: DirectionLR -> Graph -> Graph
changeFocusLeftRight dir g =
  case view (_Graph <<< _focus) g of
    FocusNode nodeId -> case lookupNode g nodeId of
      Nothing -> g
      Just node ->
        let
          leftNode = nextNodeWrap dir g node
        in
        applyGraphOp (UpdateFocus (FocusNode (view (_GraphNode <<< _id) leftNode))) g
    FocusEdge edge focusGroup ->
      let
        focus = if focusGroup /= []
          then -- There is an active focusGroup, keep cycling though
            FocusEdge (nextElemWrap dir edge (edgePosition g) focusGroup) focusGroup
          else -- Make a new focus group
            let
              newGroup' = nextEdgeGroup dir g edge
              newFocus = fromMaybe edge (newGroup' !! 0)
              -- If the focus group has a single element then collapse the focus
              newGroup = if length newGroup' == 1 then [] else newGroup'
            in
            FocusEdge newFocus newGroup
      in
      applyGraphOp (UpdateFocus focus) g
    NoFocus -> g

traverseLeft :: Graph -> Graph
traverseLeft g = changeFocusLeftRight Left g

traverseRight :: Graph -> Graph
traverseRight g = changeFocusLeftRight Right g


------
-- Positioning

rightmostNode :: forall f. Foldable f => f GraphNode -> Maybe GraphNode
rightmostNode = maximumBy (comparing viewX)

newPositionFrom :: Graph -> GraphNode -> (GraphNode -> NodeIdSet) -> Point2D
newPositionFrom g (GraphNode node) relations =
  fromMaybe { x: node.x, y: node.y + newParentYOffset } do
    (GraphNode rightmostParent) <- rightmostNode $ lookupNodes g $ relations $ GraphNode node
    pure { x: rightmostParent.x + newNodeXOffset
         , y: rightmostParent.y }

--newChildPosition :: Graph -> GraphNode -> Point2D
--newChildPosition g (GraphNode node) =
--  fromMaybe newNodeInitialPos do
--    (GraphNode rightmostChild) <- rightmostNode $ lookupNodes g node.children
--    pure { x: rightmostChild.x + newNodeXOffset, y: rightmostChild.y }
--
--newParentPosition :: Graph -> GraphNode -> Point2D
--newParentPosition g (GraphNode node) =
--  fromMaybe newNodeInitialPos do
--    (GraphNode rightmostParent) <- rightmostNode $ lookupNodes g node.parents
--    pure { x: rightmostParent.x + newNodeXOffset, y: rightmostParent.y }


------
-- Utilities

lookupNode :: Graph -> NodeId -> Maybe GraphNode
lookupNode g nodeId = view (_Graph <<< _nodes <<< at nodeId) g

lookupNodes :: Graph -> NodeIdSet -> Array GraphNode
lookupNodes g nodeIds = mapMaybe (lookupNode g) $ keys nodeIds

lookupEdge :: Graph -> EdgeId -> Maybe Edge
lookupEdge g edgeId =
  let edgeNodeIds = split (Pattern ".") edgeId
      sourceId = fromMaybe "" $ edgeNodeIds !! 0
      targetId = fromMaybe "" $ edgeNodeIds !! 1
  in
  case all isJust [lookupNode g sourceId, lookupNode g targetId] of
    false -> Nothing
    true -> Just $ Edge { source : sourceId, target: targetId }

------
-- Graph Queries

terminalestNode :: Array GraphNode -> Maybe GraphNode
terminalestNode nodes = case filter isTerminal nodes of
  [] -> maximumBy (comparing getY) nodes
  terminalNodes -> maximumBy mostParentsLowest terminalNodes
  where
    mostParentsLowest :: GraphNode -> GraphNode -> Ordering
    mostParentsLowest a b = compare (nParents a) (nParents b) `append` lower a b
    lower :: GraphNode -> GraphNode -> Ordering
    lower = comparing getY
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
fromFocus (FocusEdge edge _) = computeEdgeId edge

edgeInFocusGroup :: Graph -> Edge -> Boolean
edgeInFocusGroup (Graph g) edge =
  case g.focus of
    FocusEdge _ focusGroup -> elem edge focusGroup
    _ -> false

getParents :: GraphNode -> NodeIdSet
getParents (GraphNode node) = node.parents

getChildren :: GraphNode -> NodeIdSet
getChildren (GraphNode node) = node.children

graphTitle :: Graph -> Maybe String
graphTitle (Graph g) = titles !! 0 >>= stripPrefix titlePattern
  where
    titlePattern = Pattern "Title: "
    nodeTextArr = trim <$> (view (_GraphNode <<< _text)) <$> values g.nodes
    isTitle = contains titlePattern
    titles = filter isTitle nodeTextArr


------
-- Purescript by example

combineMaybe :: forall a f. Applicative f => Maybe (f a) -> f (Maybe a)
combineMaybe Nothing = pure Nothing
combineMaybe (Just fa) = map Just fa

data Tree a = Leaf | Branch (Tree a) a (Tree a)

instance functorTree :: Functor Tree where
  map _ Leaf = Leaf
  map f (Branch left val right) = Branch (map f left) (f val) (map f right)

instance applyTree :: Apply Tree where
  apply Leaf _ = Leaf
  apply _ Leaf = Leaf
  apply (Branch leftFunc valFunc rightFunc) (Branch left val right) = Branch (apply leftFunc left) (valFunc val) (apply rightFunc right)

instance applicativeTree :: Applicative Tree where
  pure val = Branch Leaf val Leaf

instance foldableTree :: Foldable Tree where
  foldl _ init Leaf = init
  foldl f init (Branch left val right) = foldl f (f (foldl f init left) val) right
  foldr _ init Leaf = init
  foldr f init (Branch left val right) = foldr f (f val (foldr f init right)) left
  foldMap :: forall a m. Monoid m => (a -> m) -> Tree a -> m
  foldMap _ Leaf = mempty
  foldMap f (Branch left val right) = foldMap f left `append` f val `append` foldMap f right

instance traverseTree :: Traversable Tree where
  -- traverse :: (a -> g b) -> Tree a -> g (Tree b)
  traverse _ Leaf = pure Leaf
  traverse f (Branch left val right) = Branch <$> traverse f left <*> f val <*> traverse f right
  --traverse f t = sequence $ f <$> t
  --sequence :: Tree (g a) -> g (Tree a)
  sequence Leaf = pure Leaf
  sequence (Branch left val right) = Branch <$> sequence left <*> val <*> sequence right


countThrows :: Int -> Array (Array Int)
countThrows n = do
  x <- 1 .. 6
  y <- 1 .. 6
  if x + y == n
    then pure [x, y]
    else empty

countThrowsDesugar :: Int -> Array (Array Int)
countThrowsDesugar n =
  bind (1 .. 6) \x ->
    bind (1 .. 6) \y ->
      if x + y == n
         then pure [x, y]
         else empty

countThrowsDebind :: Int -> Array (Array Int)
countThrowsDebind n =
  flip concatMap (1 .. 6) \x ->
    flip concatMap (1 .. 6) \y ->
      if x + y == n
        then [[x, y]]
        else empty


newtype Asdf = Asdf { a :: Int, s :: String, d :: Number, f :: Asdf }

hasImmute :: Asdf -> Asdf
hasImmute (Asdf asdf) = Asdf $ asdf { f = Asdf (sdfa { f = Asdf (dfas { a = 20 })}) }
  where
    Asdf sdfa = asdf.f
    Asdf dfas = sdfa.f
