module Workflow.Interaction where

import Data.Array (filter, sortWith, (!!))
import Data.Array as Array
import Data.Eq (class Eq)
import Data.Foldable (foldMap, foldl, length)
import Data.Function (on)
import Data.Lens (view)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Ord (class Ord)
import Foreign.Object (keys, size)
import Foreign.Object as Object
import Prelude (($), (<<<), map, flip, (==), (<>), bind, pure, (>>=), (+), (<$>), (#), (>), (/=), mod, (<), (-))
import Effect (Effect)

import Workflow.Core


--| Whereas a GraphOp represents a fundamental graph operation,
--| a GraphInteraction represents an abstract relation between
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

-- TODO: add to interaction typeclass interface
addNode :: Point2D -> NodeIdSet -> NodeIdSet -> Graph -> Effect Graph
addNode xyPos parentIds childIds g = do
  newNode <- createGraphNode xyPos parentIds childIds
  let newNodeId = (view (_GraphNode <<< _id) newNode)
  let addParentEdges = map (\parentId -> AddChild parentId newNodeId) $ keys parentIds
  let addChildEdges = map (\childId -> AddParent childId newNodeId) $ keys childIds
  pure $ foldl (flip applyGraphOp) g
    $ [AddNode newNode] <> addParentEdges <> addChildEdges
    <> [UpdateFocus (FocusNode newNodeId)]


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
