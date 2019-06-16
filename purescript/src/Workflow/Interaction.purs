module Workflow.Interaction where

import Workflow.Core

import Data.Array (filter, sortWith, (!!), concatMap, catMaybes, null)
import Data.Array as Array
import Data.Eq (class Eq)
import Data.Foldable (foldMap, foldl, length, class Foldable, maximumBy, elem)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord (class Ord, comparing)
import Data.String (Pattern(..), contains, stripPrefix, trim)
import Effect (Effect)
import Foreign.Class (class Encode, class Decode)
import Foreign.Generic (genericEncode, genericDecode)
import Foreign.Generic.Class (class GenericDecode, class GenericEncode)
import Foreign.Object (keys, values)
import Foreign.Object as Object
import Prelude (($), (<<<), (>>>), map, flip, (==), (<>), bind, pure, (>>=), (+), (<$>), (#), (>), (/=), mod, (<), (-), not, (&&), (/))


type Point2D = { x :: Number, y :: Number }

data Focus edge =
  FocusNode NodeId
  | FocusEdge edge (Array edge)
  | NoFocus

class (Node node) <=
      InterNode node where
  viewPos :: node -> Point2D
  moveNode :: Point2D -> node -> node
  viewText :: node -> String

class (Graph graph node edge, InterNode node) <=
      InterGraph graph node edge | graph -> node, graph -> edge where
  updateNodePosition :: Point2D -> NodeId -> graph -> graph
  moveNodeAmount :: Point2D -> node -> graph -> graph
  viewFocus :: graph -> Focus edge
  updateFocus :: Focus edge -> graph -> graph
  viewHighlighted :: graph -> NodeIdSet
  highlight :: NodeId -> graph -> graph
  unHighlight :: NodeId -> graph -> graph
  updateText :: NodeId -> String -> graph -> graph

class (InterGraph graph node edge) <= ValidatableGraph graph node edge | graph -> node, graph -> edge where
  updateNodeValidity :: Boolean -> NodeId -> graph -> graph
  updateEdgeValidity :: Boolean -> edge -> graph -> graph


------
-- Basic operations

add :: Point2D -> Point2D -> Point2D
add a b = {x: a.x + b.x, y: a.y + b.y}

subtract :: Point2D -> Point2D -> Point2D
subtract a b = {x: a.x - b.x, y: a.y - b.y}

viewX :: forall node. InterNode node => node -> Number
viewX node = (viewPos node).x

viewY :: forall node. InterNode node => node -> Number
viewY node = (viewPos node).y


------
-- Generic serialisation boilerplate

derive instance genericFocus :: (Generic edge rep) => Generic (Focus edge) _
derive instance eqFocus :: (Eq edge) => Eq (Focus edge)
instance encodeFocus :: (Generic edge rep, GenericEncode rep, Encode edge) => Encode (Focus edge) where
  encode x = genericEncode genericEncodeOpts x
instance decodeFocus :: (Generic edge rep, GenericDecode rep, Decode edge) => Decode (Focus edge) where
  decode x = genericDecode genericEncodeOpts x


------
-- Focusing

removeFocus :: forall graph node edge.
               InterGraph graph node edge =>
               graph -> graph
removeFocus g =
  case viewFocus g of
    NoFocus -> g
    FocusNode nodeId -> case lookupNode nodeId g of
      Nothing -> g
      Just focusNode ->
        let
          newFocus = case (keys (viewParents focusNode)) !! 0 of
            Nothing -> case (keys (viewChildren focusNode)) !! 0 of
              Nothing -> NoFocus
              Just childId -> FocusNode childId
            Just parentId -> FocusNode parentId
        in updateFocus newFocus
          $ unHighlight (viewId focusNode)
          $ removeNode (viewId focusNode) g
    FocusEdge edge _ ->
      let
        source = edgeSource edge
        target = edgeTarget edge
      in
        updateFocus (FocusNode source)
        $ removeEdge source target g

edgeInFocusGroup :: forall graph node edge.
                    Eq edge =>
                    InterGraph graph node edge =>
                    graph -> edge -> Boolean
edgeInFocusGroup g edge =
  case viewFocus g of
    FocusEdge _ focusGroup -> elem edge focusGroup
    _ -> false


------
-- Highlighting

clearHighlighted :: forall graph node edge.
                    InterGraph graph node edge =>
                    graph -> graph
clearHighlighted g = foldl (flip unHighlight) g $ keys $ viewHighlighted g

highlightFocus :: forall graph node edge.
                  InterGraph graph node edge =>
                  graph -> graph
highlightFocus g = case viewFocus g of
  FocusNode nodeId -> highlight nodeId g
  _ -> g

unHighlightFocus :: forall graph node edge.
                    InterGraph graph node edge =>
                    graph -> graph
unHighlightFocus g = case viewFocus g of
  FocusNode nodeId -> unHighlight nodeId g
  _ -> g

toggleHighlightFocus :: forall graph node edge.
                        InterGraph graph node edge =>
                        graph -> graph
toggleHighlightFocus g = case viewFocus g of
  FocusNode nodeId ->
    case Object.member nodeId (viewHighlighted g) of
      true -> unHighlight nodeId g
      false -> highlight nodeId g
  FocusEdge edge _ -> updateFocus (FocusEdge edge []) g
  _ -> g


------
-- Traversal

traverseUp :: forall graph node edge.
              InterGraph graph node edge =>
              graph -> graph
traverseUp g =
  case viewFocus g of
    FocusNode nodeId -> fromMaybe g do
      node <- lookupNode nodeId g
      let upEdges' = catMaybes do
            parentId <- keys $ viewParents node
            pure $ lookupEdge parentId nodeId g
      newFocus <- upEdges' !! 0
      -- If the focus group has a single element then collapse the focus
      let upEdges = if length upEdges' == 1 then [] else upEdges'
      pure $ updateFocus (FocusEdge newFocus upEdges) g
    FocusEdge edge _ -> updateFocus (FocusNode (edgeSource edge)) g
    NoFocus -> g

-- TODO: remove duplicate code
traverseDown :: forall graph node edge.
                InterGraph graph node edge =>
                graph -> graph
traverseDown g =
  case viewFocus g of
    FocusNode nodeId -> fromMaybe g do
      node <- lookupNode nodeId g
      let downEdges' = catMaybes do
            childId <- keys $ viewChildren node
            pure $ lookupEdge (viewId node) childId g
      -- If the focus group has a single element then collapse the focus
      newFocus <- downEdges' !! 0
      let downEdges = if length downEdges' == 1 then [] else downEdges'
      pure $ updateFocus (FocusEdge newFocus downEdges) g
    FocusEdge edge _ -> updateFocus (FocusNode (edgeTarget edge)) g
    NoFocus -> g

siblings :: forall graph node edge.
            InterGraph graph node edge =>
            graph -> node -> NodeIdSet
siblings g node = foldMap viewChildren parents
  where
    parents = lookupNodes parentIds g
    parentIds = viewParents node

coparents :: forall graph node edge.
             InterGraph graph node edge =>
             graph -> node -> NodeIdSet
coparents g node = foldMap viewParents children
  where
    children = lookupNodes childIds g
    childIds = viewChildren node

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

nextEdgeGroup :: forall graph node edge.
                 InterGraph graph node edge =>
                 DirectionLR -> graph -> edge -> Array edge
nextEdgeGroup dir g edge =
  fromMaybe [] $ do
    source <- lookupNode (edgeSource edge) g
    target <- lookupNode (edgeTarget edge) g
    pure $ let
      nextParentOfTarget =
        nextElemWrap dir source viewX
        $ lookupNodes (viewParents target) g
      nextChildOfSource =
        nextElemWrap dir target viewX
        $ lookupNodes (viewChildren source) g
      newSourceEdge = lookupEdge (viewId nextParentOfTarget) (viewId target) g
      newTargetEdge = lookupEdge (viewId source) (viewId nextChildOfSource) g
      in
        filter (\newEdge ->
                 edgeSource edge /= edgeSource newEdge &&
                 edgeTarget edge /= edgeTarget newEdge)
        $ catMaybes [newSourceEdge, newTargetEdge]

nextInGroup :: forall a. Eq a => DirectionLR -> a -> Array a -> Maybe a
nextInGroup Left x xs =
  Array.elemIndex x xs >>= \xIndex -> xs !! (xIndex + 1) `mod` length xs
nextInGroup Right x xs =
  Array.elemIndex x xs >>= \xIndex -> xs !! (xIndex - 1) `mod` length xs

nextNodeWrap :: forall graph node edge.
                InterGraph graph node edge =>
                DirectionLR -> graph -> node -> node
nextNodeWrap dir g node =
  nextElemWrap dir node viewX siblingsAndCoparents
  where
    siblingsAndCoparents = lookupNodes (siblings g node <> coparents g node) g

edgePosition :: forall graph node edge.
                InterGraph graph node edge =>
                graph -> edge -> Number
edgePosition g edge =
  fromMaybe 0.0 do
    source <- lookupNode (edgeSource edge) g
    target <- lookupNode (edgeTarget edge) g
    pure $ (viewX source) + (viewX target) / 2.0

changeFocusLeftRight :: forall graph node edge.
                        InterGraph graph node edge =>
                        DirectionLR -> graph -> graph
changeFocusLeftRight dir g =
  case viewFocus g of
    FocusNode nodeId -> case lookupNode nodeId g of
      Nothing -> g
      Just node ->
        let
          leftNode = nextNodeWrap dir g node
        in
        updateFocus (FocusNode (viewId leftNode)) g
    FocusEdge edge focusGroup ->
      let
        focus = if not $ null focusGroup
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
        updateFocus focus g
    NoFocus -> g

traverseLeft :: forall graph node edge.
                InterGraph graph node edge =>
                graph -> graph
traverseLeft g = changeFocusLeftRight Left g

traverseRight :: forall graph node edge.
                 InterGraph graph node edge =>
                 graph -> graph
traverseRight g = changeFocusLeftRight Right g


------
-- Positioning nodes
--

rightmostNode :: forall f node.
                 InterNode node =>
                 Foldable f =>
                 f node -> Maybe node
rightmostNode = maximumBy (comparing (viewX))

--newPositionFrom ::  -> InterNodeImpl -> (InterNodeImpl -> NodeIdSet) -> Point2D
--newPositionFrom g (InterNodeImpl node) relations =
--  fromMaybe { x: node.x, y: node.y + newParentYOffset } do
--    (InterNodeImpl rightmostParent) <- rightmostNode $ lookupNodes (relations $ InterNodeImpl node) g
--    pure { x: rightmostParent.x + newNodeXOffset
--         , y: rightmostParent.y }

newChildPosition :: forall graph node edge.
                    InterGraph graph node edge =>
                    Point2D -> graph -> node -> Point2D
newChildPosition offset g node =
  fromMaybe { x: viewX node, y: (viewY node) + offset.y } do
    rightmostChild <- rightmostNode $ lookupNodes (viewChildren node) g
    pure { x: (viewX rightmostChild) + offset.x, y: (viewY rightmostChild) }

newParentPosition :: forall graph node edge.
                     InterGraph graph node edge =>
                     Point2D -> graph -> node -> Point2D
newParentPosition offset g node =
  fromMaybe { x: viewX node, y: (viewY node) - offset.y } do
    rightmostParent <- rightmostNode $ lookupNodes (viewParents node) g
    pure { x: (viewX rightmostParent) + offset.x, y: viewY rightmostParent }

newChildOfFocus :: forall graph node edge.
                   InterGraph graph node edge =>
                   Point2D -> graph -> Effect graph
newChildOfFocus offset g = case viewFocus g of
  FocusNode nodeId -> case lookupNode nodeId g of
    Nothing -> pure g
    Just node ->
      let
        newChildPos = newChildPosition offset g node
        newChildParents = nodeIdSetFromArray $ [viewId node]
      in do
        newChildNode' <- createNode Object.empty Object.empty
        let newChildNode = moveNode newChildPos newChildNode'
        let newEdge = createEdge nodeId (viewId newChildNode)
        pure $ ((updateFocus (FocusNode (viewId newChildNode))) <<<
                (addEdge newEdge) <<<
                (insertNode newChildNode))
             $ g
  _ -> pure g

newParentOfFocus :: forall graph node edge.
                    InterGraph graph node edge =>
                    Point2D -> graph -> Effect graph
newParentOfFocus offset g = case viewFocus g of
  FocusNode nodeId -> case lookupNode nodeId g of
    Nothing -> pure g
    Just node ->
      let
        newParentPos = newParentPosition offset g node
        newParentChildren = nodeIdSetFromArray $ [viewId node]
      in do
        newParentNode' <- createNode Object.empty Object.empty
        let newParentNode = moveNode newParentPos newParentNode'
        let newEdge = createEdge (viewId newParentNode) nodeId
        pure $ ((updateFocus (FocusNode (viewId newParentNode)))
                <<<
                (addEdge newEdge)
                <<<
                (insertNode newParentNode))
             $ g
  _ -> pure g


------
-- Subgraph collapse/expand

parentsOfGroup :: forall node. Node node => Array node -> NodeIdSet
parentsOfGroup nodes =
  let
    allParents = concatMap (viewParents >>> keys) nodes
    nodesIds = map viewId nodes
  in
    nodeIdSetFromArray $ filter (not <<< flip Array.elem nodesIds) allParents

childrenOfGroup :: forall node. Node node => Array node -> NodeIdSet
childrenOfGroup nodes =
  let
    allChildren = concatMap (viewChildren >>> keys) nodes
    nodesIds = map viewId nodes
  in
    nodeIdSetFromArray $ filter (not <<< flip Array.elem nodesIds) allChildren

groupHighlighted ::forall graph node edge.
                   InterGraph graph node edge =>
                   graph -> graph
groupHighlighted g =
  case do
    defaultGroupNodeId <- Array.index (keys (viewHighlighted g)) 0
    lookupNode defaultGroupNodeId g
  of
    Nothing -> g
    Just defaultGroupNode ->
      let
        groupNode = case viewFocus g of
          FocusNode nodeId ->
            if Array.elem nodeId $ keys $ viewHighlighted g
               then case lookupNode nodeId g of
                 Just focusNode -> focusNode
                 Nothing -> defaultGroupNode
               else defaultGroupNode
          _ -> defaultGroupNode
        subgraphNodes = lookupNodes (viewHighlighted g) g
        unglued = unglue subgraphNodes g
        groupParents = parentsOfGroup subgraphNodes
        groupChildren = childrenOfGroup subgraphNodes
        groupNodeId = viewId groupNode
        parentGraph = updateFocus (FocusNode groupNodeId) unglued.parentGraph
        parentGraph' = clearHighlighted parentGraph
        parentEdges = (\parentId -> createEdge parentId groupNodeId)
                      <$> keys groupParents
        childEdges = (\childId -> createEdge groupNodeId childId)
                     <$> keys groupChildren
        groupExternalEdges = parentEdges <> childEdges
        subgraph = updateFocus (FocusNode groupNodeId) unglued.childGraph
        subgraph' = clearHighlighted subgraph
        groupInterNode =
          replaceSubgraph subgraph' groupNode
      in
       (insertNode groupInterNode
        >>>
        (replaceEdges groupNodeId groupExternalEdges))
       $ parentGraph'

expandFocus :: forall graph node edge.
               InterGraph graph node edge =>
               graph -> graph
expandFocus g =
  case viewFocus g of
    FocusNode groupNodeId ->
      case lookupNode groupNodeId g of
        Nothing -> g
        Just groupNode ->
          let subgraph = viewSubgraph groupNode in
          if Object.isEmpty (nodes subgraph) then g else
          let
            groupNodePos = viewPos groupNode
            subgraphGroupNodePos = case lookupNode groupNodeId subgraph of
              Nothing -> viewPos groupNode
              Just subgraphNode -> viewPos subgraphNode
            groupMovement = groupNodePos `subtract` subgraphGroupNodePos
            movedSubgraph = foldl
                            (\graph_ node -> moveNodeAmount groupMovement node graph_)
                            subgraph
                            $ values $ nodes subgraph
            glued = glue movedSubgraph $ removeNode (viewId groupNode) g
            glued' = updateFocus (FocusNode groupNodeId) glued
            glued'' = clearHighlighted glued'
            newHighlighted = keys $ nodes subgraph
            glued''' = foldl (flip highlight) glued'' newHighlighted
          in
            glued'''
    _ -> g

toggleGroupExpand :: forall graph node edge.
                     InterGraph graph node edge =>
                     graph -> graph
toggleGroupExpand g = case length (viewHighlighted g) of
  0 -> expandFocus g
  _ -> groupHighlighted g


------
-- Text

graphTitle :: forall graph node edge.
              InterGraph graph node edge =>
              graph -> Maybe String
graphTitle g = titles !! 0 >>= stripPrefix titlePattern
  where
    titlePattern = Pattern "Title: "
    nodeTextArr = trim <$> viewText <$> values (nodes g)
    isTitle = contains titlePattern
    titles = filter isTitle nodeTextArr
