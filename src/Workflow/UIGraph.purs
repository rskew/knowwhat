module Workflow.UIGraph where

import Prelude

import Control.Alt ((<|>))
import Point2D (Point2D)
import Data.Array (sortWith, (!!), null)
import Data.Array as Array
import Data.Foldable (length, class Foldable, maximumBy, minimumBy, elem, foldr)
import Data.Function (on)
import Data.Lens (view, (^.), (.~), (%~), toListOf, traversed)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String as String
import Data.UUID (genUUID)
import Effect (Effect)
import Math as Math
import Workflow.Core (EdgeId, NodeId, _edgeId, _nodeId, _isDual, _nodes, _source, _subgraph, _target, allEdges, deleteEdgeId, deleteNode, glue, insertEdge, insertNode, lookupChildren, lookupCoparents, lookupEdgesBetweenGraphs, lookupIncomingEdges, lookupNode, lookupOutgoingEdges, lookupParents, lookupSiblings, unglue, withDual)
import Workflow.UIGraph.Types (UIGraph(..), UINode(..), UIEdge(..), Focus(..), _focus, _highlighted, _pos, _x, _y, _nodeText)


freshUIEdge :: EdgeId -> UIEdge
freshUIEdge edgeId = UIEdge
                     { id : edgeId
                     , text : ""
                     , isValid : true
                     }

freshUINode :: Effect UINode
freshUINode = genUUID >>= \id -> pure $
              UINode
              { id : id
              , children : Map.empty
              , parents : Map.empty
              , subgraph : emptyUIGraph
              , position : { x : 0.0, y : 0.0 }
              , text : ""
              , isValid : true
              }

emptyUIGraph :: UIGraph
emptyUIGraph = UIGraph
  { nodes : Map.empty
  , isDual : false
  , focus : NoFocus
  , highlighted : Set.empty
  }


------
-- Focusing

removeFocus :: UIGraph -> UIGraph
removeFocus graph =
  case graph ^. _focus of
    NoFocus -> graph
    FocusEdge edgeId _ ->
      graph
      # _focus .~ FocusNode edgeId.source
      # deleteEdgeId edgeId
    FocusNode nodeId ->
      case lookupNode graph nodeId of
        Nothing -> graph
        Just focusNode ->
          let
            maybeNewFocus =     Set.findMin (lookupParents graph focusNode)
                            <|> Set.findMin (lookupChildren graph focusNode)
            newFocus = fromMaybe NoFocus $ (FocusNode <<< view _nodeId) <$> maybeNewFocus
          in
            graph
            # _focus .~ newFocus
            # _highlighted %~ Set.delete (focusNode ^. _nodeId)
            # deleteNode focusNode

edgeInFocusGroup :: UIGraph -> UIEdge -> Boolean
edgeInFocusGroup graph edge =
  case graph ^. _focus of
    FocusEdge _ focusGroup -> elem (edge ^. _edgeId) focusGroup
    _ -> false


------
-- Highlighting

highlightFocus :: UIGraph -> UIGraph
highlightFocus graph = case graph ^. _focus of
  FocusNode nodeId -> graph # _highlighted %~ Set.insert nodeId
  _ -> graph

unHighlightFocus :: UIGraph -> UIGraph
unHighlightFocus graph = case graph ^. _focus of
  FocusNode nodeId -> graph # _highlighted %~ Set.delete nodeId
  _ -> graph

toggleHighlightFocus :: UIGraph -> UIGraph
toggleHighlightFocus graph = case graph ^. _focus of
  FocusNode nodeId ->
    case Set.member nodeId (graph ^. _highlighted) of
      true -> unHighlightFocus graph
      false -> highlightFocus graph
  FocusEdge edge _ -> graph # _focus .~ (FocusEdge edge [])
  _ -> graph


--------
---- Traversal

-- | Move the focus to the element 'above', using the graph connectivity for
-- | direction rather than spatial direction.
-- |
-- | case focus of:
-- |   nothing in focus: no-op
-- |   edge in focus: move focus to source node
-- |   node in focus:
-- |     move the focus to on of the incident edges, but keep track of all
-- |     the other incident edges, which can be cycled through by left/right
-- |     motion to select the intended edge.
traverseUp :: UIGraph -> UIGraph
traverseUp graph =
  case graph ^. _focus of
    NoFocus -> graph
    FocusEdge edgeId _ -> graph # _focus .~ FocusNode edgeId.source
    FocusNode nodeId -> fromMaybe graph do
      node <- Map.lookup nodeId $ graph ^. _nodes
      let upEdges' = Array.fromFoldable $ lookupIncomingEdges graph node
      newFocusEdge <- upEdges' !! 0
      -- If the focus group has a single element then collapse the focus
      let upEdges = if length upEdges' == 1 then [] else upEdges'
      let newFocus = FocusEdge (newFocusEdge ^. _edgeId) (view _edgeId <$> upEdges)
      pure $ graph # _focus .~ newFocus

-- | Dual of traverseUp, in the sense of flipping the direction
-- | of all the edges.
traverseDown :: UIGraph -> UIGraph
traverseDown = withDual traverseUp

data DirectionLR = MoveLeft | MoveRight

edgePosition :: UIGraph -> UIEdge -> Point2D
edgePosition graph edge =
  fromMaybe { x : 0.0, y : 0.0 } $ do
    source <- lookupNode graph (edge ^. _source)
    target <- lookupNode graph (edge ^. _target)
    pure $ { x : ((source ^. _pos <<< _x) + (target ^. _pos <<< _x)) / 2.0
           , y : ((source ^. _pos <<< _y) + (target ^. _pos <<< _y)) / 2.0
           }

-- | The next edge group from an edge is the edges that share a source or target.
nextEdgeGroup :: DirectionLR -> UIGraph -> EdgeId -> Array UIEdge
nextEdgeGroup dir graph edgeId =
  fromMaybe [] $ do
    source <- lookupNode graph edgeId.source
    target <- lookupNode graph edgeId.target
    pure
      $ sortWith (edgePosition graph)
      $ Array.fromFoldable
      $ lookupIncomingEdges graph target
      <> lookupOutgoingEdges graph source

nextInGroup :: forall a. Eq a => DirectionLR -> a -> Array a -> a
nextInGroup MoveLeft x xs = fromMaybe x $
  Array.elemIndex x xs >>= \xIndex -> xs !! (xIndex + 1) `mod` length xs
nextInGroup MoveRight x xs = fromMaybe x $
  Array.elemIndex x xs >>= \xIndex -> xs !! (xIndex - 1) `mod` length xs

-- | Pseudo code for functionality
-- | Aim for code to be close to the pseudcode


-- | Changing the focus to the node or edge that is spatially on the right/left.

-- | case current focus:
-- |   Nothing in focus: no-op
-- |   Node in focus:
-- |     get the nodes that share a parent or child, and pick the node
-- |     on the left or right (depending on `dir` parameter) that is closest
-- |   Edge:
-- |     if there is an edge set in focus, then keep cycling through these,
-- |     otherwise find the edges between the target node's parents and the source
-- |       node's children, and pick the closest edge on the left or right
-- |       (depending on the input `dir` parameter) according to the edge's centroid.
changeFocusLeftRight :: DirectionLR -> UIGraph -> UIGraph
changeFocusLeftRight dir graph =
  case graph ^. _focus of
    NoFocus -> graph
    FocusNode nodeId -> case lookupNode graph nodeId of
      Nothing -> graph
      Just node ->
        let
          siblingsAndCoparents = Array.fromFoldable $ lookupSiblings graph node <> lookupCoparents graph node
          viewX = view $ _pos <<< _x
          sortedByX = sortWith viewX siblingsAndCoparents
          nextNode = fromMaybe node $ do
            nextNodeIndex <- case dir of
              MoveLeft -> sortedByX # Array.findLastIndex (node # (>) `on` viewX)
              MoveRight -> sortedByX # Array.findIndex (node # (<=) `on` viewX)
            sortedByX !! nextNodeIndex
        in
          graph # _focus .~ FocusNode (nextNode ^. _nodeId)
    FocusEdge edgeId focusGroup ->
      let
        newFocus = if not $ null focusGroup
          then -- There is an active focusGroup, keep cycling though
            let
              nextEdgeId = nextInGroup dir edgeId focusGroup
            in
              FocusEdge nextEdgeId focusGroup
          else -- Make a new focus group
            let
              newGroup' = view _edgeId <$> nextEdgeGroup dir graph edgeId
              newFocusEdgeId = fromMaybe edgeId $ newGroup' !! 0
              -- If the focus group has a single element then collapse the focus
              newGroup = if length newGroup' == 1 then [] else newGroup'
            in
            FocusEdge newFocusEdgeId newGroup
      in
        graph # _focus .~ newFocus

traverseLeft :: UIGraph -> UIGraph
traverseLeft = changeFocusLeftRight MoveLeft

traverseRight :: UIGraph -> UIGraph
traverseRight = changeFocusLeftRight MoveRight


------
-- Positioning nodes
--

rightmostNode :: forall f. Foldable f =>
                 f UINode -> Maybe UINode
rightmostNode = maximumBy $ comparing $ view $ _pos <<< _x

newChildPosition :: Point2D -> UINode -> UIGraph -> Point2D
newChildPosition offset node graph =
  let
    defaultChildPos = { x: node ^. _pos <<< _x
                      , y: (node ^. _pos <<< _y) + offset.y
                      }
  in
    fromMaybe defaultChildPos do
      rightmostChild <- rightmostNode $ lookupChildren graph node
      pure { x: (rightmostChild ^. _pos <<< _x) + offset.x
           , y: (rightmostChild ^. _pos <<< _y)
           }

newParentPosition :: Point2D -> UINode -> UIGraph -> Point2D
newParentPosition offset node graph =
  graph
  # _isDual %~ not
  # newChildPosition { x : offset.x, y : -offset.y } node

newChildOfFocus :: Point2D -> UIGraph -> Effect UIGraph
newChildOfFocus offset graph = case graph ^. _focus of
  FocusNode nodeId -> case lookupNode graph nodeId of
    Nothing -> pure graph
    Just node ->
      let
        newChildPos = newChildPosition offset node graph
        newChildParents = Set.fromFoldable $ [node ^. _nodeId]
      in do
        newChildNode' <- freshUINode
        let
          newChildNode = newChildNode' # _pos .~ newChildPos
          newEdge = freshUIEdge { source : nodeId, target : newChildNode ^. _nodeId }
        pure $ graph
             # insertNode newChildNode
             # insertEdge newEdge
             # _focus .~ FocusNode (newChildNode ^. _nodeId)
  _ -> pure graph

newParentOfFocus :: Point2D -> UIGraph -> Effect UIGraph
newParentOfFocus offset graph =
  graph
  # _isDual %~ not
  # newChildOfFocus { x : offset.x, y : -offset.y }


------
-- Subgraph collapse/expand

-- | Handle UIGraph extensions (i.e. highlighing, focus) for the glue operation
-- |
-- | merge highlighted
-- | take focus from first arg
glueUI :: UIGraph -> UIGraph -> UIGraph
glueUI graphA graphB =
  glue graphA graphB
  # _highlighted .~ Set.union (graphA ^. _highlighted) (graphB ^. _highlighted)
  # _focus .~ graphA ^. _focus

-- | Logic for handling focus and highlighted nodes in the unglue operation
-- |
-- | If the focused element is in the parent or child graph then keep it there,
-- | and for the other give it NoFocus.
-- | remove nodes in the childGraph from the parentGraph's highlighted set
-- | add the highlighted nodes in the childGraph to its highlighted set
-- TODO: quickcheck test for unglueUI >>> glueUI
unglueUI :: Set UINode -> UIGraph -> { parentGraph :: UIGraph, childGraph :: UIGraph }
unglueUI nodes graph =
  let
    unglued = unglue nodes graph
    propagateHighlighted subgraph =
      subgraph # _highlighted .~ Set.filter
        (\nodeId -> List.elem nodeId (toListOf (_nodes <<< traversed <<< _nodeId) subgraph))
        (graph ^. _highlighted)
    propagateFocus subgraph = subgraph # _focus .~
      case graph ^. _focus of
        NoFocus -> NoFocus
        FocusEdge edgeId focusGroup ->
          let
            subgraphEdgeIds = Array.fromFoldable (allEdges subgraph) <#> view _edgeId
            focusGroupInSubgraph = focusGroup # Array.filter ((flip Array.elem) subgraphEdgeIds)
          in
            if Array.elem edgeId subgraphEdgeIds
            then FocusEdge edgeId focusGroupInSubgraph
            else NoFocus
        FocusNode nodeId ->
          if Set.member nodeId $ Map.keys (subgraph ^. _nodes)
          then FocusNode nodeId
          else NoFocus
  in
    { parentGraph : unglued.parentGraph # propagateHighlighted >>> propagateFocus
    , childGraph : unglued.childGraph # propagateHighlighted >>> propagateFocus
    }



-- | Take the highlighted nodes and extract them into a subgraph
-- | of the focus node (making the focus node a 'group node')
-- | if the focus is in the highlighted group,
-- | otherwise just pick an arbitrary node to be the group node.
-- |
-- | pick a group node:
-- |   if the focus node is highlighted, pick that
-- |   otherwise pick the first element of the highlighted group
-- | extract subgraph:
-- |   take highlighted nodes, unglue them
-- |   get the edges from the subgraph to the parent graph
-- |   add the edges to the group node
-- |   insert the subgraph into the group node
-- | insert the group node into the non-highlighted graph
-- |   focus on the group node
groupHighlighted :: UIGraph -> UIGraph
groupHighlighted graph =
  -- | Grab some highlighted node. If there are none, don't do anything.
  case Set.findMin (graph ^. _highlighted) >>= lookupNode graph of
    Nothing -> graph
    Just defaultGroupNode ->
      let
        -- Pick a group node
        maybeFocusNodeId = case graph ^. _focus of
          FocusNode nodeId -> if Set.member nodeId (graph ^. _highlighted)
                              then Just nodeId
                              else Nothing
          _ -> Nothing
        groupNode = fromMaybe defaultGroupNode $ maybeFocusNodeId >>= lookupNode graph
        -- Extract subgraph
        highlightedNodes = Set.mapMaybe (lookupNode graph) (graph ^. _highlighted)
        unglued = unglueUI highlightedNodes graph
        edgesBetween = lookupEdgesBetweenGraphs unglued.childGraph unglued.parentGraph
        -- Collapse edges between subgraph and main graph to the groupNode
        subgraph = unglued.childGraph # _highlighted .~ Set.empty
        subgraphEdgeToGroupNode edge =
          if Map.member (edge ^. _source) (subgraph ^. _nodes)
          then edge # _edgeId .~ { source : groupNode ^. _nodeId, target : edge ^. _target }
          else edge # _edgeId .~ { source : edge ^. _source, target : groupNode ^. _nodeId }
        edgesToGroupNode = Set.map subgraphEdgeToGroupNode edgesBetween
        -- Insert subgraph into groupNode
        groupNode' = groupNode # _subgraph .~ subgraph
      in
        -- Insert groupNode into parent graph
        unglued.parentGraph
        # insertNode groupNode'
        # (\g -> foldr insertEdge g edgesToGroupNode)
        # _focus .~ FocusNode (groupNode' ^. _nodeId)

--  case do
--    defaultGroupNodeId <- Array.index (keys (viewHighlighted g)) 0
--    lookupNode defaultGroupNodeId graph
--  of
--    Nothing -> graph
--    Just defaultGroupNode ->
--      let
--        groupNode = case viewFocus graph of
--          FocusNode nodeId ->
--            if Array.elem nodeId $ keys $ viewHighlighted graph
--               then case lookupNode nodeId graph of
--                 Just focusNode -> focusNode
--                 Nothing -> defaultGroupNode
--               else defaultGroupNode
--          _ -> defaultGroupNode
--        subgraphNodes = lookupNodes (viewHighlighted g) graph
--        unglued = unglue subgraphNodes graph
--        groupParents = parentsOfGroup subgraphNodes
--        groupChildren = childrenOfGroup subgraphNodes
--        groupNodeId = viewId groupNode
--        parentGraph = updateFocus (FocusNode groupNodeId) unglued.parentGraph
--        parentGraph' = clearHighlighted parentGraph
--        parentEdges = (\parentId -> createEdge parentId groupNodeId)
--                      <$> keys groupParents
--        childEdges = (\childId -> createEdge groupNodeId childId)
--                     <$> keys groupChildren
--        groupExternalEdges = parentEdges <> childEdges
--        subgraph = updateFocus (FocusNode groupNodeId) unglued.childGraph
--        subgraph' = clearHighlighted subgraph
--        groupInterNode =
--          replaceSubgraph subgraph' groupNode
--      in
--       (insertNode groupInterNode
--        >>>
--        (replaceEdges groupNodeId groupExternalEdges))
--       $ parentGraph'

-- | Take the focus node and expand its subgraph into the current graph.
-- |
-- | take focus node
-- | remove it from graph
-- | glue in its subgraph.
-- TODO: propagate new edges to group node to expanded subgraph
-- TODO: propagate movement of group node to expanded subgraph
expandFocus :: UIGraph -> UIGraph
expandFocus graph =
  case graph ^. _focus of
    FocusNode groupNodeId ->
      case lookupNode graph groupNodeId of
        Just groupNode ->
          let
            subgraph = groupNode ^. _subgraph
            subgraph' = subgraph # _highlighted .~ Map.keys (subgraph ^. _nodes)
          in
            if not $ Map.isEmpty ((groupNode ^. _subgraph) ^. _nodes)
            then  graph
                  # deleteNode groupNode
                  # glueUI subgraph'
            else graph
        Nothing -> graph
    _ -> graph
--  case viewFocus graph of
--    FocusNode groupNodeId ->
--      case lookupNode groupNodeId graph of
--        Nothing -> graph
--        Just groupNode ->
--          let subgraph = viewSubgraph groupNode in
--          if Object.isEmpty (nodes subgraph) then graph else
--          let
--            groupNodePos = viewPos groupNode
--            subgraphGroupNodePos = case lookupNode groupNodeId subgraph of
--              Nothing -> viewPos groupNode
--              Just subgraphNode -> viewPos subgraphNode
--            groupMovement = groupNodePos `subtract` subgraphGroupNodePos
--            movedSubgraph = foldl
--                            (\graph_ node -> moveNodeAmount groupMovement node graph_)
--                            subgraph
--                            $ values $ nodes subgraph
--            glued = glue movedSubgraph $ removeNode (viewId groupNode) graph
--            glued' = updateFocus (FocusNode groupNodeId) glued
--            glued'' = clearHighlighted glued'
--            newHighlighted = keys $ nodes subgraph
--            glued''' = foldl (flip highlight) glued'' newHighlighted
--          in
--            glued'''
--    _ -> graph

toggleGroupExpand :: UIGraph -> UIGraph
toggleGroupExpand graph = case Set.size (graph ^. _highlighted) of
  0 -> expandFocus graph
  _ -> groupHighlighted graph


------
-- Text

graphTitle :: UIGraph -> Maybe String
graphTitle graph =
  List.head titles >>= String.stripPrefix titlePattern
  where
    nodesText =
      graph ^. _nodes
      # Map.values
      <#> view _nodeText
      <#> String.trim
    titlePattern = (Pattern "Title: ")
    titles = List.filter (String.contains titlePattern) nodesText


------
-- Utilities

-- | Eventually replace with KD tree or something cooler then linear search
getNearestNeighbor :: Point2D -> UIGraph -> Maybe { nodeId :: NodeId
                                                  , distance :: Number
                                                  }
getNearestNeighbor point graph =
  let
    distanceToPoint node =
       Math.sqrt $   (Math.pow (point.x - (node ^. _pos).x) 2.0)
                   + (Math.pow (point.y - (node ^. _pos).y) 2.0)
  in do
    closestNode <- minimumBy (comparing distanceToPoint) $ graph ^. _nodes
    pure { nodeId : closestNode ^. _nodeId, distance : distanceToPoint closestNode }
