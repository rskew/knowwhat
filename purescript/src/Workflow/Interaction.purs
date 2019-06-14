module Workflow.Interaction where

import Workflow.Core

import Control.Monad.Except.Trans (ExceptT)
import Foreign (ForeignError)
import Data.List.NonEmpty (NonEmptyList)
import Data.Identity (Identity)
import Data.Array (filter, sortWith, (!!), concatMap)
import Data.Array as Array
import Data.Eq (class Eq)
import Data.Foldable (foldMap, foldl, length, class Foldable, maximumBy, elem)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', lens, view, set, over, setJust)
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord (class Ord, comparing)
import Data.String (Pattern(..), contains, stripPrefix, trim)
import Data.Symbol (SProxy(..))
import Data.UUID (genUUID)
import Effect (Effect)
import Foreign.Class (class Encode, class Decode)
import Foreign.Generic (genericEncode, genericDecode)
import Foreign.Object (Object, keys, values)
import Foreign.Object as Object
import Prelude (($), (<<<), (>>>), map, flip, (==), (<>), bind, pure, (>>=), (+), (<$>), (#), (>), (/=), mod, (<), (-), not, unit, show)



-- | Free monad undo/redo implementation.
-- | A Interaction represents an abstract relation between
-- | the before and after affect of an interaction with the graph.
-- | A Interaction is recorded after the application of the interaction
-- | so that all information required to perform an undo is available (such as nodeId).
-- data Interaction =
  -- AddNode InterNode
  -- AddEdge InterEdge
  -- RemoveEdge InterEdge
  -- MoveNode InterNode Point2D
  -- AddHighlight NodeId
  -- RemoveHighlight NodeId
  -- Group (Object InterNode) InterNode
  -- UnGroup InterNode (Object InterNode)
  -- UngroupNode

version :: String
version = "0.00001"

------
-- Constants

newNodeXOffset :: Number
newNodeXOffset = 100.0

newNodeYOffset :: Number
newNodeYOffset = 100.0

------
-- Core implementation

instance graphEdge :: Edge InterEdge where
  edgeSource = view _InterEdge >>> _.source
  edgeTarget = view _InterEdge >>> _.target

instance graphNode :: Node InterNode where
  createNode = createInterNode
  viewId = view _id
  viewParents = view _parents
  viewChildren = view _children

instance graph :: Graph InterGraph InterNode InterEdge where
  emptyGraph = emptyInterGraph
  viewNodes = view _nodes
  insertNode = insertInterNode
  removeNode = removeInterNode
  viewSubgraph = view _subgraph
  addEdge = addInterEdge
  removeEdge = removeInterEdge
  lookupEdge = lookupInterEdge
  replaceSubgraph = replaceSubgraph'
  lookupNode = lookupInterNode

data Focus =
  FocusNode String
  | FocusEdge InterEdge (Array InterEdge)
  | NoFocus

newtype InteractionState = InteractionState { focus :: Focus
                                            , highlighted :: NodeIdSet
                                            }

emptyInteractionState :: InteractionState
emptyInteractionState = InteractionState { focus : NoFocus
                                         , highlighted : Object.empty
                                         }

type Point2D = { x :: Number, y :: Number }

add :: Point2D -> Point2D -> Point2D
add a b = {x: a.x + b.x, y: a.y + b.y}

subtract :: Point2D -> Point2D -> Point2D
subtract a b = {x: a.x - b.x, y: a.y - b.y}

newtype InterGraph = InterGraph
  { nodes :: Object InterNode
  , edgeData :: Object Boolean
  , interactionState :: InteractionState
  }

emptyInterGraph :: InterGraph
emptyInterGraph = InterGraph
  { nodes: Object.empty
  , edgeData: Object.empty
  , interactionState : emptyInteractionState
  }

newtype InterNode = InterNode
  { id :: NodeId
  , children :: NodeIdSet
  , parents :: NodeIdSet
  , subgraph :: InterGraph
  , text :: String
  , position :: Point2D
  , isValid :: Boolean
  }

createInterNode :: NodeIdSet -> NodeIdSet -> Effect InterNode
createInterNode parentIds childIds = do
   nodeId <- genUUID
   pure $ InterNode
      { id : show nodeId
      , children : childIds
      , parents : parentIds
      , subgraph : emptyInterGraph
      , text : ""
      , position : { x: 0.0, y: 0.0 }
      , isValid : false
      }

newtype InterEdge = InterEdge { source :: NodeId
                              , target :: NodeId
                              , isValid :: Boolean
                              }

lookupInterEdge :: NodeId -> NodeId -> InterGraph -> InterEdge
lookupInterEdge source target g =
  let
    isValid = fromMaybe false
              $ view (_edgeData <<< at (edgeId source target))
              $ g
  in
    InterEdge { source : source, target : target, isValid : isValid }

------
-- Generic serialisation boilerplate

derive instance generic :: Generic InterGraph _
derive instance eq :: Eq InterGraph
instance encode :: Encode InterGraph where
  encode = genericEncode genericEncodeOpts
instance decode :: Decode InterGraph where
  decode = genericDecode genericEncodeOpts

derive instance genericNode :: Generic InterNode _
derive instance eqNode :: Eq InterNode
instance encodeNode :: Encode InterNode where
  encode node = genericEncode genericEncodeOpts node
instance decodeNode :: Decode InterNode where
  decode node = genericDecode genericEncodeOpts node

derive instance genericInterEdge :: Generic InterEdge _
derive instance eqInterEdge :: Eq InterEdge
instance encodeInterEdge :: Encode InterEdge where
  encode x = genericEncode genericEncodeOpts x
instance decodeInterEdge :: Decode InterEdge where
  decode x = genericDecode genericEncodeOpts x

derive instance genericFocus :: Generic Focus _
derive instance eqFocus :: Eq Focus
instance encodeFocus :: Encode Focus where
  encode x = genericEncode genericEncodeOpts x
instance decodeFocus :: Decode Focus where
  decode x = genericDecode genericEncodeOpts x

derive instance genericInteractionState :: Generic InteractionState _
derive instance eqInteractionState :: Eq InteractionState
instance encodeInteractionState :: Encode InteractionState where
  encode = genericEncode genericEncodeOpts
instance decodeInteractionState :: Decode InteractionState where
  decode = genericDecode genericEncodeOpts

------
-- Lens boilerplate

_InterGraph :: Lens' InterGraph { nodes :: Object InterNode
                                , edgeData :: Object Boolean
                                , interactionState :: InteractionState
                                }
_InterGraph = lens (\(InterGraph g) -> g) (\_ -> InterGraph)

_nodes :: Lens' InterGraph (Object InterNode)
_nodes = _InterGraph <<< prop (SProxy :: SProxy "nodes")

_edgeData :: Lens' InterGraph (Object Boolean)
_edgeData = _InterGraph <<< prop (SProxy :: SProxy "edgeData")

_interactionState :: Lens' InterGraph InteractionState
_interactionState = _InterGraph <<< prop (SProxy :: SProxy "interactionState")

_InteractionState :: Lens' InteractionState { focus :: Focus
                                            , highlighted :: NodeIdSet
                                            }
_InteractionState = lens (\(InteractionState s) -> s) (\_ -> InteractionState)

_highlighted :: Lens' InterGraph NodeIdSet
_highlighted = _interactionState <<< _InteractionState <<< prop (SProxy :: SProxy "highlighted")

_focus :: Lens' InterGraph Focus
_focus = _interactionState <<< _InteractionState <<< prop (SProxy :: SProxy "focus")

_InterNode :: Lens' InterNode { id :: NodeId
                              , children :: NodeIdSet
                              , parents :: NodeIdSet
                              , subgraph :: InterGraph
                              , text :: String
                              , position :: Point2D
                              , isValid :: Boolean
                              }
_InterNode = lens (\(InterNode n) -> n) (\_ -> InterNode)

_parents :: Lens' InterNode NodeIdSet
_parents = _InterNode <<< prop (SProxy :: SProxy "parents")

_children :: Lens' InterNode NodeIdSet
_children = _InterNode <<< prop (SProxy :: SProxy "children")

_id :: Lens' InterNode String
_id = _InterNode <<< prop (SProxy :: SProxy "id")

_subgraph :: Lens' InterNode InterGraph
_subgraph = _InterNode <<< prop (SProxy :: SProxy "subgraph")

_position :: Lens' InterNode Point2D
_position = _InterNode <<< prop (SProxy :: SProxy "position")

_x :: Lens' InterNode Number
_x = _position <<< prop (SProxy :: SProxy "x")

_y :: Lens' InterNode Number
_y = _position <<< prop (SProxy :: SProxy "y")

_text :: Lens' InterNode String
_text = _InterNode <<< prop (SProxy :: SProxy "text")

_nodeIsValid :: Lens' InterNode Boolean
_nodeIsValid = _InterNode <<< prop (SProxy :: SProxy "isValid")

_InterEdge :: Lens' InterEdge { source :: NodeId
                              , target :: NodeId
                              , isValid :: Boolean
                              }
_InterEdge = lens (\(InterEdge e) -> e) (\_ -> InterEdge)

_edgeIsValid :: Lens' InterEdge Boolean
_edgeIsValid = _InterEdge <<< prop (SProxy :: SProxy "isValid")

_subgraphInteractionState :: Lens' InterNode InteractionState
_subgraphInteractionState = _subgraph <<<
                            _interactionState


------
--  basic operations

addParent :: NodeId -> InterNode -> InterNode
addParent nodeId = over _parents $ insertNodeId nodeId

deleteParent :: NodeId -> InterNode -> InterNode
deleteParent nodeId = over _parents $ deleteNodeId nodeId

addChild :: NodeId -> InterNode -> InterNode
addChild nodeId = over _children $ insertNodeId nodeId

deleteChild :: NodeId -> InterNode -> InterNode
deleteChild nodeId = over _children $ deleteNodeId nodeId

insertInterNode :: InterNode -> InterGraph -> InterGraph
insertInterNode newNode g =
  let
    newNodeId = (viewId newNode)
    g' = setJust (_nodes <<< (at (viewId newNode))) newNode g
  in
    g'

removeInterNode :: NodeId -> InterGraph -> InterGraph
removeInterNode nodeId g =
  set (_nodes <<< (at nodeId)) Nothing
    $ removeParents nodeId
    $ removeChildren nodeId
    $ g

updateEdgeData :: NodeId -> NodeId -> Maybe Boolean -> InterGraph -> InterGraph
updateEdgeData source target newData g = case newData of
  Nothing -> over _edgeData (Object.delete (edgeId source target)) g
  Just valid -> over _edgeData (Object.insert (edgeId source target) valid) g

addInterEdge :: InterEdge -> InterGraph -> InterGraph
addInterEdge (InterEdge edge) =
  (over _edgeData (Object.insert
                   (edgeId edge.source edge.target)
                   (view _edgeIsValid (InterEdge edge))))
  <<<
  (over (_nodes <<< (at edge.target)) $ map $ addParent edge.source)
  <<<
  (over (_nodes <<< (at edge.source)) $ map $ addChild edge.target)

removeInterEdge :: NodeId -> NodeId -> InterGraph -> InterGraph
removeInterEdge source target =
  (updateEdgeData source target Nothing)
  <<<
  (over (_nodes <<< (at target)) $ map $ deleteParent source)
  <<<
  (over (_nodes <<< (at source)) $ map $ deleteChild target)

replaceSubgraph' :: InterGraph -> InterNode -> InterNode
replaceSubgraph' = set _subgraph

lookupInterNode :: NodeId -> InterGraph -> Maybe InterNode
lookupInterNode nodeId g = view (_nodes <<< at nodeId) g

lookupEdgeData :: NodeId -> NodeId -> InterGraph -> Maybe Boolean
lookupEdgeData source target g = do
  Object.lookup (edgeId source target) $ view _edgeData g

updateNodePosition :: Point2D -> NodeId -> InterGraph -> InterGraph
updateNodePosition newPos nodeId =
  over (_nodes <<< (at nodeId)) $ map $ moveNode newPos

moveNode :: Point2D -> InterNode -> InterNode
moveNode pos = set _x pos.x <<<
               set _y pos.y

moveNodeAmount :: Point2D -> InterNode -> InterGraph -> InterGraph
moveNodeAmount motion node g =
   let newPos = add {x: view _x node, y: view _y node} motion in
   updateNodePosition newPos (view _id node) g

updateNodeText :: String -> InterNode -> InterNode
updateNodeText = set _text


------
-- Focusing

updateFocus :: Focus -> InterGraph -> InterGraph
updateFocus = set _focus

removeFocus :: InterGraph -> InterGraph
removeFocus g =
  case view _focus g of
    NoFocus -> g
    FocusNode nodeId -> case lookupNode nodeId g of
      Nothing -> g
      Just focusNode ->
        let
          newFocus = case (keys (view _parents focusNode)) !! 0 of
            Nothing -> case (keys (view _children focusNode)) !! 0 of
              Nothing -> NoFocus
              Just childId -> FocusNode childId
            Just parentId -> FocusNode parentId
        in updateFocus newFocus
          $ unHighlight (view _id focusNode)
          $ removeNode (view _id focusNode) g
    FocusEdge (InterEdge edge) _ ->
      updateFocus (FocusNode edge.source)
      $ removeEdge edge.source edge.target g

edgeInFocusGroup :: InterGraph -> InterEdge -> Boolean
edgeInFocusGroup g edge =
  case view _focus g of
    FocusEdge _ focusGroup -> elem edge focusGroup
    _ -> false



------
-- Highlighting

highlight :: NodeId -> InterGraph -> InterGraph
highlight nodeId = over _highlighted (Object.insert nodeId unit)

unHighlight :: NodeId -> InterGraph -> InterGraph
unHighlight nodeId = over _highlighted (Object.delete nodeId)

clearHighlighted :: InterGraph -> InterGraph
clearHighlighted = set _highlighted Object.empty

highlightFocus :: InterGraph -> InterGraph
highlightFocus g = case view _focus g of
  FocusNode nodeId -> highlight nodeId g
  _ -> g

unHighlightFocus :: InterGraph -> InterGraph
unHighlightFocus g = case view _focus g of
  FocusNode nodeId -> unHighlight nodeId g
  _ -> g

toggleHighlightFocus :: InterGraph -> InterGraph
toggleHighlightFocus g = case view _focus g of
  FocusNode nodeId ->
    case Object.member nodeId (view _highlighted g) of
      true -> unHighlight nodeId g
      false -> highlight nodeId g
  FocusEdge edge _ -> set _focus (FocusEdge edge []) g
  _ -> g


------
-- Traversal

traverseUp :: InterGraph -> InterGraph
traverseUp g =
  case view _focus g of
    FocusNode nodeId -> fromMaybe g do
      node <- lookupNode nodeId g
      let upEdges' = InterEdge <$> { source: _
                                   , target: view _id node
                                   , isValid: false
                                   } <$> (keys (view _parents node))
      newFocus <- upEdges' !! 0
      -- If the focus group has a single element then collapse the focus
      let upEdges = if length upEdges' == 1 then [] else upEdges'
      pure $ set _focus (FocusEdge newFocus upEdges) g
    FocusEdge (InterEdge edge) _ -> set _focus (FocusNode edge.source) g
    NoFocus -> g

-- TODO: remove duplicate code
traverseDown :: InterGraph -> InterGraph
traverseDown g =
  case view _focus g of
    FocusNode nodeId -> fromMaybe g do
      node <- lookupNode nodeId g
      let downEdges' = InterEdge <$> { source: view _id node
                                     , target: _
                                     , isValid: false
                                     } <$> (keys (view _children node))
      newFocus <- downEdges' !! 0
      -- If the focus group has a single element then collapse the focus
      let downEdges = if length downEdges' == 1 then [] else downEdges'
      pure $ set _focus (FocusEdge newFocus downEdges) g
    FocusEdge (InterEdge edge) _ -> set _focus (FocusNode edge.target) g
    NoFocus -> g

siblings :: InterGraph -> InterNode -> NodeIdSet
siblings g node = foldMap (view _children) parents
  where
    parents = lookupNodes parentIds g
    parentIds = view _parents node

coparents :: InterGraph -> InterNode -> NodeIdSet
coparents g node = foldMap (view _parents) children
  where
    children = lookupNodes childIds g
    childIds = view _children node

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

nextEdgeGroup :: DirectionLR -> InterGraph -> InterEdge -> Array InterEdge
nextEdgeGroup dir g (InterEdge edge) =
  case lookupNode edge.source g of
    Nothing -> []
    Just source ->
      case lookupNode edge.target g of
        Nothing -> []
        Just target ->
          let
            leftParentOfTarget =
              nextElemWrap dir source (view _x)
              $ lookupNodes (view _parents target) g
            leftChildOfSource =
              nextElemWrap dir target (view _x)
              $ lookupNodes (view _children source) g
            newSourceEdge = InterEdge { source: view _id leftParentOfTarget
                                      , target: view _id target
                                      , isValid: false
                                      }
            newTargetEdge = InterEdge { source: view _id source
                                      , target: view _id leftChildOfSource
                                      , isValid: false
                                      }
          in
          filter ((/=) (InterEdge edge)) [newSourceEdge, newTargetEdge]

nextInGroup :: forall a. Eq a => DirectionLR -> a -> Array a -> Maybe a
nextInGroup Left x xs =
  Array.elemIndex x xs >>= \xIndex -> xs !! (xIndex + 1) `mod` length xs
nextInGroup Right x xs =
  Array.elemIndex x xs >>= \xIndex -> xs !! (xIndex - 1) `mod` length xs

nextNodeWrap :: DirectionLR -> InterGraph -> InterNode -> InterNode
nextNodeWrap dir g node =
  nextElemWrap dir node (view _x) siblingsAndCoparents
  where
    siblingsAndCoparents = lookupNodes (siblings g node <> coparents g node) g

edgePosition :: InterGraph -> InterEdge -> Number
edgePosition g (InterEdge edge) =
  fromMaybe 0.0 do
    source <- lookupNode edge.source g
    target <- lookupNode edge.target g
    pure $ (view _x source) + (view _x target)

changeFocusLeftRight :: DirectionLR -> InterGraph -> InterGraph
changeFocusLeftRight dir g =
  case view _focus g of
    FocusNode nodeId -> case lookupNode nodeId g of
      Nothing -> g
      Just node ->
        let
          leftNode = nextNodeWrap dir g node
        in
        set _focus (FocusNode (view _id leftNode)) g
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
      set _focus focus g
    NoFocus -> g

traverseLeft :: InterGraph -> InterGraph
traverseLeft g = changeFocusLeftRight Left g

traverseRight :: InterGraph -> InterGraph
traverseRight g = changeFocusLeftRight Right g


------
-- Positioning nodes
--

rightmostNode :: forall f. Foldable f => f InterNode -> Maybe InterNode
rightmostNode = maximumBy (comparing (view _x))

--newPositionFrom ::  -> InterNode -> (InterNode -> NodeIdSet) -> Point2D
--newPositionFrom g (InterNode node) relations =
--  fromMaybe { x: node.x, y: node.y + newParentYOffset } do
--    (InterNode rightmostParent) <- rightmostNode $ lookupNodes (relations $ InterNode node) g
--    pure { x: rightmostParent.x + newNodeXOffset
--         , y: rightmostParent.y }

newChildPosition :: InterGraph -> InterNode -> Point2D
newChildPosition g node =
  fromMaybe { x: view _x node, y: (view _y node) + newNodeYOffset } do
    rightmostChild <- rightmostNode $ lookupNodes (view _children node) g
    pure { x: (view _x rightmostChild) + newNodeXOffset, y: (view _y rightmostChild) }

newParentPosition :: InterGraph -> InterNode -> Point2D
newParentPosition g node =
  fromMaybe { x: view _x node, y: (view _y node) - newNodeYOffset } do
    rightmostParent <- rightmostNode $ lookupNodes (view _parents node) g
    pure { x: (view _x rightmostParent) + newNodeXOffset, y: view _y rightmostParent }

newChildOfFocus :: InterGraph -> Effect InterGraph
newChildOfFocus g = case view _focus g of
  FocusNode nodeId -> case lookupNode nodeId g of
    Nothing -> pure g
    Just node ->
      let
        newChildPos = newChildPosition g node
        newChildParents = nodeIdSetFromArray $ [viewId node]
      in do
        newChildNode' <- createNode Object.empty Object.empty
        let newChildNode = set _position newChildPos newChildNode'
        let newEdge = InterEdge { source : nodeId
                                , target : viewId newChildNode
                                , isValid : false
                                }
        pure $ ((updateFocus (FocusNode (view _id newChildNode))) <<<
                (addEdge newEdge) <<<
                (insertNode newChildNode))
             $ g
  _ -> pure g

newParentOfFocus :: InterGraph -> Effect InterGraph
newParentOfFocus g = case view _focus g of
  FocusNode nodeId -> case lookupNode nodeId g of
    Nothing -> pure g
    Just node ->
      let
        newParentPos = newParentPosition g node
        newParentChildren = nodeIdSetFromArray $ [viewId node]
      in do
        newParentNode' <- createNode Object.empty Object.empty
        let newParentNode = set _position newParentPos newParentNode'
        let newEdge = InterEdge { source : viewId newParentNode
                                , target : viewId node
                                , isValid : false
                                }
        pure $ ((updateFocus (FocusNode (viewId newParentNode)))
                <<<
                (addEdge newEdge)
                <<<
                (insertNode newParentNode))
             $ g
  _ -> pure g


------
-- Subgraph collapse/expand

parentsOfGroup :: Array InterNode -> NodeIdSet
parentsOfGroup nodes =
  let
    allParents = concatMap (view _parents >>> keys) nodes
    nodesIds = map (view _id) nodes
  in
    nodeIdSetFromArray $ filter (not <<< flip Array.elem nodesIds) allParents

childrenOfGroup :: Array InterNode -> NodeIdSet
childrenOfGroup nodes =
  let
    allChildren = concatMap (view _children >>> keys) nodes
    nodesIds = map (view _id) nodes
  in
    nodeIdSetFromArray $ filter (not <<< flip Array.elem nodesIds) allChildren

groupHighlighted :: InterGraph -> InterGraph
groupHighlighted g =
  case do
    defaultGroupNodeId <- Array.index (keys (view _highlighted g)) 0
    lookupNode defaultGroupNodeId g
  of
    Nothing -> g
    Just defaultGroupNode ->
      let
        groupNode = case view _focus g of
          FocusNode nodeId ->
            if Array.elem nodeId $ keys $ view _highlighted g
               then case lookupNode nodeId g of
                 Just focusNode -> focusNode
                 Nothing -> defaultGroupNode
               else defaultGroupNode
          _ -> defaultGroupNode
        subgraphNodes = lookupNodes (view _highlighted g) g
        unglued = unglue subgraphNodes g
        groupParents = parentsOfGroup subgraphNodes
        groupChildren = childrenOfGroup subgraphNodes
        groupInterNode =
          replaceSubgraph unglued.childGraph groupNode
        focusGroupNodeNoHighlight = InteractionState { highlighted : Object.empty
                                                     , focus : FocusNode (viewId groupNode)
                                                     }
        groupInterNode' = set _subgraphInteractionState focusGroupNodeNoHighlight groupInterNode
        groupNodeId = viewId groupInterNode'
        parentGraph = set _interactionState focusGroupNodeNoHighlight unglued.parentGraph
        parentEdges = (\parentId ->
                        InterEdge { source : parentId
                                  , target : groupNodeId
                                  , isValid : false
                                  })
                      <$> keys groupParents
        childEdges = (\childId ->
                       InterEdge { source : groupNodeId
                                 , target : childId
                                 , isValid : false
                                 })
                     <$> keys groupChildren
      in
       (insertNode groupInterNode'
        >>>
        (replaceEdges groupNodeId (parentEdges <> childEdges)))
       $ parentGraph

expandFocus :: InterGraph -> InterGraph
expandFocus g =
  case view _focus g of
    FocusNode groupNodeId ->
      case lookupNode groupNodeId g of
        Nothing -> g
        Just groupNode ->
          if Object.isEmpty (view (_subgraph <<< _nodes) groupNode) then g else
          let
            subgraph = view _subgraph groupNode
            groupNodePos = view _position groupNode
            subgraphGroupNodePos = case lookupNode groupNodeId subgraph of
              Nothing -> view _position groupNode
              Just subgraphNode -> view _position subgraphNode
            groupMovement = groupNodePos `subtract` subgraphGroupNodePos
            movedSubgraph = foldl
                            (\graph_ node -> moveNodeAmount groupMovement node graph_)
                            subgraph
                            $ view (_subgraph <<< _nodes) groupNode
            glued = glue movedSubgraph $ removeNode (view _id groupNode) g
            newInteractionState = InteractionState { focus : FocusNode groupNodeId
                                                   , highlighted : nodeIdSetFromArray $ keys $ view (_subgraph <<< _nodes) groupNode
                                                   }
            glued' = set _interactionState newInteractionState glued
          in
            set _focus (FocusNode groupNodeId) glued'
    _ -> g

toggleGroupExpand :: InterGraph -> InterGraph
toggleGroupExpand g = case length (view _highlighted g) of
  0 -> expandFocus g
  _ -> groupHighlighted g


------
-- Validity

updateNodeValidity :: Boolean -> NodeId -> InterGraph -> InterGraph
updateNodeValidity validity nodeId = over (_nodes <<< (at nodeId)) (map (set _nodeIsValid validity))

updateEdgeValidity :: Boolean -> InterEdge -> InterGraph -> InterGraph
updateEdgeValidity validity (InterEdge edge) g =
  updateEdgeData edge.source edge.target (Just validity) g


------
-- Text

updateCellText :: NodeId -> String -> InterGraph -> InterGraph
updateCellText nodeId newText = over (_nodes <<< at nodeId) (map (set _text newText))


graphTitle :: InterGraph -> Maybe String
graphTitle g = titles !! 0 >>= stripPrefix titlePattern
  where
    titlePattern = Pattern "Title: "
    nodeTextArr = trim <$> (view _text) <$> values (view _nodes g)
    isTitle = contains titlePattern
    titles = filter isTitle nodeTextArr

------
-- Demo

demo :: InterGraph
demo = updateFocus (FocusEdge (InterEdge { source: "title"
                                         , target: "goofus"
                                         , isValid: false
                                         })
                    [InterEdge { source: "title"
                               , target: "goofus"
                               , isValid: false
                               },
                     InterEdge { source: "thingo"
                               , target: "goofus"
                               , isValid: false
                               }])
       $ highlight "thingo"
       $ addEdge (InterEdge { source : "title"
                                 , target : "goofus"
                                 , isValid : false
                                 })
       $ insertNode (InterNode
           { text: "Title: Workflow"
           , isValid: true
           , position : { x : 205.0
                        , y : 150.0
                        }
           , id : "title"
           , parents : Object.empty
           , children : Object.empty
           , subgraph : emptyInterGraph
           })
       $ addEdge (InterEdge { source : "thingo"
                                 , target : "goofus"
                                 , isValid : false
                                 })
       $ insertNode (InterNode
           { text: "thingo"
           , isValid: false
           , position : { x : 205.0
                        , y : 100.0
                        }
           , id : "thingo"
           , parents : Object.empty
           , children : Object.empty
           , subgraph : emptyInterGraph
           })
       $ insertNode (InterNode
           { text: "asdf"
           , isValid: true
           , position : { x: 450.0
                        , y: 270.0
                        }
           , id : "goofus"
           , parents : Object.empty
           , children : Object.empty
           , subgraph : emptyInterGraph
           })
       $ emptyInterGraph

-- TODO: re-export to JS using module system properly
fromMaybe_ :: forall a. a -> Maybe a -> a
fromMaybe_ = fromMaybe

-- Give some polymorphic functions concrete instances to make them
-- easier to call from JavaScript
resolvedInterGraphEdges :: InterGraph -> Array { source :: InterNode, target :: InterNode }
resolvedInterGraphEdges = resolvedGraphEdges

lookupInterNodes :: NodeIdSet -> InterGraph -> Array InterNode
lookupInterNodes = lookupNodes

interGraphToJSON :: InterGraph -> String
interGraphToJSON = graphToJSON

interGraphFromJSON :: String -> ExceptT (NonEmptyList ForeignError) Identity InterGraph
interGraphFromJSON = graphFromJSON
