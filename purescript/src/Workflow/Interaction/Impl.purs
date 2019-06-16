module Workflow.Interaction.Impl where

import Workflow.Core (class Edge, class Graph, class Node, NodeId, NodeIdSet, addEdge, deleteNodeId, edgeId, genericEncodeOpts, graphFromJSON, graphToJSON, insertNode, insertNodeId, lookupNodes, removeChildren, removeParents, resolvedGraphEdges, viewId)
import Workflow.Interaction

import Control.Monad.Except.Trans (ExceptT)
import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity)
import Data.Lens (Lens', lens, view, set, over, setJust)
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Data.UUID (genUUID)
import Effect (Effect)
import Foreign (ForeignError)
import Foreign.Class (class Encode, class Decode)
import Foreign.Generic (genericEncode, genericDecode)
import Foreign.Object (Object)
import Foreign.Object as Object
import Prelude (($), (<<<), (>>>), map, bind, pure, unit, show)


------
-- Constants

version :: String
version = "0.00001"

newNodeOffset :: Point2D
newNodeOffset = { x: 100.0, y: 100.0 }


------
-- Types

newtype InteractionState = InteractionState { focus :: Focus InterEdgeImpl
                                            , highlighted :: NodeIdSet
                                            }

emptyInteractionState :: InteractionState
emptyInteractionState = InteractionState { focus : NoFocus
                                         , highlighted : Object.empty
                                         }

newtype InterGraphImpl = InterGraphImpl
  { nodes :: Object InterNodeImpl
  , edgeData :: Object Boolean
  , interactionState :: InteractionState
  }

emptyInterGraphImpl :: InterGraphImpl
emptyInterGraphImpl = InterGraphImpl
  { nodes: Object.empty
  , edgeData: Object.empty
  , interactionState : emptyInteractionState
  }

newtype InterNodeImpl = InterNodeImpl
  { id :: NodeId
  , children :: NodeIdSet
  , parents :: NodeIdSet
  , subgraph :: InterGraphImpl
  , text :: String
  , position :: Point2D
  , isValid :: Boolean
  }

createInterNodeImpl :: NodeIdSet -> NodeIdSet -> Effect InterNodeImpl
createInterNodeImpl parentIds childIds = do
   nodeId <- genUUID
   pure $ InterNodeImpl
      { id : show nodeId
      , children : childIds
      , parents : parentIds
      , subgraph : emptyInterGraphImpl
      , text : ""
      , position : { x: 0.0, y: 0.0 }
      , isValid : false
      }

createEdgeImpl :: NodeId -> NodeId -> InterEdgeImpl
createEdgeImpl source target = InterEdgeImpl { source : source
                                                  , target : target
                                                  , isValid : false
                                                  }

newtype InterEdgeImpl = InterEdgeImpl { source :: NodeId
                                      , target :: NodeId
                                      , isValid :: Boolean
                                      }

lookupEdgeImpl :: NodeId -> NodeId -> InterGraphImpl -> Maybe InterEdgeImpl
lookupEdgeImpl source target g = do
  isValid <- view (_edgeData <<< at (edgeId source target)) g
  pure $ InterEdgeImpl { source : source, target : target, isValid : isValid }


------
-- Graph typeclass instances

instance interNode :: InterNode InterNodeImpl where
  viewPos = view _position
  moveNode = moveNodeImpl
  viewText = view _text

instance interGraph :: InterGraph InterGraphImpl InterNodeImpl InterEdgeImpl where
  updateNodePosition = updateNodePositionImpl
  moveNodeAmount = moveNodeAmountImpl
  viewFocus = viewFocusImpl
  updateFocus = updateFocusImpl
  viewHighlighted = view _highlighted
  highlight = highlightImpl
  unHighlight = unHighlightImpl
  updateText = updateTextImpl

instance validatableInterGraph :: ValidatableGraph InterGraphImpl InterNodeImpl InterEdgeImpl where
  updateNodeValidity = updateNodeValidityImpl
  updateEdgeValidity = updateEdgeValidityImpl

instance graphEdge :: Edge InterEdgeImpl where
  createEdge = createEdgeImpl
  edgeSource = view _InterEdgeImpl >>> _.source
  edgeTarget = view _InterEdgeImpl >>> _.target

instance graphNode :: Node InterNodeImpl where
  createNode = createInterNodeImpl
  viewId = view _id
  viewParents = view _parents
  viewChildren = view _children

instance graph :: Graph InterGraphImpl InterNodeImpl InterEdgeImpl where
  emptyGraph = emptyInterGraphImpl
  nodes = view _nodes
  lookupNode = lookupNodeImpl
  insertNode = insertNodeImpl
  removeNode = removeNodeImpl
  lookupEdge = lookupEdgeImpl
  addEdge = addEdgeImpl
  removeEdge = removeEdgeImpl
  viewSubgraph = view _subgraph
  replaceSubgraph = replaceSubgraph'


------
-- Generic serialisation boilerplate

derive instance generic :: Generic InterGraphImpl _
derive instance eq :: Eq InterGraphImpl
instance encode :: Encode InterGraphImpl where
  encode = genericEncode genericEncodeOpts
instance decode :: Decode InterGraphImpl where
  decode = genericDecode genericEncodeOpts

derive instance genericNode :: Generic InterNodeImpl _
derive instance eqNode :: Eq InterNodeImpl
instance encodeNode :: Encode InterNodeImpl where
  encode node = genericEncode genericEncodeOpts node
instance decodeNode :: Decode InterNodeImpl where
  decode node = genericDecode genericEncodeOpts node

derive instance genericInterEdgeImpl :: Generic InterEdgeImpl _
derive instance eqInterEdgeImpl :: Eq InterEdgeImpl
instance encodeInterEdgeImpl :: Encode InterEdgeImpl where
  encode x = genericEncode genericEncodeOpts x
instance decodeInterEdgeImpl :: Decode InterEdgeImpl where
  decode x = genericDecode genericEncodeOpts x

derive instance genericInteractionState :: Generic InteractionState _
derive instance eqInteractionState :: Eq InteractionState
instance encodeInteractionState :: Encode InteractionState where
  encode = genericEncode genericEncodeOpts
instance decodeInteractionState :: Decode InteractionState where
  decode = genericDecode genericEncodeOpts


------
-- Lens boilerplate

_InterGraphImpl :: Lens' InterGraphImpl { nodes :: Object InterNodeImpl
                                        , edgeData :: Object Boolean
                                        , interactionState :: InteractionState
                                        }
_InterGraphImpl = lens (\(InterGraphImpl g) -> g) (\_ -> InterGraphImpl)

_nodes :: Lens' InterGraphImpl (Object InterNodeImpl)
_nodes = _InterGraphImpl <<< prop (SProxy :: SProxy "nodes")

_edgeData :: Lens' InterGraphImpl (Object Boolean)
_edgeData = _InterGraphImpl <<< prop (SProxy :: SProxy "edgeData")

_interactionState :: Lens' InterGraphImpl InteractionState
_interactionState = _InterGraphImpl <<< prop (SProxy :: SProxy "interactionState")

_InteractionState :: Lens' InteractionState
                           { focus :: Focus InterEdgeImpl
                           , highlighted :: NodeIdSet
                           }
_InteractionState = lens (\(InteractionState s) -> s) (\_ -> InteractionState)

_highlighted :: Lens' InterGraphImpl NodeIdSet
_highlighted = _interactionState <<< _InteractionState <<< prop (SProxy :: SProxy "highlighted")

_focus :: Lens' InterGraphImpl (Focus InterEdgeImpl)
_focus = _interactionState <<< _InteractionState <<< prop (SProxy :: SProxy "focus")

_InterNodeImpl :: Lens' InterNodeImpl { id :: NodeId
                                      , children :: NodeIdSet
                                      , parents :: NodeIdSet
                                      , subgraph :: InterGraphImpl
                                      , text :: String
                                      , position :: Point2D
                                      , isValid :: Boolean
                                      }
_InterNodeImpl = lens (\(InterNodeImpl n) -> n) (\_ -> InterNodeImpl)

_parents :: Lens' InterNodeImpl NodeIdSet
_parents = _InterNodeImpl <<< prop (SProxy :: SProxy "parents")

_children :: Lens' InterNodeImpl NodeIdSet
_children = _InterNodeImpl <<< prop (SProxy :: SProxy "children")

_id :: Lens' InterNodeImpl String
_id = _InterNodeImpl <<< prop (SProxy :: SProxy "id")

_subgraph :: Lens' InterNodeImpl InterGraphImpl
_subgraph = _InterNodeImpl <<< prop (SProxy :: SProxy "subgraph")

_position :: Lens' InterNodeImpl Point2D
_position = _InterNodeImpl <<< prop (SProxy :: SProxy "position")

_x :: Lens' InterNodeImpl Number
_x = _position <<< prop (SProxy :: SProxy "x")

_y :: Lens' InterNodeImpl Number
_y = _position <<< prop (SProxy :: SProxy "y")

_text :: Lens' InterNodeImpl String
_text = _InterNodeImpl <<< prop (SProxy :: SProxy "text")

_nodeIsValid :: Lens' InterNodeImpl Boolean
_nodeIsValid = _InterNodeImpl <<< prop (SProxy :: SProxy "isValid")

_InterEdgeImpl :: Lens' InterEdgeImpl { source :: NodeId
                                      , target :: NodeId
                                      , isValid :: Boolean
                                      }
_InterEdgeImpl = lens (\(InterEdgeImpl e) -> e) (\_ -> InterEdgeImpl)

_edgeIsValid :: Lens' InterEdgeImpl Boolean
_edgeIsValid = _InterEdgeImpl <<< prop (SProxy :: SProxy "isValid")

_subgraphInteractionState :: Lens' InterNodeImpl InteractionState
_subgraphInteractionState = _subgraph <<<
                            _interactionState


------
--  basic operations

addParent :: NodeId -> InterNodeImpl -> InterNodeImpl
addParent nodeId = over _parents $ insertNodeId nodeId

deleteParent :: NodeId -> InterNodeImpl -> InterNodeImpl
deleteParent nodeId = over _parents $ deleteNodeId nodeId

addChild :: NodeId -> InterNodeImpl -> InterNodeImpl
addChild nodeId = over _children $ insertNodeId nodeId

deleteChild :: NodeId -> InterNodeImpl -> InterNodeImpl
deleteChild nodeId = over _children $ deleteNodeId nodeId

insertNodeImpl :: InterNodeImpl -> InterGraphImpl -> InterGraphImpl
insertNodeImpl newNode g =
  setJust (_nodes <<< (at (viewId newNode))) newNode g

removeNodeImpl :: NodeId -> InterGraphImpl -> InterGraphImpl
removeNodeImpl nodeId g =
  set (_nodes <<< (at nodeId)) Nothing
    $ removeParents nodeId
    $ removeChildren nodeId
    $ g

updateEdgeData :: NodeId -> NodeId -> Maybe Boolean -> InterGraphImpl -> InterGraphImpl
updateEdgeData source target newData g = case newData of
  Nothing -> over _edgeData (Object.delete (edgeId source target)) g
  Just valid -> over _edgeData (Object.insert (edgeId source target) valid) g

addEdgeImpl :: InterEdgeImpl -> InterGraphImpl -> InterGraphImpl
addEdgeImpl (InterEdgeImpl edge) =
  (over _edgeData (Object.insert
                   (edgeId edge.source edge.target)
                   (view _edgeIsValid (InterEdgeImpl edge))))
  <<<
  (over (_nodes <<< (at edge.target)) $ map $ addParent edge.source)
  <<<
  (over (_nodes <<< (at edge.source)) $ map $ addChild edge.target)

removeEdgeImpl :: NodeId -> NodeId -> InterGraphImpl -> InterGraphImpl
removeEdgeImpl source target =
  (updateEdgeData source target Nothing)
  <<<
  (over (_nodes <<< (at target)) $ map $ deleteParent source)
  <<<
  (over (_nodes <<< (at source)) $ map $ deleteChild target)

replaceSubgraph' :: InterGraphImpl -> InterNodeImpl -> InterNodeImpl
replaceSubgraph' = set _subgraph

lookupNodeImpl :: NodeId -> InterGraphImpl -> Maybe InterNodeImpl
lookupNodeImpl nodeId g = view (_nodes <<< at nodeId) g

lookupEdgeData :: NodeId -> NodeId -> InterGraphImpl -> Maybe Boolean
lookupEdgeData source target g = do
  Object.lookup (edgeId source target) $ view _edgeData g

updateNodePositionImpl :: Point2D -> NodeId -> InterGraphImpl -> InterGraphImpl
updateNodePositionImpl newPos nodeId =
  over (_nodes <<< (at nodeId)) $ map $ moveNode newPos

moveNodeImpl :: Point2D -> InterNodeImpl -> InterNodeImpl
moveNodeImpl pos = set _x pos.x <<<
                   set _y pos.y

moveNodeAmountImpl :: Point2D -> InterNodeImpl -> InterGraphImpl -> InterGraphImpl
moveNodeAmountImpl motion node g =
  let newPos = add {x: viewX node, y: viewY node} motion in
  updateNodePosition newPos (viewId node) g


------
-- Focusing

viewFocusImpl :: InterGraphImpl -> Focus InterEdgeImpl
viewFocusImpl = view _focus

updateFocusImpl :: Focus InterEdgeImpl -> InterGraphImpl -> InterGraphImpl
updateFocusImpl = set _focus


------
-- Highlighting

highlightImpl :: NodeId -> InterGraphImpl -> InterGraphImpl
highlightImpl nodeId = over _highlighted (Object.insert nodeId unit)

unHighlightImpl :: NodeId -> InterGraphImpl -> InterGraphImpl
unHighlightImpl nodeId = over _highlighted (Object.delete nodeId)


------
-- Validity

updateNodeValidityImpl :: Boolean -> NodeId -> InterGraphImpl -> InterGraphImpl
updateNodeValidityImpl validity nodeId = over (_nodes <<< (at nodeId)) (map (set _nodeIsValid validity))

updateEdgeValidityImpl :: Boolean -> InterEdgeImpl -> InterGraphImpl -> InterGraphImpl
updateEdgeValidityImpl validity (InterEdgeImpl edge) g =
  updateEdgeData edge.source edge.target (Just validity) g


------
-- Text

updateTextImpl :: NodeId -> String -> InterGraphImpl -> InterGraphImpl
updateTextImpl nodeId newText = over (_nodes <<< at nodeId) (map (set _text newText))


------
-- Demo

demo :: InterGraphImpl
demo = updateFocus (FocusEdge (InterEdgeImpl { source: "title"
                                         , target: "goofus"
                                         , isValid: false
                                         })
                    [InterEdgeImpl { source: "title"
                               , target: "goofus"
                               , isValid: false
                               },
                     InterEdgeImpl { source: "thingo"
                               , target: "goofus"
                               , isValid: false
                               }])
       $ highlight "thingo"
       $ addEdge (InterEdgeImpl { source : "title"
                                 , target : "goofus"
                                 , isValid : false
                                 })
       $ insertNode (InterNodeImpl
           { text: "Title: Workflow"
           , isValid: true
           , position : { x : 205.0
                        , y : 150.0
                        }
           , id : "title"
           , parents : Object.empty
           , children : Object.empty
           , subgraph : emptyInterGraphImpl
           })
       $ addEdge (InterEdgeImpl { source : "thingo"
                                 , target : "goofus"
                                 , isValid : false
                                 })
       $ insertNode (InterNodeImpl
           { text: "thingo"
           , isValid: false
           , position : { x : 205.0
                        , y : 100.0
                        }
           , id : "thingo"
           , parents : Object.empty
           , children : Object.empty
           , subgraph : emptyInterGraphImpl
           })
       $ insertNode (InterNodeImpl
           { text: "asdf"
           , isValid: true
           , position : { x: 450.0
                        , y: 270.0
                        }
           , id : "goofus"
           , parents : Object.empty
           , children : Object.empty
           , subgraph : emptyInterGraphImpl
           })
       $ emptyInterGraphImpl

-- TODO: re-export to JS using module system properly
fromMaybe_ :: forall a. a -> Maybe a -> a
fromMaybe_ = fromMaybe

-- Give some polymorphic functions concrete instances to make them
-- easier to call from JavaScript
resolvedInterGraphEdges :: InterGraphImpl -> Array { source :: InterNodeImpl, target :: InterNodeImpl }
resolvedInterGraphEdges = resolvedGraphEdges

lookupNodesImpl :: NodeIdSet -> InterGraphImpl -> Array InterNodeImpl
lookupNodesImpl = lookupNodes

interGraphToJSON :: InterGraphImpl -> String
interGraphToJSON = graphToJSON

interGraphFromJSON :: String -> ExceptT (NonEmptyList ForeignError) Identity InterGraphImpl
interGraphFromJSON = graphFromJSON

edgeInFocusGroupImpl :: InterGraphImpl -> InterEdgeImpl -> Boolean
edgeInFocusGroupImpl = edgeInFocusGroup

removeFocusImpl :: InterGraphImpl -> InterGraphImpl
removeFocusImpl = removeFocus

clearHighlightedImpl :: InterGraphImpl -> InterGraphImpl
clearHighlightedImpl = clearHighlighted

highlightFocusImpl :: InterGraphImpl -> InterGraphImpl
highlightFocusImpl = highlightFocus

toggleHighlightFocusImpl :: InterGraphImpl -> InterGraphImpl
toggleHighlightFocusImpl = toggleHighlightFocus

traverseUpImpl :: InterGraphImpl -> InterGraphImpl
traverseUpImpl = traverseUp

traverseDownImpl :: InterGraphImpl -> InterGraphImpl
traverseDownImpl = traverseDown

traverseLeftImpl :: InterGraphImpl -> InterGraphImpl
traverseLeftImpl = traverseLeft

traverseRightImpl :: InterGraphImpl -> InterGraphImpl
traverseRightImpl = traverseRight

graphTitleImpl :: InterGraphImpl -> Maybe String
graphTitleImpl = graphTitle

newParentOfFocusImpl :: InterGraphImpl -> Effect InterGraphImpl
newParentOfFocusImpl = newParentOfFocus newNodeOffset

newChildOfFocusImpl :: InterGraphImpl -> Effect InterGraphImpl
newChildOfFocusImpl = newChildOfFocus newNodeOffset

toggleGroupExpandImpl :: InterGraphImpl -> InterGraphImpl
toggleGroupExpandImpl  = toggleGroupExpand
