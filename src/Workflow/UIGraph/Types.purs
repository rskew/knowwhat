module Workflow.UIGraph.Types where

import Prelude

import Point2D (Point2D)
import Data.Array as Array
import Data.Traversable (traverse)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Either (Either(..), note)
import Foreign.Class (class Encode, class Decode)
import Data.Map (Map)
import Foreign.Generic (defaultOptions, genericEncode, genericDecode)
import Data.Map as Map
import Data.Set (Set)
import Data.Set as Set
import Data.UUID (UUID, parseUUID)
import Control.Monad.Except.Trans (ExceptT, except, withExceptT)
import Data.List.NonEmpty (NonEmptyList, singleton)
import Data.Identity (Identity)
import Foreign (ForeignError(..))
import Data.UUID as UUID
import Data.Tuple (Tuple(..))
import Workflow.Core (class Graph, EdgeId, NodeId)
import Foreign.Object (Object)
import Foreign.Object as Object
import Data.Lens (Lens', lens)
import Data.Lens.Record (prop)
import Data.Symbol (SProxy(..))


type UIEdgeInner =
  { id :: EdgeId
  , text :: String
  , isValid :: Boolean
  }
newtype UIEdge = UIEdge UIEdgeInner
derive instance eqUIEdge :: Eq UIEdge
derive instance ordUIEdge :: Ord UIEdge
derive instance genericUIEdge :: Generic UIEdge _
instance showUIEdge :: Show UIEdge where
  show (UIEdge edge) = show edge

type UINodeInner =
  { id :: NodeId
  , children :: Map NodeId UIEdge
  , parents :: Map NodeId UIEdge
  , subgraph :: UIGraph
  , position :: Point2D
  , text :: String
  , isValid :: Boolean
  }
newtype UINode = UINode UINodeInner
derive instance eqUINode :: Eq UINode
derive instance ordUINode :: Ord UINode
derive instance genericUINode :: Generic UINode _
instance showUINode :: Show UINode where
  show (UINode node) = show node

data Focus =
  FocusNode NodeId
  | FocusEdge EdgeId (Array EdgeId)
  | NoFocus
derive instance eqFocus :: Eq Focus
derive instance ordFocus :: Ord Focus
derive instance genericFocus :: Generic Focus _
instance showFocus :: Show Focus where
  show = genericShow

type UIGraphInner =
  { nodes :: Map NodeId UINode
  , isDual :: Boolean
  , focus :: Focus
  , highlighted :: Set NodeId
  }
newtype UIGraph = UIGraph UIGraphInner
derive instance eqUIGraph :: Eq UIGraph
derive instance ordUIGraph :: Ord UIGraph
derive instance genericUIGraph :: Generic UIGraph _
instance showUIGraph :: Show UIGraph where
  show (UIGraph graph) = show graph

instance graphUIGraph :: Graph UIGraph UINode UIEdge where
  _isDual = _isDualImpl
  _nodes = _nodesImpl
  _parents = _parentsImpl
  _children = _childrenImpl
  _nodeId = _nodeIdImpl
  _subgraph = _subgraphImpl
  _edgeId = _edgeIdImpl

_UIGraph :: Lens' UIGraph UIGraphInner
_UIGraph = lens (\(UIGraph g) -> g) (\_ -> UIGraph)

_UINode :: Lens' UINode UINodeInner
_UINode = lens (\(UINode n) -> n) (\_ -> UINode)

_UIEdge :: Lens' UIEdge UIEdgeInner
_UIEdge = lens (\(UIEdge e) -> e) (\_ -> UIEdge)

_isDualImpl :: Lens' UIGraph Boolean
_isDualImpl = _UIGraph <<< prop (SProxy :: SProxy "isDual")

_nodesImpl :: Lens' UIGraph (Map NodeId UINode)
_nodesImpl = _UIGraph <<< prop (SProxy :: SProxy "nodes")

_parentsImpl :: Lens' UINode (Map NodeId UIEdge)
_parentsImpl = _UINode <<< prop (SProxy :: SProxy "parents")

_childrenImpl :: Lens' UINode (Map NodeId UIEdge)
_childrenImpl = _UINode <<< prop (SProxy :: SProxy "children")

_nodeIdImpl :: Lens' UINode NodeId
_nodeIdImpl = _UINode <<< prop (SProxy :: SProxy "id")

_subgraphImpl :: Lens' UINode UIGraph
_subgraphImpl = _UINode <<< prop (SProxy :: SProxy "subgraph")

_edgeIdImpl :: Lens' UIEdge EdgeId
_edgeIdImpl = _UIEdge <<< prop (SProxy :: SProxy "id")

_isValidEdge :: Lens' UIEdge Boolean
_isValidEdge = _UIEdge <<< prop (SProxy :: SProxy "isValid")

_isValidNode :: Lens' UINode Boolean
_isValidNode = _UINode <<< prop (SProxy :: SProxy "isValid")

_pos :: Lens' UINode Point2D
_pos = _UINode <<< prop (SProxy :: SProxy "position")

_x :: Lens' Point2D Number
_x = prop (SProxy :: SProxy "x")

_y :: Lens' Point2D Number
_y = prop (SProxy :: SProxy "y")

_nodeText :: Lens' UINode String
_nodeText = _UINode <<< prop (SProxy :: SProxy "text")

_edgeText :: Lens' UIEdge String
_edgeText = _UIEdge <<< prop (SProxy :: SProxy "text")

_focus :: Lens' UIGraph Focus
_focus = _UIGraph <<< prop (SProxy :: SProxy "focus")

_highlighted :: Lens' UIGraph (Set NodeId)
_highlighted = _UIGraph <<< prop (SProxy :: SProxy "highlighted")


------
-- Serialisation/deserialisation types

-- | Since Maps and Sets aren't Generic, need to convert them

type ForeignNodeId = String

newtype ForeignEdgeId =
  ForeignEdgeId
  { source :: ForeignNodeId
  , target :: ForeignNodeId
  }
derive instance genericForeignEdgeId :: Generic ForeignEdgeId _
instance encodeForeignEdgeId :: Encode ForeignEdgeId where
  encode = genericEncode defaultOptions
instance decodeForeignEdgeId :: Decode ForeignEdgeId where
  decode = genericDecode defaultOptions

newtype ForeignUIEdge =
  ForeignUIEdge
  { id :: ForeignEdgeId
  , text :: String
  , isValid :: Boolean
  }
derive instance genericForeignUIEdge :: Generic ForeignUIEdge _
instance encodeForeignUIEdge :: Encode ForeignUIEdge where
  encode = genericEncode defaultOptions
instance decodeForeignUIEdge :: Decode ForeignUIEdge where
  decode = genericDecode defaultOptions

newtype ForeignUINode =
  ForeignUINode
  { id :: ForeignNodeId
  , children :: Object UIEdge
  , parents :: Object UIEdge
  , subgraph :: UIGraph
  , position :: Point2D
  , text :: String
  , isValid :: Boolean
  }
derive instance genericForeignUINode :: Generic ForeignUINode _
instance encodeForeignUINode :: Encode ForeignUINode where
  encode = genericEncode defaultOptions
instance decodeForeignUINode :: Decode ForeignUINode where
  decode = genericDecode defaultOptions

data ForeignFocus
  = ForeignFocusNode ForeignNodeId
  | ForeignFocusEdge ForeignEdgeId (Array ForeignEdgeId)
  | ForeignNoFocus
derive instance genericForeignFocus :: Generic ForeignFocus _
instance encodeForeignFocus :: Encode ForeignFocus where
  encode = genericEncode defaultOptions
instance decodeForeignFocus :: Decode ForeignFocus where
  decode = genericDecode defaultOptions

newtype ForeignUIGraph =
  ForeignUIGraph
  { nodes :: Object UINode
  , isDual :: Boolean
  , focus :: ForeignFocus
  , highlighted :: Array ForeignNodeId
  }
derive instance genericForeignUIGraph :: Generic ForeignUIGraph _
instance encodeForeignUIGraph :: Encode ForeignUIGraph where
  encode x = genericEncode defaultOptions x
instance decodeForeignUIGraph :: Decode ForeignUIGraph where
  decode x = genericDecode defaultOptions x


------
-- Serialisation

toForeignEdgeId :: EdgeId -> ForeignEdgeId
toForeignEdgeId edgeId =
  ForeignEdgeId $
  { source : UUID.toString edgeId.source
  , target : UUID.toString edgeId.target
  }

toForeignUIEdge :: UIEdge -> ForeignUIEdge
toForeignUIEdge (UIEdge edge) =
  ForeignUIEdge $
  edge { id = toForeignEdgeId edge.id }

toForeignMap :: forall a b c. (a -> b) -> (c -> String) -> Map c a -> Object b
toForeignMap toForeignValue showKey someMap =
  let
    tuples :: Array (Tuple c a)
    tuples = Map.toUnfoldable someMap
    foreignTuples = (\(Tuple key value) ->
                      Tuple (showKey key) (toForeignValue value)) <$> tuples
  in
   Object.fromFoldable foreignTuples

toForeignUINode :: UINode -> ForeignUINode
toForeignUINode (UINode node) =
  ForeignUINode $
  node { id = UUID.toString node.id
       , children = toForeignMap identity UUID.toString node.children
       , parents = toForeignMap identity UUID.toString node.parents
       , subgraph = node.subgraph
       }

toForeignFocus :: Focus -> ForeignFocus
toForeignFocus (FocusNode nodeId) = ForeignFocusNode $ UUID.toString nodeId
toForeignFocus (FocusEdge edgeId edgeIdSet) =
  ForeignFocusEdge
  (toForeignEdgeId edgeId)
  (toForeignEdgeId <$> Array.fromFoldable edgeIdSet)
toForeignFocus NoFocus = ForeignNoFocus

toForeignUIGraph :: UIGraph -> ForeignUIGraph
toForeignUIGraph (UIGraph graph) =
  ForeignUIGraph $
  graph { nodes = toForeignMap identity UUID.toString graph.nodes
        , highlighted = UUID.toString <$> Array.fromFoldable graph.highlighted
        , focus = toForeignFocus graph.focus
        }

instance encodeUIEdge :: Encode UIEdge where
  encode x = x # toForeignUIEdge >>> genericEncode defaultOptions

instance encodeUINode :: Encode UINode where
  encode x = x # toForeignUINode >>> genericEncode defaultOptions

instance encodeUIGraph :: Encode UIGraph where
  encode x = x # toForeignUIGraph >>> genericEncode defaultOptions


------
-- Deserialisation

fromForeignEdgeId :: ForeignEdgeId -> Either String EdgeId
fromForeignEdgeId (ForeignEdgeId foreignEdgeId) =
  note "Failed to convert EdgeId from foreign" do
  source <- parseUUID foreignEdgeId.source
  target <- parseUUID foreignEdgeId.target
  pure $ { source : source
         , target : target
         }

fromForeignUIEdge :: ForeignUIEdge -> Either String UIEdge
fromForeignUIEdge (ForeignUIEdge foreignEdge) = do
  id <- fromForeignEdgeId foreignEdge.id
  pure $ UIEdge $ foreignEdge { id = id }

fromForeignMap :: forall a b c s. Ord c =>
                  (b -> Either s a) -> (String -> Either s c) -> Object b -> Either s (Map c a)
fromForeignMap fromForeignValue unShowKey someObject =
  let
    foreignTuples :: Array (Tuple String b)
    foreignTuples = Object.toUnfoldable someObject
    tuples = traverse
             (\(Tuple str foreignValue) -> do
                 key <- unShowKey str
                 value <- fromForeignValue foreignValue
                 pure (Tuple key value))
             foreignTuples
  in
    Map.fromFoldable <$> tuples

parseUUIDEither :: String -> Either String UUID
parseUUIDEither = parseUUID >>> note "failed to convert UUID from foreign"

fromForeignUINode :: ForeignUINode -> Either String UINode
fromForeignUINode (ForeignUINode foreignNode) = do
  id <- parseUUIDEither foreignNode.id
  children <- fromForeignMap pure parseUUIDEither foreignNode.children
  parents <- fromForeignMap pure parseUUIDEither foreignNode.parents
  let subgraph = foreignNode.subgraph
  pure $ UINode $
    foreignNode { id = id
                , children = children
                , parents = parents
                , subgraph = subgraph
                }

fromForeignFocus :: ForeignFocus -> Either String Focus
fromForeignFocus foreignFocus =
  case foreignFocus of
    ForeignNoFocus -> Right NoFocus
    ForeignFocusNode foreignNodeId ->
      FocusNode <$> parseUUIDEither foreignNodeId
    ForeignFocusEdge foreignEdgeId foreignEdgeIdSet -> do
      edgeId <- fromForeignEdgeId foreignEdgeId
      edgeIdSet <- traverse fromForeignEdgeId foreignEdgeIdSet
      pure $ FocusEdge edgeId edgeIdSet

fromForeignUIGraph :: ForeignUIGraph -> Either String UIGraph
fromForeignUIGraph (ForeignUIGraph foreignGraph) = do
  nodes <- fromForeignMap pure parseUUIDEither foreignGraph.nodes
  highlighted <- Set.fromFoldable <$> traverse parseUUIDEither foreignGraph.highlighted
  focus <- fromForeignFocus foreignGraph.focus
  pure $ UIGraph $
    foreignGraph { nodes = nodes
                 , highlighted = highlighted
                 , focus = focus
                 }

toExceptT :: forall a. Either String a -> ExceptT (NonEmptyList ForeignError) Identity a
toExceptT = except >>> withExceptT (singleton <<< ForeignError)

instance decodeUIEdge :: Decode UIEdge where
  decode x = x # genericDecode defaultOptions >>= fromForeignUIEdge >>> toExceptT

instance decodeUINode :: Decode UINode where
  decode x = x # genericDecode defaultOptions >>= fromForeignUINode >>> toExceptT

instance decodeUIGraph :: Decode UIGraph where
  decode x = x # genericDecode defaultOptions >>= fromForeignUIGraph >>> toExceptT
