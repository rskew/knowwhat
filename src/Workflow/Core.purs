module Workflow.Core where

import Prelude

import Control.Monad.Except.Trans (ExceptT, except, withExceptT)
import Data.Array as Array
import Data.Either (Either(..), note)
import Data.Map (Map)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', Traversal', lens, traversed, (%~))
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.List.NonEmpty (NonEmptyList, singleton)
import Data.Identity (Identity)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.UUID (UUID, parseUUID)
import Data.UUID as UUID
import Foreign (ForeignError(..))
import Foreign.Class (class Encode, class Decode)
import Foreign.Generic (defaultOptions, genericEncode, genericDecode)
import Foreign.Object (Object)
import Foreign.Object as Object
import Point2D (Point2D)


------
-- Types

type NodeId = UUID

type EdgeId = { source :: NodeId, target :: NodeId }

-- | The dual graph is the graph with the edge directions flipped.
-- | Using the dual representation allows the use of the symmetry of edge flipping
-- | explicitly.
-- |
-- | The lookupEdge, insertEdge and deleteEdge functions respect the
-- | current view (dual or not dual) of the graph. withDual allows running
-- | a graph operation temprarily with the dual representation.
withDual :: (Graph -> Graph) -> Graph -> Graph
withDual op graph =
  graph
  # _isDual %~ not
  # op
  # _isDual %~ not

type EdgeInner =
  { id :: EdgeId
  , text :: String
  , isValid :: Boolean
  }

newtype Edge = Edge EdgeInner

derive instance eqEdge :: Eq Edge

derive instance ordEdge :: Ord Edge

derive instance genericEdge :: Generic Edge _

instance showEdge :: Show Edge where
  show (Edge edge) = show edge

type NodeInner =
  { id :: NodeId
  , children :: Map NodeId Edge
  , parents :: Map NodeId Edge
  , subgraph :: Graph
  , position :: Point2D
  , text :: String
  , isValid :: Boolean
  }

newtype Node = Node NodeInner

derive instance eqNode :: Eq Node

derive instance ordNode :: Ord Node

derive instance genericNode :: Generic Node _

instance showNode :: Show Node where
  show (Node node) = show node

data Focus =
  FocusNode NodeId
  | FocusEdge EdgeId (Array EdgeId)
  | NoFocus

derive instance eqFocus :: Eq Focus

derive instance ordFocus :: Ord Focus

derive instance genericFocus :: Generic Focus _

instance showFocus :: Show Focus where
  show = genericShow

type GraphInner =
  { nodes :: Map NodeId Node
  , isDual :: Boolean
  , focus :: Focus
  , highlighted :: Set NodeId
  }

newtype Graph = Graph GraphInner

derive instance eqGraph :: Eq Graph

derive instance ordGraph :: Ord Graph

derive instance genericGraph :: Generic Graph _

instance showGraph :: Show Graph where
  show (Graph graph) = show graph


------
-- Lenses

_source :: Lens' Edge NodeId
_source = _edgeId <<< prop (SProxy :: SProxy "source")

_target :: Lens' Edge NodeId
_target = _edgeId <<< prop (SProxy :: SProxy "target")

_sourceTarget :: NodeId -> NodeId -> Traversal' Graph (Maybe Edge)
_sourceTarget source target = _nodes <<< at source <<< traversed <<< _children <<< at target

_targetSource :: NodeId -> NodeId -> Traversal' Graph (Maybe Edge)
_targetSource source target = _nodes <<< at target <<< traversed <<< _parents <<< at source

_Graph :: Lens' Graph GraphInner
_Graph = lens (\(Graph g) -> g) (\_ -> Graph)

_Node :: Lens' Node NodeInner
_Node = lens (\(Node n) -> n) (\_ -> Node)

_Edge :: Lens' Edge EdgeInner
_Edge = lens (\(Edge e) -> e) (\_ -> Edge)

_isDual :: Lens' Graph Boolean
_isDual = _Graph <<< prop (SProxy :: SProxy "isDual")

_nodes :: Lens' Graph (Map NodeId Node)
_nodes = _Graph <<< prop (SProxy :: SProxy "nodes")

_parents :: Lens' Node (Map NodeId Edge)
_parents = _Node <<< prop (SProxy :: SProxy "parents")

_children :: Lens' Node (Map NodeId Edge)
_children = _Node <<< prop (SProxy :: SProxy "children")

_nodeId :: Lens' Node NodeId
_nodeId = _Node <<< prop (SProxy :: SProxy "id")

_subgraph :: Lens' Node Graph
_subgraph = _Node <<< prop (SProxy :: SProxy "subgraph")

_edgeId :: Lens' Edge EdgeId
_edgeId = _Edge <<< prop (SProxy :: SProxy "id")

_isValidEdge :: Lens' Edge Boolean
_isValidEdge = _Edge <<< prop (SProxy :: SProxy "isValid")

_isValidNode :: Lens' Node Boolean
_isValidNode = _Node <<< prop (SProxy :: SProxy "isValid")

_pos :: Lens' Node Point2D
_pos = _Node <<< prop (SProxy :: SProxy "position")

_x :: Lens' Point2D Number
_x = prop (SProxy :: SProxy "x")

_y :: Lens' Point2D Number
_y = prop (SProxy :: SProxy "y")

_nodeText :: Lens' Node String
_nodeText = _Node <<< prop (SProxy :: SProxy "text")

_edgeText :: Lens' Edge String
_edgeText = _Edge <<< prop (SProxy :: SProxy "text")

_focus :: Lens' Graph Focus
_focus = _Graph <<< prop (SProxy :: SProxy "focus")

_highlighted :: Lens' Graph (Set NodeId)
_highlighted = _Graph <<< prop (SProxy :: SProxy "highlighted")


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

newtype ForeignEdge =
  ForeignEdge
  { id :: ForeignEdgeId
  , text :: String
  , isValid :: Boolean
  }
derive instance genericForeignEdge :: Generic ForeignEdge _
instance encodeForeignEdge :: Encode ForeignEdge where
  encode = genericEncode defaultOptions
instance decodeForeignEdge :: Decode ForeignEdge where
  decode = genericDecode defaultOptions

newtype ForeignNode =
  ForeignNode
  { id :: ForeignNodeId
  , children :: Object Edge
  , parents :: Object Edge
  , subgraph :: Graph
  , position :: Point2D
  , text :: String
  , isValid :: Boolean
  }
derive instance genericForeignNode :: Generic ForeignNode _
instance encodeForeignNode :: Encode ForeignNode where
  encode = genericEncode defaultOptions
instance decodeForeignNode :: Decode ForeignNode where
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

newtype ForeignGraph =
  ForeignGraph
  { nodes :: Object Node
  , isDual :: Boolean
  , focus :: ForeignFocus
  , highlighted :: Array ForeignNodeId
  }
derive instance genericForeignGraph :: Generic ForeignGraph _
instance encodeForeignGraph :: Encode ForeignGraph where
  encode x = genericEncode defaultOptions x
instance decodeForeignGraph :: Decode ForeignGraph where
  decode x = genericDecode defaultOptions x


------
-- Serialisation

toForeignEdgeId :: EdgeId -> ForeignEdgeId
toForeignEdgeId edgeId =
  ForeignEdgeId $
  { source : UUID.toString edgeId.source
  , target : UUID.toString edgeId.target
  }

toForeignEdge :: Edge -> ForeignEdge
toForeignEdge (Edge edge) =
  ForeignEdge $
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

toForeignNode :: Node -> ForeignNode
toForeignNode (Node node) =
  ForeignNode $
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

toForeignGraph :: Graph -> ForeignGraph
toForeignGraph (Graph graph) =
  ForeignGraph $
  graph { nodes = toForeignMap identity UUID.toString graph.nodes
        , highlighted = UUID.toString <$> Array.fromFoldable graph.highlighted
        , focus = toForeignFocus graph.focus
        }

instance encodeEdge :: Encode Edge where
  encode x = x # toForeignEdge >>> genericEncode defaultOptions

instance encodeNode :: Encode Node where
  encode x = x # toForeignNode >>> genericEncode defaultOptions

instance encodeGraph :: Encode Graph where
  encode x = x # toForeignGraph >>> genericEncode defaultOptions


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

fromForeignEdge :: ForeignEdge -> Either String Edge
fromForeignEdge (ForeignEdge foreignEdge) = do
  id <- fromForeignEdgeId foreignEdge.id
  pure $ Edge $ foreignEdge { id = id }

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

fromForeignNode :: ForeignNode -> Either String Node
fromForeignNode (ForeignNode foreignNode) = do
  id <- parseUUIDEither foreignNode.id
  children <- fromForeignMap pure parseUUIDEither foreignNode.children
  parents <- fromForeignMap pure parseUUIDEither foreignNode.parents
  let subgraph = foreignNode.subgraph
  pure $ Node $
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

fromForeignGraph :: ForeignGraph -> Either String Graph
fromForeignGraph (ForeignGraph foreignGraph) = do
  nodes <- fromForeignMap pure parseUUIDEither foreignGraph.nodes
  highlighted <- Set.fromFoldable <$> traverse parseUUIDEither foreignGraph.highlighted
  focus <- fromForeignFocus foreignGraph.focus
  pure $ Graph $
    foreignGraph { nodes = nodes
                 , highlighted = highlighted
                 , focus = focus
                 }

toExceptT :: forall a. Either String a -> ExceptT (NonEmptyList ForeignError) Identity a
toExceptT = except >>> withExceptT (singleton <<< ForeignError)

instance decodeEdge :: Decode Edge where
  decode x = x # genericDecode defaultOptions >>= fromForeignEdge >>> toExceptT

instance decodeNode :: Decode Node where
  decode x = x # genericDecode defaultOptions >>= fromForeignNode >>> toExceptT

instance decodeGraph :: Decode Graph where
  decode x = x # genericDecode defaultOptions >>= fromForeignGraph >>> toExceptT
