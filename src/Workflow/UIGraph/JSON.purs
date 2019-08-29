-- TODO: remove. All functionality now in ForeignUIGraph and UIGraph
module Workflow.UIGraph.JSON where

import Prelude

import Workflow.Core (EdgeId)
import Workflow.UIGraph (UIGraph(..), UINode(..), UIEdge(..), Focus(..))
import Workflow.UIGraph.ForeignUIGraph (ForeignUIGraph(..), ForeignUINode(..), ForeignUIEdge(..), ForeignFocus(..), ForeignEdgeId(..), genericEncodeOpts)

import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Identity (Identity(..))
import Data.List.NonEmpty (NonEmptyList)
import Data.Map (Map)
import Data.Map as Map
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.UUID (UUID, parseUUID)
import Data.UUID as UUID
import Foreign (ForeignError, renderForeignError)
import Foreign.Generic (genericDecodeJSON, genericEncodeJSON)
import Foreign.Object (Object)
import Foreign.Object as Object


-- TODO: use rowToList from the typelevel prelude to convert Map, Set to Object for any type

------
-- Serialisation

objectifyEdgeId :: EdgeId -> ForeignEdgeId
objectifyEdgeId edgeId =
  ForeignEdgeId $
  { source : UUID.toString edgeId.source
  , target : UUID.toString edgeId.target
  }

objectifyUIEdge :: UIEdge -> ForeignUIEdge
objectifyUIEdge (UIEdge edge) =
  ForeignUIEdge $
  edge { id = objectifyEdgeId edge.id }

objectifyMap :: forall a b c. (a -> b) -> (c -> String) -> Map c a -> Object b
objectifyMap objectifyValue showKey someMap =
  let
    tuples :: Array (Tuple c a)
    tuples = Map.toUnfoldable someMap
    foreignTuples = (\(Tuple key value) -> Tuple (showKey key) (objectifyValue value)) <$> tuples
  in
   Object.fromFoldable foreignTuples

objectifyUINode :: UINode -> ForeignUINode
objectifyUINode (UINode node) =
  ForeignUINode $
  node { id = UUID.toString node.id
       , children = objectifyMap objectifyUIEdge UUID.toString node.children
       , parents = objectifyMap objectifyUIEdge UUID.toString node.parents
       , subgraph = objectifyUIGraph $ node.subgraph
       }

objectifyFocus :: Focus -> ForeignFocus
objectifyFocus (FocusNode nodeId) = ForeignFocusNode $ UUID.toString nodeId
objectifyFocus (FocusEdge edgeId edgeIdSet) =
  ForeignFocusEdge
  (objectifyEdgeId edgeId)
  (objectifyEdgeId <$> Array.fromFoldable edgeIdSet)
objectifyFocus NoFocus = ForeignNoFocus

objectifyUIGraph :: UIGraph -> ForeignUIGraph
objectifyUIGraph (UIGraph graph) =
  ForeignUIGraph $
  graph { nodes = objectifyMap objectifyUINode UUID.toString graph.nodes
        , highlighted = UUID.toString <$> Array.fromFoldable graph.highlighted
        , focus = objectifyFocus graph.focus
        }

uiGraphToJson :: UIGraph -> String
uiGraphToJson =
  objectifyUIGraph >>> genericEncodeJSON genericEncodeOpts


------
-- Deserialisation

unObjectifyEdgeId :: ForeignEdgeId -> Either String EdgeId
unObjectifyEdgeId (ForeignEdgeId foreignEdgeId) =
  note "Failed to convert EdgeId from foreign" do
  source <- parseUUID foreignEdgeId.source
  target <- parseUUID foreignEdgeId.target
  pure $ { source : source
         , target : target
         }

unObjectifyUIEdge :: ForeignUIEdge -> Either String UIEdge
unObjectifyUIEdge (ForeignUIEdge foreignEdge) = do
  id <- unObjectifyEdgeId foreignEdge.id
  pure $ UIEdge $ foreignEdge { id = id }

unObjectifyMap :: forall a b c s. Ord c =>
                  (b -> Either s a) -> (String -> Either s c) -> Object b -> Either s (Map c a)
unObjectifyMap unObjectifyValue unShowKey someObject =
  let
    foreignTuples :: Array (Tuple String b)
    foreignTuples = Object.toUnfoldable someObject
    tuples = traverse
             (\(Tuple str foreignValue) -> do
                 key <- unShowKey str
                 value <- unObjectifyValue foreignValue
                 pure (Tuple key value))
             foreignTuples
  in
    Map.fromFoldable <$> tuples

parseUUIDEither :: String -> Either String UUID
parseUUIDEither = parseUUID >>> note "failed to convert UUID from foreign"

unObjectifyUINode :: ForeignUINode -> Either String UINode
unObjectifyUINode (ForeignUINode foreignNode) = do
  id <- parseUUIDEither foreignNode.id
  children <- unObjectifyMap unObjectifyUIEdge parseUUIDEither foreignNode.children
  parents <- unObjectifyMap unObjectifyUIEdge parseUUIDEither foreignNode.parents
  subgraph <- unObjectifyUIGraph foreignNode.subgraph
  pure $ UINode $
    foreignNode { id = id
                , children = children
                , parents = parents
                , subgraph = subgraph
                }

unObjectifyFocus :: ForeignFocus -> Either String Focus
unObjectifyFocus foreignFocus =
  case foreignFocus of
    ForeignNoFocus -> Right NoFocus
    ForeignFocusNode foreignNodeId ->
      FocusNode <$> parseUUIDEither foreignNodeId
    ForeignFocusEdge foreignEdgeId foreignEdgeIdSet -> do
      edgeId <- unObjectifyEdgeId foreignEdgeId
      edgeIdSet <- traverse unObjectifyEdgeId foreignEdgeIdSet
      pure $ FocusEdge edgeId edgeIdSet

unObjectifyUIGraph :: ForeignUIGraph -> Either String UIGraph
unObjectifyUIGraph (ForeignUIGraph foreignGraph) = do
  nodes <- unObjectifyMap unObjectifyUINode parseUUIDEither foreignGraph.nodes
  highlighted <- Set.fromFoldable <$> traverse parseUUIDEither foreignGraph.highlighted
  focus <- unObjectifyFocus foreignGraph.focus
  pure $ UIGraph $
    foreignGraph { nodes = nodes
                 , highlighted = highlighted
                 , focus = focus
                 }

uiGraphFromJson :: String -> Either String UIGraph
uiGraphFromJson json =
  let
    exceptTForeignUIGraphWithMeta :: ExceptT (NonEmptyList ForeignError) Identity ForeignUIGraph
    exceptTForeignUIGraphWithMeta = genericDecodeJSON genericEncodeOpts json
    Identity (eitherForeignUIGraphWithMeta) = runExceptT exceptTForeignUIGraphWithMeta
  in
    eitherForeignUIGraphWithMeta
    # lmap ((map renderForeignError) >>> show)
    # (flip bind) unObjectifyUIGraph
