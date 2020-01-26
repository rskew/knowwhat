module Foreign.Utils where

import Prelude

import Control.Monad.Except.Trans (ExceptT, except, withExceptT)
import Data.Array ((!!))
import Data.Either (Either, note)
import Data.Identity (Identity)
import Data.List.NonEmpty (NonEmptyList, singleton)
import Data.Map (Map)
import Data.Map as Map
import Data.String (Pattern(..), split)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.UUID (UUID, parseUUID)
import Data.UUID as UUID
import Foreign (ForeignError(..), MultipleErrors, renderForeignError)
import Foreign.Object (Object)
import Foreign.Object as Object
import Graph (EdgeMetadata)


toForeignMap :: forall a b c. (a -> b) -> (c -> String) -> Map c a -> Object b
toForeignMap toForeignValue showKey someMap =
  let
    tuples :: Array (Tuple c a)
    tuples = Map.toUnfoldable someMap
    foreignTuples = (\(Tuple key value) ->
                      Tuple (showKey key) (toForeignValue value)) <$> tuples
  in
    Object.fromFoldable foreignTuples

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

toExceptT :: forall a. Either String a -> ExceptT (NonEmptyList ForeignError) Identity a
toExceptT = except >>> withExceptT (singleton <<< ForeignError)

parseUUIDEither :: String -> Either String UUID
parseUUIDEither = parseUUID >>> note "failed to convert UUID from foreign"

edgeMetadataToString :: EdgeMetadata -> String
edgeMetadataToString edgeMetadata = UUID.toString edgeMetadata.id
                        <> " "
                        <> UUID.toString edgeMetadata.source
                        <> " "
                        <> UUID.toString edgeMetadata.target

parseEdgeMetadataEither :: String -> Either String EdgeMetadata
parseEdgeMetadataEither edgeMetadataStr =
  let
    edgeMetadataStrs = split (Pattern " ") edgeMetadataStr
  in
  note "Failed to parse EdgeId" do
    edgeMetadata        <- edgeMetadataStrs !! 0 >>= parseUUID
    sourceId      <- edgeMetadataStrs !! 0 >>= parseUUID
    targetId      <- edgeMetadataStrs !! 2 >>= parseUUID
    pure { id     : edgeMetadata
         , source : sourceId
         , target : targetId
         }

showForeignError :: MultipleErrors -> String
showForeignError = show <<< map renderForeignError
