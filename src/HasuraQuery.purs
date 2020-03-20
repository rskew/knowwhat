-- | A schema is a row type with items of:
-- |   tableName :: Record tableRow
-- |
-- | Mutations are type-safe, queries are stringy
module HasuraQuery where

import Prelude

import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Encode (encodeJson)
import Data.Array as Array
import Data.Array (length)
import Data.Array.NonEmpty (fromNonEmpty, toNonEmpty)
import Data.NonEmpty (NonEmpty)
import Data.NonEmpty as NonEmpty
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.UUID (UUID)
import Data.UUID as UUID
import Foreign.Class (class Encode)
import Foreign.Generic (encodeJSON)
import Prim.Row (class Cons)
import Prim.Row as Row
import Prim.RowList as RL
import Record (get)
import Record.Extra (class Keys, keys)
import Type.Prelude (class IsSymbol, class RowToList, class Union, RLProxy(..), RProxy(..), SProxy(..), reflectSymbol)
import Unsafe.Coerce (unsafeCoerce)

-- | GraphQLQuery and GraphQLMutation behave exactly the same, but cannot be
-- | combined.
data GraphQLQuery schema
  = Query (RProxy schema) (NonEmpty Array String)

data GraphQLMutation schema
  = Mutation (RProxy schema) (NonEmpty Array String)

emptyQuery :: forall schema. GraphQLQuery schema
emptyQuery = Query RProxy (NonEmpty.singleton "")

emptyMutation :: forall schema. GraphQLMutation schema
emptyMutation = Mutation RProxy (NonEmpty.singleton "")

instance semigroupGraphQLQuery :: Semigroup (GraphQLQuery schema) where
  append (Query _ subQueriesA) (Query _ subQueriesB) =
    Query RProxy $ toNonEmpty $ fromNonEmpty subQueriesA <> fromNonEmpty subQueriesB

instance semigroupGraphQLMutation :: Semigroup (GraphQLMutation schema) where
  append (Mutation _ subQueriesA) (Mutation _ subQueriesB) =
    Mutation RProxy $ toNonEmpty $ fromNonEmpty subQueriesA <> fromNonEmpty subQueriesB

queryToString :: forall schema. GraphQLQuery schema -> String
queryToString (Query _ subQueries) =
  "query { " <>
  String.joinWith " " (NonEmpty.fromNonEmpty Array.cons subQueries) <>
  " }"

mutationToString :: forall schema. GraphQLMutation schema -> String
mutationToString (Mutation _ subMutation) =
  "mutation { " <>
  String.joinWith " " (NonEmpty.fromNonEmpty Array.cons subMutation) <>
  " }"

renderQuery :: forall schema. UUID -> GraphQLQuery schema -> String
renderQuery id query =
  stringify $ encodeJson $ { type : "start"
                           , id : UUID.toString id
                           , payload : { query : queryToString query }
                           }

renderMutation :: forall schema. UUID -> GraphQLMutation schema -> String
renderMutation id mutation =
  stringify $ encodeJson $ { type : "start"
                           , id : UUID.toString id
                           , payload : { query : mutationToString mutation }
                           }

upsertOperation ::
  forall schema tableName tableRow tableRowL r.
  IsSymbol tableName
  -- Table is in the schema
  => Cons tableName (Record tableRow) r schema
  -- We can get the keys of the tableRow record
  => RowToList tableRow tableRowL
  => Keys tableRowL
  -- The fields are JSON encodable
  => RowListEncodeJSON tableRow tableRowL
  => SProxy tableName
  -> Array (Record tableRow)
  -> GraphQLMutation schema
upsertOperation _ objects =
  if length objects == 0
  then emptyMutation
  else
    let
      tableName = reflectSymbol (SProxy :: SProxy tableName)
      varNames = Array.fromFoldable $ keys (RProxy :: RProxy tableRow)
      stringifyObject =
        rowListEncodeJSONImpl (RLProxy :: RLProxy tableRowL)
        >>> map (\(Tuple field val) -> field <> ": " <> val)
        >>> \fieldValsStr -> "{ " <> String.joinWith ", " fieldValsStr <> " }"
      objectsStr = "[ " <> String.joinWith " " (stringifyObject <$> objects) <> " ]"
    in
      Mutation RProxy
      $ NonEmpty.singleton
      $ "insert_" <> tableName <> "(\
        \    objects: " <> objectsStr <> "\
        \    on_conflict: {\
        \        constraint: " <> tableName <> "_pkey\
        \        update_columns: [" <> (String.joinWith ", " varNames) <> "]\
        \    }\
        \) {\
        \  affected_rows\
        \}"

updateOperation ::
  forall schema tableName tableRow fields fieldsL r r' r''.
  IsSymbol tableName
  -- Table is in the schema
  => Cons tableName (Record tableRow) r schema
  -- Schema must contain an id: UUID field
  => Cons "id" UUID r' tableRow
  -- The vars must be a subset of the schema fields
  => Union fields r'' tableRow
  -- Get the keys of the vars record
  => RowToList (id :: UUID | fields) fieldsL
  => Keys fieldsL
  -- The vars are JSON encodable
  => RowListEncodeJSON (id :: UUID | fields) fieldsL
  => SProxy tableName
  -> Record (id :: UUID | fields)
  -> GraphQLMutation schema
updateOperation _ vars =
  let
    tableName = reflectSymbol (SProxy :: SProxy tableName)
    varNames = Array.fromFoldable $ keys (RProxy :: RProxy ("id" :: UUID | fields))
    varsStr =
      vars
      # rowListEncodeJSONImpl (RLProxy :: RLProxy fieldsL)
      <#> (\(Tuple field val) -> field <> ": " <> val)
      # String.joinWith ", "
  in
    Mutation RProxy
    $ NonEmpty.singleton
    $ "update_" <> tableName <> "(\
      \    _set: {" <> varsStr <> "}\
      \    where: {id: {_eq: \"" <> UUID.toString (get (SProxy :: SProxy "id") vars) <> "\"}}\
      \) {\
      \    affected_rows\
      \}"

deleteOperation ::
  forall schema tableName tableRow r r'.
  IsSymbol tableName
  -- Table is in the schema
  => Cons tableName (Record tableRow) r schema
  -- Schema must contain an id: UUID field
  => Cons "id" UUID r' tableRow
  => SProxy tableName
  -> Array UUID
  -> GraphQLMutation schema
deleteOperation _ ids =
  if length ids == 0
  then emptyMutation
  else
    let
      tableName = reflectSymbol (SProxy :: SProxy tableName)
    in
      Mutation RProxy
      $ NonEmpty.singleton
      $ "delete_" <> tableName <> "(\
        \  where: {id: {_in: [ \"" <> String.joinWith "\" \"" (UUID.toString <$> ids) <> "\" ]}}\
        \) {\
        \  affected_rows\
        \}"


------
-- Utils

class RowListEncodeJSON r rL | rL -> r where
  rowListEncodeJSONImpl :: RLProxy rL -> Record r -> Array (Tuple String String)

instance nilRowListEncodeJSON :: RowListEncodeJSON () RL.Nil where
  rowListEncodeJSONImpl _ _ = []

instance consRowListEncodeJSON
         :: ( Row.Cons field ty tail r
            , RowListEncodeJSON tail tailL
            , IsSymbol field
            , Encode ty
            )
         => RowListEncodeJSON r (RL.Cons field ty tailL)
  where
    rowListEncodeJSONImpl _ rec = Array.cons (Tuple firstField (encodeJSON firstVal)) rest
      where
        firstField = reflectSymbol (SProxy :: SProxy field)
        firstVal = get (SProxy :: SProxy field) rec
        rest = rowListEncodeJSONImpl (RLProxy :: RLProxy tailL) $ unsafeCoerce rec

type GraphQLWebsocketResponse
  = { type :: String
    , id :: UUID
    , payload :: Json
    }
