-- | A schema is a row type with items of:
-- |   tableName :: Record tableRow
-- |
-- | Mutations are type-safe, queries are TODO
module HasuraQuery where

import Prelude

import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode (encodeJson)
import Data.Array as Array
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
  "{ " <>
  String.joinWith " " (NonEmpty.fromNonEmpty Array.cons subQueries) <>
  " }"

mutationToString :: forall schema. GraphQLMutation schema -> String
mutationToString (Mutation _ subMutation) =
  "mutation { " <>
  String.joinWith " " (NonEmpty.fromNonEmpty Array.cons subMutation) <>
  " }"

renderQuery :: forall schema. GraphQLQuery schema -> String
renderQuery query =
  stringify $ encodeJson $ { type : "start"
                           , id : ""
                           , payload : { query : queryToString query }
                           }

renderMutation :: forall schema. GraphQLMutation schema -> String
renderMutation mutation =
  stringify $ encodeJson $ { type : "start"
                           , id : ""
                           , payload : { query : mutationToString mutation }
                           }

upsertOperation :: forall schema tableName tableRow tableRowL r.
                   IsSymbol tableName
                   -- Table is in the schema
                   => Cons tableName (Record tableRow) r schema
                   -- So we can get the keys of the vars record
                   => RowToList tableRow tableRowL
                   => Keys tableRowL
                   -- So the vars are JSON encodable
                   => RowListEncodeJSON tableRow tableRowL
                   => SProxy tableName
                   -> Record tableRow
                   -> GraphQLMutation schema
upsertOperation _ vars =
  let
    tableName = reflectSymbol (SProxy :: SProxy tableName)
    varNames = Array.fromFoldable $ keys (RProxy :: RProxy tableRow)
    varsStr =
      vars
      # rowListEncodeJSONImpl (RLProxy :: RLProxy tableRowL)
      <#> (\(Tuple field val) -> field <> ": " <> val)
      # String.joinWith ", "
  in
    Mutation RProxy
    $ NonEmpty.singleton
    $ "insert_" <> tableName <> "(\
      \    objects: [\
      \      {\
      \        " <> varsStr <> "\
      \      }\
      \    ]\
      \    on_conflict: {\
      \        constraint: " <> tableName <> "_pkey\
      \        update_columns: [" <> (String.joinWith ", " varNames) <> "]\
      \    }\
      \) {\
      \  affected_rows\
      \}"

updateOperation :: forall schema tableName tableRow fields fieldsL r r' r''.
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
                   -- So the vars are JSON encodable
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

deleteOperation :: forall schema tableName tableRow r r'.
                   IsSymbol tableName
                   -- Table is in the schema
                   => Cons tableName (Record tableRow) r schema
                   -- Schema must contain an id: UUID field
                   => Cons "id" UUID r' tableRow
                   => SProxy tableName
                   -> UUID
                   -> GraphQLMutation schema
deleteOperation _ id =
  let
    tableName = reflectSymbol (SProxy :: SProxy tableName)
  in
    Mutation RProxy
    $ NonEmpty.singleton
    $ "delete_" <> tableName <> "(\
      \  where: {id: {_eq: \"" <> UUID.toString id <> "\"}}\
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
