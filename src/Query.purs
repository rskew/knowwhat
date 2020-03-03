module Query where

import Prelude

import AppState (GraphState, _graph, emptyGraphState)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Lens ((.~), (%~))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.NonEmpty as NonEmpty
import Data.Tuple (Tuple(..))
import Data.UUID as UUID
import HasuraQuery (class RowListEncodeJSON, GraphQLMutation, GraphQLQuery(..), deleteOperation, upsertOperation)
import Megagraph (Edge, EdgeId, EdgeRow, GraphId, NodeId, NodeRow, Node, _nodes, _text, _title, batchInsertEdges)
import Record.Extra (class Keys)
import Type.Prelude (class RowToList, RProxy(..), SProxy(..))

type GraphRow
  = ( id :: GraphId
    , title :: String
    )

type GraphSchema
  = ( graphs :: Record GraphRow
    , nodes :: Node
    , edges :: Edge
    )

renderGraphUpsertQuery :: Array (Record GraphRow) -> GraphQLMutation GraphSchema
renderGraphUpsertQuery graphRows =
  upsertOperation (SProxy :: SProxy "graphs") graphRows

renderGraphDeleteQuery :: Array GraphId -> GraphQLMutation GraphSchema
renderGraphDeleteQuery ids = deleteOperation (SProxy :: SProxy "graphs") ids

renderNodeUpsertQuery ::
  forall nodeRowL.
  RowToList NodeRow nodeRowL
  => Keys nodeRowL
  => RowListEncodeJSON NodeRow nodeRowL
  => Array Node
  -> GraphQLMutation GraphSchema
renderNodeUpsertQuery nodes = upsertOperation (SProxy :: SProxy "nodes") nodes

--renderNodeUpdateQuery ::
--  forall fields r fieldsL.
--  Union fields r NodeRow
--  => RowToList (id :: NodeId | fields) fieldsL
--  => Keys fieldsL
--  => RowListEncodeJSON (id :: NodeId | fields) fieldsL
--  => Record (id :: NodeId | fields)
--  -> GraphQLMutation GraphSchema
--renderNodeUpdateQuery fields =
--  updateOperation (SProxy :: SProxy "nodes") fields

renderNodeDeleteQuery :: Array NodeId -> GraphQLMutation GraphSchema
renderNodeDeleteQuery ids = deleteOperation (SProxy :: SProxy "nodes") ids

renderEdgeUpsertQuery ::
  forall edgeRowL.
  RowToList EdgeRow edgeRowL
  => Keys edgeRowL
  => Array Edge
  -> GraphQLMutation GraphSchema
renderEdgeUpsertQuery edges =
  upsertOperation (SProxy :: SProxy "edges") edges

--edgeUpdateQuery :: forall fields r fieldsL.
--                   Union fields r EdgeRow
--                   => RowToList (id :: EdgeId | fields) fieldsL
--                   => Keys fieldsL
--                   => RowListEncodeJSON (id :: EdgeId | fields) fieldsL
--                   => Record (id :: EdgeId | fields)
--                   -> GraphQLMutation GraphSchema
--edgeUpdateQuery fields =
--  updateOperation (SProxy :: SProxy "edges") fields

renderEdgeDeleteQuery :: Array EdgeId -> GraphQLMutation GraphSchema
renderEdgeDeleteQuery ids = deleteOperation (SProxy :: SProxy "edges") ids

-- TODO type safety by constraining query subfields by schema fields
-- a.k.a using purescript's type system to validate the query format
-- against the schema and general well-formedness.
--
-- Literally just a type-level version of the below query string that
-- can be checked against a schema with type-level slow-gramming
--
-- And then type-directed parsing of the response
graphFetchQuery :: GraphId -> GraphQLQuery GraphSchema
graphFetchQuery graphId =
  Query RProxy
  $ NonEmpty.singleton
  $ "graphs(where: {id: {_eq: \"" <> UUID.toString graphId <> "\"}}) {\
    \  id\
    \  title\
    \  nodes {\
    \    id\
    \    graphId\
    \    subgraph\
    \    positionX\
    \    positionY\
    \    text\
    \    isValid\
    \  }\
    \  edges {\
    \    id\
    \    graphId\
    \    source\
    \    target\
    \    midpointAngle\
    \    midpointRadius\
    \    text\
    \    isValid\
    \  }\
    \}"

type GraphFetchResponse
  = { data ::
      { graphs :: Array
        { id :: GraphId
        , title :: String
        , nodes :: Array (Record NodeRow)
        , edges :: Array (Record EdgeRow)
        }
      }
    }

parseGraphFetchResponse :: Json -> Either String GraphState
parseGraphFetchResponse json =
  let
    rect = { width : 0.0, left : 0.0, right : 0.0, height : 0.0, top : 0.0, bottom : 0.0 }
  in do
    graphFetchResponse :: GraphFetchResponse <- decodeJson json
    case Array.head graphFetchResponse.data.graphs of
      Nothing -> Left "No graph returned from query"
      Just graphData ->
        let
          nodes = Map.fromFoldable (map (\row -> Tuple row.id row) graphData.nodes)
        in
          pure $ emptyGraphState graphData.id rect
               # _graph <<< _nodes .~ nodes
               # _graph <<< _title <<< _text .~ graphData.title
               # _graph %~ batchInsertEdges graphData.edges

type GraphQLWebsocketResponse
  = { type :: String
    , id :: String
    , payload :: Json
    }
