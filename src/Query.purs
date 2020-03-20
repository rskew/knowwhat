module Query where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Lens ((.~))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.NonEmpty as NonEmpty
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.UUID (UUID)
import Data.UUID as UUID
import HasuraQuery (class RowListEncodeJSON, GraphQLMutation, GraphQLQuery(..), upsertOperation)
import Megagraph (Edge, EdgeId, EdgeMappingEdge, EdgeMappingEdgeRow, EdgeRow, Graph, GraphId, Mapping, MappingId, Node, NodeId, NodeMappingEdge, NodeMappingEdgeRow, NodeRow, _nodes, _text, _title, batchInsertEdges, emptyGraph)
import Record.Extra (class Keys)
import Type.Prelude (class RowToList, RProxy(..), SProxy(..))

type GraphRow
  = ( id :: GraphId
    , title :: String
    )

type MappingRow
  = ( id :: MappingId
    , title :: String
    , sourceGraph :: GraphId
    , targetGraph :: GraphId
    )

type MegagraphSchema
  = ( graphs :: Record GraphRow
    , nodes :: Node
    , edges :: Edge
    , mappings :: Record MappingRow
    , nodeMappingEdges :: NodeMappingEdge
    , edgeMappingEdges :: EdgeMappingEdge
    )

renderGraphUpsertQuery :: Array (Record GraphRow) -> GraphQLMutation MegagraphSchema
renderGraphUpsertQuery graphRows =
  upsertOperation (SProxy :: SProxy "graphs") graphRows

type GraphUpsertResponse
  = { data ::
      { insert_graphs ::
        { affected_rows :: Int
        }
      }
    }

parseGraphUpsertResponse :: Json -> Either String Int
parseGraphUpsertResponse json = do
  graphUpsertResponse :: GraphUpsertResponse <- decodeJson json
  pure $ graphUpsertResponse.data.insert_graphs.affected_rows

renderNodeUpsertQuery ::
  forall nodeRowL.
  RowToList NodeRow nodeRowL
  => Keys nodeRowL
  => RowListEncodeJSON NodeRow nodeRowL
  => Array Node
  -> GraphQLMutation MegagraphSchema
renderNodeUpsertQuery nodes = upsertOperation (SProxy :: SProxy "nodes") nodes

type NodeUpsertResponse
  = { data ::
      { insert_nodes ::
        { affected_rows :: Int
        }
      }
    }

parseNodeUpsertResponse :: Json -> Either String Int
parseNodeUpsertResponse json = do
  nodeUpsertResponse :: NodeUpsertResponse <- decodeJson json
  pure $ nodeUpsertResponse.data.insert_nodes.affected_rows

renderEdgeUpsertQuery ::
  forall edgeRowL.
  RowToList EdgeRow edgeRowL
  => Keys edgeRowL
  => Array Edge
  -> GraphQLMutation MegagraphSchema
renderEdgeUpsertQuery edges =
  upsertOperation (SProxy :: SProxy "edges") edges

renderMappingUpsertQuery :: Array (Record MappingRow) -> GraphQLMutation MegagraphSchema
renderMappingUpsertQuery mappingRows =
  upsertOperation (SProxy :: SProxy "mappings") mappingRows

renderNodeMappingEdgeUpsertQuery ::
  forall nodeMappingEdgeRowL.
  RowToList NodeMappingEdgeRow nodeMappingEdgeRowL
  => Keys nodeMappingEdgeRowL
  => RowListEncodeJSON NodeMappingEdgeRow nodeMappingEdgeRowL
  => Array NodeMappingEdge
  -> GraphQLMutation MegagraphSchema
renderNodeMappingEdgeUpsertQuery nodeMappingEdges = upsertOperation (SProxy :: SProxy "nodeMappingEdges") nodeMappingEdges

renderEdgeMappingEdgeUpsertQuery ::
  forall edgeMappingEdgeRowL.
  RowToList EdgeMappingEdgeRow edgeMappingEdgeRowL
  => Keys edgeMappingEdgeRowL
  => RowListEncodeJSON EdgeMappingEdgeRow edgeMappingEdgeRowL
  => Array EdgeMappingEdge
  -> GraphQLMutation MegagraphSchema
renderEdgeMappingEdgeUpsertQuery edgeMappingEdges = upsertOperation (SProxy :: SProxy "edgeMappingEdges") edgeMappingEdges

type MegagraphUpsertResponse
  = { data :: Json }

parseMegagraphUpsertResponse :: Json -> Either String MegagraphUpsertResponse
parseMegagraphUpsertResponse json =
  decodeJson json


------
-- Queries
--
-- It would be nice to make these type-safe :)
-- Pretty far down the roadmap though.

graphFetchQuery :: GraphId -> Array GraphId -> GraphQLQuery MegagraphSchema
graphFetchQuery graphId presentGraphIds =
  let
    presentGraphIdsStr = case presentGraphIds of
      [] -> ""
      many -> "\"" <> String.joinWith "\" \"" (UUID.toString <$> presentGraphIds) <> "\""
  in
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
      \    deleted\
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
      \    deleted\
      \  }\
      \}\
      \mappings (where: {_or: [\
      \   { sourceGraph: {_in: [\
      \       " <> presentGraphIdsStr <> "\
      \     ]}\
      \   }\
      \   { targetGraph: {_in: [\
      \       " <> presentGraphIdsStr <> "\
      \     ]}\
      \   }\
      \]}) {\
      \  id\
      \  sourceGraph\
      \  targetGraph\
      \  title\
      \  nodeMappingEdges {\
      \    id\
      \    mappingId\
      \    sourceNode\
      \    targetNode\
      \    midpointAngle\
      \    midpointRadius\
      \    deleted\
      \  }\
      \  edgeMappingEdges {\
      \    id\
      \    mappingId\
      \    sourceEdge\
      \    targetEdge\
      \    midpointAngle\
      \    midpointRadius\
      \    deleted\
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
      , mappings :: Array
        { id :: MappingId
        , title :: String
        , sourceGraph :: GraphId
        , targetGraph :: GraphId
        , nodeMappingEdges :: Array
          { id :: EdgeId
          , mappingId :: MappingId
          , sourceNode :: NodeId
          , targetNode :: NodeId
          , midpointAngle :: Number
          , midpointRadius :: Number
          , deleted :: Boolean
          }
        , edgeMappingEdges :: Array
          { id :: EdgeId
          , mappingId :: MappingId
          , sourceEdge :: EdgeId
          , targetEdge :: EdgeId
          , midpointAngle :: Number
          , midpointRadius :: Number
          , deleted :: Boolean
          }
        }
      }
    }

parseGraphFetchResponse :: Json -> Either String {graph :: Graph, mappings :: Array Mapping}
parseGraphFetchResponse json =
  let
    rect = { width : 0.0, left : 0.0, right : 0.0, height : 0.0, top : 0.0, bottom : 0.0 }
  in do
    graphFetchResponse :: GraphFetchResponse <- decodeJson json
    graph <- case Array.head graphFetchResponse.data.graphs of
      Nothing -> Left "No graph returned from query"
      Just graphData ->
        let
          nodes = Map.fromFoldable (map (\row -> Tuple row.id row) graphData.nodes)
        in
          pure $ emptyGraph graphData.id
               # _nodes .~ nodes
               # _title <<< _text .~ graphData.title
               # batchInsertEdges graphData.edges
    let
      rawMappings = graphFetchResponse.data.mappings
      mappings = rawMappings <#> \rawMapping ->
        rawMapping { nodeMappingEdges = elementArrayToMap rawMapping.nodeMappingEdges
                   , edgeMappingEdges = elementArrayToMap rawMapping.edgeMappingEdges
                   }
    pure $ { graph : graph
           , mappings : mappings
           }

-- | Ask for the id of the graph with a given title text
graphIdWithTitleQuery :: String -> GraphQLQuery MegagraphSchema
graphIdWithTitleQuery title =
  Query RProxy
  $ NonEmpty.singleton
  $ "graphs(where: {title: {_eq: \"" <> title <> "\"}}) {\
    \  id\
    \  title\
    \}"

type GraphIdWithTitleResponse
  = { data ::
      { graphs :: Array
        { id :: GraphId
        , title :: String
        }
      }
    }

parseGraphIdWithTitleResponse :: Json -> Either String (Maybe (Record GraphRow))
parseGraphIdWithTitleResponse json = do
  graphIdWithTitleResponse :: GraphIdWithTitleResponse <- decodeJson json
  pure $ Array.head graphIdWithTitleResponse.data.graphs

-- | Ask for the nodes that have the given graph as a subgraph
nodesWithSubgraphQuery :: GraphId -> GraphQLQuery MegagraphSchema
nodesWithSubgraphQuery graphId =
  Query RProxy
  $ NonEmpty.singleton
  $ "nodes(where: {subgraph: {_eq: \"" <> UUID.toString graphId <> "\"}}) {\
    \    id\
    \    graphId\
    \    subgraph\
    \    positionX\
    \    positionY\
    \    text\
    \    isValid\
    \    deleted\
    \}"

type NodesWithSubgraphResponse
  = { data ::
      { nodes :: Array (Record NodeRow)
      }
    }

parseNodesWithSubgraphResponse :: Json -> Either String (Array Node)
parseNodesWithSubgraphResponse json = do
  nodesWithSubgraphResponse :: NodesWithSubgraphResponse <- decodeJson json
  pure $ nodesWithSubgraphResponse.data.nodes

elementArrayToMap :: forall r. Array {id :: UUID | r } -> Map UUID {id :: UUID | r}
elementArrayToMap = Map.fromFoldable <<< map \mappingEdge -> Tuple mappingEdge.id mappingEdge
