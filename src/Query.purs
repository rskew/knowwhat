module Query where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (head, toArray)
import Data.Lens ((.~))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.NonEmpty as NonEmpty
import Data.Set as Set
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.UUID (UUID)
import Data.UUID as UUID
import Foreign (F, Foreign, ForeignError(..), fail)
import Foreign.Generic (decode)
import HasuraQuery (class RowListEncodeJSON, GraphQLMutation, GraphQLQuery(..), upsertOperation)
import Megagraph (Edge, EdgeId, EdgeMappingEdge, EdgeMappingEdgeRow, EdgeRow, Graph, GraphId, Mapping, MappingId, Node, NodeId, NodeMappingEdge, NodeMappingEdgeRow, NodeRow, PathEquation, PathEquationId, _nodes, _text, _title, batchInsertEdges, edgeSetToPathEquation, emptyGraph)
import Record as Record
import Record.Extra (class Keys)
import Type.Prelude (class RowToList, RProxy(..), SProxy(..))

type GraphRow
  = ( id :: GraphId
    , title :: String
    )

-- | Since we can't create tables on the fly easily with Hasura,
-- | store all edges in all equations in one ginormous table
-- | with each row corresponding to a single edge in a single
-- | equation.
-- | The two paths can be reconstructed from the set of edges,
-- | so it's fine to just associate the edges with the equation ID
-- | and not the path they're from.
type PathEquationRow
  = ( pathEquationId :: PathEquationId
    , graphId :: GraphId
    , edgeId :: EdgeId
    , deleted :: Boolean
    , id :: String
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
    , pathEquations :: Record PathEquationRow
    , mappings :: Record MappingRow
    , nodeMappingEdges :: NodeMappingEdge
    , edgeMappingEdges :: EdgeMappingEdge
    )

renderGraphUpsertQuery :: Array (Record GraphRow) -> GraphQLMutation MegagraphSchema
renderGraphUpsertQuery graphRows =
  upsertOperation (SProxy :: SProxy "graphs") graphRows

type GraphUpsertResponse
  = { insert_graphs ::
      { affected_rows :: Int
      }
    }

parseGraphUpsertResponse :: Foreign -> F Int
parseGraphUpsertResponse response = do
  graphUpsertResponse :: GraphUpsertResponse <- decode response
  pure $ graphUpsertResponse.insert_graphs.affected_rows

renderNodeUpsertQuery ::
  forall nodeRowL.
  RowToList NodeRow nodeRowL
  => Keys nodeRowL
  => RowListEncodeJSON NodeRow nodeRowL
  => Array Node
  -> GraphQLMutation MegagraphSchema
renderNodeUpsertQuery nodes = upsertOperation (SProxy :: SProxy "nodes") nodes

type NodeUpsertResponse
  = { insert_nodes ::
      { affected_rows :: Int
      }
    }

parseNodeUpsertResponse :: Foreign -> F Int
parseNodeUpsertResponse response = do
  nodeUpsertResponse :: NodeUpsertResponse <- decode response
  pure $ nodeUpsertResponse.insert_nodes.affected_rows

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

renderPathEquationUpsertQuery ::
  forall pathEquationRowL.
  RowToList PathEquationRow pathEquationRowL
  => Keys pathEquationRowL
  => RowListEncodeJSON PathEquationRow pathEquationRowL
  => PathEquation
  -> GraphQLMutation MegagraphSchema
renderPathEquationUpsertQuery pathEquation =
  upsertOperation (SProxy :: SProxy "pathEquations") $ pathEquationToRows pathEquation

pathEquationToRows :: PathEquation -> Array (Record PathEquationRow)
pathEquationToRows pathEquation =
  (pathEquation.pathA <> pathEquation.pathB)
  <#> \edgeId -> { graphId: pathEquation.graphId
                 , pathEquationId: pathEquation.id
                 , edgeId: edgeId
                 , deleted: pathEquation.deleted
                 , id: UUID.toString pathEquation.id <> "_" <> UUID.toString edgeId
                 }


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
      \  pathEquations {\
      \    pathEquationId\
      \    graphId\
      \    edgeId\
      \    deleted\
      \    id\
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
  = { graphs :: Array
      { id :: GraphId
      , title :: String
      , nodes :: Array (Record NodeRow)
      , edges :: Array (Record EdgeRow)
      , pathEquations :: Array (Record PathEquationRow)
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

parseGraphFetchResponse :: Foreign -> F {graph :: Graph, mappings :: Array Mapping}
parseGraphFetchResponse response =
  let
    rect = { width : 0.0, left : 0.0, right : 0.0, height : 0.0, top : 0.0, bottom : 0.0 }
  in do
    graphFetchResponse :: GraphFetchResponse <- decode response
    graph <- case Array.head graphFetchResponse.graphs of
      Nothing -> fail $ ForeignError "No graph returned from query"
      Just graphData ->
        let
          nodes = Map.fromFoldable (map (\row -> Tuple row.id row) graphData.nodes)
          graph' = emptyGraph graphData.id
                   # _nodes .~ nodes
                   # _title <<< _text .~ graphData.title
                   # batchInsertEdges graphData.edges
          pathEquationEdgeIdSets = graphData.pathEquations
                                   # Array.sortBy (comparing _.pathEquationId)
                                   # Array.groupBy (\a b -> a.pathEquationId == b.pathEquationId)
          pathEquations = pathEquationEdgeIdSets
                          <#> (\pathEquationRows ->
                                  let
                                    pathEquationId = head pathEquationRows # _.pathEquationId
                                    pathEquationEdgeIds = (toArray pathEquationRows) <#> _.edgeId
                                    graphIdEdgeIdSet = Set.fromFoldable $ ((Tuple graph'.id) <$> pathEquationEdgeIds)
                                  in
                                    edgeSetToPathEquation graph' pathEquationId graphIdEdgeIdSet
                                    <#> _{deleted = head pathEquationRows # _.deleted}
                                    <#> \pathEquation -> Tuple pathEquationId pathEquation)
                          # Array.catMaybes
                          # Map.fromFoldable
        in
          pure $ graph' {pathEquations = pathEquations}
    let
      rawMappings = graphFetchResponse.mappings
      mappings = rawMappings <#> \rawMapping ->
        rawMapping { nodeMappingEdges = elementArrayToMap rawMapping.nodeMappingEdges
                   , edgeMappingEdges = elementArrayToMap rawMapping.edgeMappingEdges
                   }
                   # Record.insert (SProxy :: SProxy "isValid") true
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
  = { graphs :: Array
      { id :: GraphId
      , title :: String
      }
    }

parseGraphIdWithTitleResponse :: Foreign -> F (Maybe (Record GraphRow))
parseGraphIdWithTitleResponse response = do
  graphIdWithTitleResponse :: GraphIdWithTitleResponse <- decode response
  pure $ Array.head graphIdWithTitleResponse.graphs

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
  = { nodes :: Array (Record NodeRow)
    }

parseNodesWithSubgraphResponse :: Foreign -> F (Array Node)
parseNodesWithSubgraphResponse response = do
  nodesWithSubgraphResponse :: NodesWithSubgraphResponse <- decode response
  pure $ nodesWithSubgraphResponse.nodes

elementArrayToMap :: forall r. Array {id :: UUID | r } -> Map UUID {id :: UUID | r}
elementArrayToMap = Map.fromFoldable <<< map \mappingEdge -> Tuple mappingEdge.id mappingEdge
