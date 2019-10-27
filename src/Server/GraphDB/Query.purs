module Server.GraphDB.Query where

import Prelude

import AppOperation (AppOperation(..), encodeGraphDataAsAppOperation)
import Control.Monad.Except.Trans (ExceptT(..))
import Core (Edge, EdgeId, GraphId, GraphSpacePoint2D(..), Node, NodeId, batchInsertEdges, emptyPane, freshTitle)
import Data.Array (length, head)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.UUID as UUID
import Effect.Aff (Aff, try, catchError)
import Effect.Class.Console as Console
import Foreign.Generic (encodeJSON)
import Foreign.Utils (showForeignError)
import SQLite3 (DBConnection)
import SQLite3 as SQLite
import Server.Config (config)
import Server.GraphDB.ParseRow (parseEdgeRow, parseGraphRow, parseHistoryRow, parseNodeRow)
import Server.GraphDB.Schema (EdgeRow, GraphRow, HistoryRow, NodeRow, historyTableSchema, undoneTableSchema)
import Simple.JSON (class ReadForeign, read)

------
-- DB Actions (queries with no return value)

insertGraph :: GraphId -> String -> DBConnection -> ExceptT String Aff Unit
insertGraph graphId title db =
  let
    queryStr = "INSERT INTO graphs (  graphid,  title ) \
               \VALUES             ( $graphid, $title );"
    params = { "$graphid" : UUID.toString graphId
             , "$title"   : title
             }
  in do
    ExceptT $ Right <$> (Console.log "inserting graph")
    dbAction queryStr params db
    dbAction (historyTableSchema graphId) {} db
    dbAction (undoneTableSchema graphId) {} db

updateGraphTitle :: GraphId -> String -> DBConnection -> ExceptT String Aff Unit
updateGraphTitle graphId newTitle =
  let

    queryStr = "UPDATE graphs SET title = $title \
               \WHERE graphid = $graphid;"
  in
    dbAction queryStr { "$graphid" : UUID.toString graphId
                      , "$title"   : String.trim newTitle
                      }

insertNode :: Node -> DBConnection -> ExceptT String Aff Unit
insertNode node =
  let
    -- We have to handle subgraphid specially becuase we can't pass NULL as the
    -- value of a named parameter
    subgraphId = subgraphStr node.subgraph
    queryStr = "INSERT INTO nodes (  nodeid,  graphid,      subgraphid,       positionx,  positiony,  nodetext ) \
               \VALUES            ( $nodeid, $graphid, " <> subgraphId <> ", $positionx, $positiony, $nodetext );"

    GraphSpacePoint2D pos = node.position
    params = { "$nodeid"     : UUID.toString node.id
             , "$graphid"    : UUID.toString node.graphId
             , "$positionx"  : pos.x
             , "$positiony"  : pos.y
             , "$nodetext"   : node.text
             }
  in
    dbAction queryStr params

deleteNode :: NodeId -> DBConnection -> ExceptT String Aff Unit
deleteNode nodeId =
  let
    queryStr = "DELETE FROM nodes WHERE nodeid = $nodeid;"
    params = { "$nodeid" : UUID.toString nodeId }
  in
    dbAction queryStr params

moveNode :: NodeId -> GraphSpacePoint2D -> DBConnection -> ExceptT String Aff Unit
moveNode nodeId (GraphSpacePoint2D pos) =
  let
    queryStr = "UPDATE nodes SET positionx = $positionx, positiony = $positiony \
               \WHERE nodeid = $nodeid;"
    params = { "$nodeid"    : UUID.toString nodeId
             , "$positionx" : pos.x
             , "$positiony" : pos.y
             }
  in
    dbAction queryStr params

updateNodeText :: NodeId -> String -> DBConnection -> ExceptT String Aff Unit
updateNodeText nodeId text =
  let
    queryStr = "UPDATE nodes SET nodetext = $nodetext \
               \WHERE nodeid = $nodeid;"
    params = { "$nodeid"   : UUID.toString nodeId
             , "$nodetext" : text
             }
  in
    dbAction queryStr params

updateNodeSubgraph :: NodeId -> Maybe GraphId -> DBConnection -> ExceptT String Aff Unit
updateNodeSubgraph nodeId maybeSubgraphId =
  let
    subgraphId = subgraphStr maybeSubgraphId
    queryStr = "UPDATE nodes SET subgraphid = " <> subgraphId <> " \
               \WHERE nodeid = $nodeid;"
    params = { "$nodeid" : UUID.toString nodeId }
  in
    dbAction queryStr params

insertEdge :: Edge -> DBConnection -> ExceptT String Aff Unit
insertEdge edge =
  let
    queryStr = "INSERT INTO edges (  sourcenodeid,  sourcegraphid,  targetnodeid,  targetgraphid,  edgetext ) \
               \VALUES            ( $sourcenodeid, $sourcegraphid, $targetnodeid, $targetgraphid, $edgetext );"

    params = { "$sourcenodeid"  : UUID.toString edge.id.source
             , "$sourcegraphid" : UUID.toString edge.id.sourceGraph
             , "$targetnodeid"  : UUID.toString edge.id.target
             , "$targetgraphid" : UUID.toString edge.id.targetGraph
             , "$edgetext"      : edge.text
             }
  in
    dbAction queryStr params

deleteEdge :: EdgeId -> DBConnection -> ExceptT String Aff Unit
deleteEdge edgeId =
  let
    queryStr = "DELETE FROM edges WHERE ( sourcenodeid, targetnodeid ) = ( $sourcenodeid, $targetnodeid );"
    params = { "$sourcenodeid" : UUID.toString edgeId.source
             , "$targetnodeid" : UUID.toString edgeId.target
             }
  in
    dbAction queryStr params

updateEdgeText :: EdgeId -> String -> DBConnection -> ExceptT String Aff Unit
updateEdgeText edgeId text =
  let
    queryStr = "UPDATE edges SET edgetext = $edgetext \
               \WHERE ( sourcenodeid, targetnodeid ) = ( $sourcenodeid, $targetnodeid );"
    params = { "$sourcenodeid" : UUID.toString edgeId.source
             , "$targetnodeid" : UUID.toString edgeId.target
             , "$edgetext"     : text
             }
  in
    dbAction queryStr params

consHistory :: AppOperation Unit -> DBConnection -> ExceptT String Aff Unit
consHistory appOp =
  let
    AppOperation graphId op = appOp
    queryStr = "INSERT INTO " <> historyTableName graphId <> " ( operation ) VALUES ( $operation );"
    params   = { "$operation" : encodeJSON appOp }
  in
    dbAction queryStr params

consUndone :: AppOperation Unit -> DBConnection -> ExceptT String Aff Unit
consUndone appOp =
  let
    AppOperation graphId op = appOp
    queryStr = "INSERT INTO "<> undoneTableName graphId <> " ( operation ) VALUES ( $operation );"
    params   = { "$operation" : encodeJSON appOp }
  in
    dbAction queryStr params

replaceLastOpInHistory :: AppOperation Unit -> DBConnection -> ExceptT String Aff Unit
replaceLastOpInHistory appOp =
  let
    AppOperation graphId op = appOp
    queryStr = "UPDATE " <> historyTableName graphId <> " \
               \SET operation = $operation \
               \WHERE historyindex = ( SELECT max(historyindex) FROM " <> historyTableName graphId <> " );"
    params   = { "$operation" : encodeJSON appOp }
  in
    dbAction queryStr params

removeOpFromHistory :: GraphId -> HistoryRow -> DBConnection -> ExceptT String Aff Unit
removeOpFromHistory graphId row =
  let
    queryStr = "DELETE FROM" <> historyTableName graphId <> " \
               \WHERE historyindex = $historyindex;"
    params   = { "$historyindex" : row.historyindex }
  in
    dbAction queryStr params

removeOpFromUndone :: GraphId -> HistoryRow -> DBConnection -> ExceptT String Aff Unit
removeOpFromUndone graphId row =
  let
    queryStr = "DELETE FROM" <> undoneTableName graphId <> " \
               \WHERE historyindex = $historyindex;"
    params   = { "$historyindex" : row.historyindex }
  in
    dbAction queryStr params

dbAction :: forall params. String -> Record params -> DBConnection -> ExceptT String Aff Unit
dbAction queryStr params db = ExceptT do
  result <- lmap show <$> (try $ SQLite.queryObjectDB db queryStr params)
  case result of
    Left e -> do
      Console.log "The following statement was unsuccessful:"
      Console.log queryStr
      Console.log "with the following error:"
      Console.log $ show e
      pure $ Left e
    Right _ -> do
      Console.log "The following statement was executed successfully:"
      Console.log queryStr
      pure $ Right unit


------
-- DB Queries

selectGraph :: GraphId -> DBConnection -> ExceptT String Aff GraphRow
selectGraph graphId db =
    let
      queryStr = "SELECT * FROM graphs WHERE graphid = $graphid;"
      params = { "$graphid" : UUID.toString graphId }
    in do
      graphRows <- dbQuery queryStr params db
      ExceptT case Tuple (length graphRows) (head graphRows) of
        Tuple 1 (Just row) -> pure $ Right row
        _ -> do
          pure $ Left $ "Zero or > 1 rows in graphs table for graph: " <> show graphId

selectNodesInGraph :: GraphId -> DBConnection -> ExceptT String Aff (Array NodeRow)
selectNodesInGraph graphId =
  let
    queryStr = "SELECT * FROM nodes WHERE graphid = $graphid;"
    params = { "$graphid" : UUID.toString graphId }
  in
    dbQuery queryStr params

selectEdgesInGraph :: GraphId -> DBConnection -> ExceptT String Aff (Array EdgeRow)
selectEdgesInGraph graphId =
  let
    queryStr = "SELECT * FROM edges WHERE ( sourcegraphid = $graphid OR targetgraphid = $graphid );"
    params = { "$graphid" : UUID.toString graphId}
  in
    dbQuery queryStr params

selectEdgesBetweenNodes :: NodeId -> NodeId -> DBConnection -> ExceptT String Aff (Array EdgeRow)
selectEdgesBetweenNodes sourceNodeId targetNodeId =
  let
    queryStr = "SELECT * FROM edges WHERE (                                   \
               \sourcenodeid = $sourcenodeid AND targetnodeid = $targetnodeid \
               \);"
    params   = { "$sourcenodeid" : UUID.toString sourceNodeId
               , "$targetnodeid" : UUID.toString targetNodeId
               }
  in
    dbQuery queryStr params

selectEdgesBetweenGraphs :: GraphId -> GraphId -> DBConnection -> ExceptT String Aff (Array EdgeRow)
selectEdgesBetweenGraphs sourceGraphId targetGraphId =
  let
    queryStr = "SELECT * FROM edges WHERE ( \
               \( sourcegraphid = $sourcegraphid AND targetgraphid = $targetgraphid ) \
               \OR \
               \( sourcegraphid = $targetgraphid AND targetgraphid = $sourcegraphid ) \
               \);"
    params   = { "$sourcegraphid" : UUID.toString sourceGraphId
               , "$targetgraphid" : UUID.toString targetGraphId
               }
  in
    dbQuery queryStr params

lastOpFromHistory :: GraphId -> DBConnection -> ExceptT String Aff HistoryRow
lastOpFromHistory graphId db =
  let
    queryStr = "SELECT * \
               \FROM " <> historyTableName graphId <> " \
               \WHERE historyindex = ( SELECT max(historyindex) FROM " <> historyTableName graphId <> " );"
  in do
    rows <- dbQuery queryStr {} db
    case rows of
      [ row ] -> pure row
      [] -> ExceptT $ pure $ Left $ "History empty for graph: " <> show graphId
      _  -> ExceptT $ pure $ Left $ "Error: multiple rows with max index in history." <> UUID.toString graphId

lastOpFromUndone :: GraphId -> DBConnection -> ExceptT String Aff HistoryRow
lastOpFromUndone graphId db =
  let
    queryStr = "SELECT * \
               \FROM " <> undoneTableName graphId <> " \
               \WHERE historyindex = ( SELECT max(historyindex) FROM " <> undoneTableName graphId <> " );"
  in do
    rows <- dbQuery queryStr {} db
    case rows of
      [ row ] -> pure row
      [] -> ExceptT $ pure $ Left $ "Undone empty for graph: " <> show graphId
      _  -> ExceptT $ pure $ Left $ "Error: multiple rows with max index in undone." <> UUID.toString graphId

selectGraphHistory :: GraphId -> DBConnection -> ExceptT String Aff (Array HistoryRow)
selectGraphHistory graphId =
  let
    queryStr = "SELECT * FROM " <> historyTableName graphId <> " \
               \ORDER BY historyindex DESC;"
  in
    dbQuery queryStr {}

selectGraphUndone :: GraphId -> DBConnection -> ExceptT String Aff (Array HistoryRow)
selectGraphUndone graphId =
  let
    queryStr = "SELECT * FROM " <> undoneTableName graphId <> " \
               \ORDER BY historyindex DESC;"
  in
    dbQuery queryStr {}

graphWithTitle :: String -> DBConnection -> ExceptT String Aff (Maybe GraphId)
graphWithTitle title db =
  let
    queryStr = "SELECT * FROM graphs WHERE title = $title;"
    params   = { "$title" : title }
  in do
    rows <- dbQuery queryStr params db
    parsedRows <- ExceptT $ pure $ traverse parseGraphRow rows
    case parsedRows of
      [ { graphId } ] -> pure $ Just graphId
      _ -> pure Nothing

selectNode :: NodeId -> DBConnection -> ExceptT String Aff (Maybe Node)
selectNode nodeId db =
  let
    queryStr = "SELECT * FROM nodes WHERE nodeid = $nodeid;"
    params   = { "$nodeid" : UUID.toString nodeId }
  in do
    rows <- dbQuery queryStr params db
    parsedNodes <- ExceptT $ pure $ traverse parseNodeRow rows
    case parsedNodes of
      [ node ] -> pure $ Just node
      _ -> pure Nothing

lookupNodesBySubgraph :: GraphId -> DBConnection -> ExceptT String Aff (Array NodeRow)
lookupNodesBySubgraph subgraphId =
  let
    queryStr = "SELECT * FROM nodes WHERE subgraphid = $subgraphid;"
    params   = { "$subgraphid" : UUID.toString subgraphId }
  in
    dbQuery queryStr params

lookupRepresentativeInKnowledgeNavigator :: GraphId -> DBConnection -> ExceptT String Aff (Array Node)
lookupRepresentativeInKnowledgeNavigator graphId db =
  let
    queryStr = "SELECT * FROM nodes WHERE ( graphid, subgraphid ) = ( $graphid, $subgraphid );"
    params   = { "$graphid"    : UUID.toString config.knowledgeNavigatorId
               , "$subgraphid" : UUID.toString graphId
               }
  in do
    nodeRows <- dbQuery queryStr params db
    ExceptT $ pure $ traverse parseNodeRow nodeRows

dbQuery :: forall params row. ReadForeign row => String -> Record params -> DBConnection -> ExceptT String Aff (Array row)
dbQuery queryStr params db = do
  results <- ExceptT $ lmap show <$> (try $ SQLite.queryObjectDB db queryStr params)
  ExceptT $ pure $ lmap showForeignError $ read results

historyTableName :: GraphId -> String
historyTableName graphId = "\"history_" <> UUID.toString graphId <> "\""

undoneTableName :: GraphId -> String
undoneTableName graphId = "\"undone_" <> UUID.toString graphId <> "\""

loadGraphAsAppOperation :: GraphId -> DBConnection -> ExceptT String Aff (AppOperation Unit)
loadGraphAsAppOperation graphId db = do -- Aff
  graphRow     <- selectGraph graphId db
  nodeRows     <- selectNodesInGraph graphId db
  edgeRows     <- selectEdgesInGraph graphId db
  historyRows  <- catchError (selectGraphHistory graphId db) (const $ pure [])
  undoneRows   <- catchError (selectGraphUndone  graphId db) (const $ pure [])
  graphIdTitle <- ExceptT $ pure $ parseGraphRow graphRow
  nodes        <- ExceptT $ pure $ traverse parseNodeRow nodeRows
  edges        <- ExceptT $ pure $ traverse parseEdgeRow edgeRows
  history      <- ExceptT $ pure $ traverse parseHistoryRow historyRows
  undone       <- ExceptT $ pure $ traverse parseHistoryRow undoneRows
  let
    nodesMap  = nodes <#> (\node -> Tuple node.id node) # Map.fromFoldable
    panesMap  = Map.singleton graphId $ emptyPane graphId
    titlesMap = Map.singleton graphId $ freshTitle { titleText = graphRow.title }
    graphData = { nodes  : nodesMap
                , edges  : { sourceTarget : Map.empty, targetSource : Map.empty }
                , panes  : panesMap
                , titles : titlesMap
                }
                # flip batchInsertEdges edges
  pure $ encodeGraphDataAsAppOperation graphId graphData history undone

subgraphStr :: Maybe GraphId -> String
subgraphStr = case _ of
  Nothing -> "NULL"
  Just subgraphId' -> "\"" <> UUID.toString subgraphId' <> "\""
