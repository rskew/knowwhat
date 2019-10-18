module Server.Query ( insertGraph
                    , updateGraphTitle
                    , insertNode
                    , deleteNode
                    , moveNode
                    , updateNodeText
                    , updateNodeSubgraph
                    , insertEdge
                    , deleteEdge
                    , updateEdgeText
                    ) where

import Prelude

import Control.Monad.Except (runExcept, withExcept)
import Core (Edge, EdgeId, GraphId, GraphSpacePoint2D(..), Node, NodeId)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.UUID as UUID
import Effect.Aff (Aff, try)
import Effect.Class.Console as Console
import Foreign (ForeignError, readArray, readString, renderForeignError)
import SQLite3 (DBConnection)
import SQLite3 as SQLite

insertGraph :: GraphId -> String -> DBConnection -> Aff (Either String Unit)
insertGraph graphId title =
  let
    queryStr = "INSERT INTO graphs (  graphid,  title ) \
               \VALUES             ( $graphid, $title );"
  in
    dbAction queryStr $ { "$graphid" : UUID.toString graphId
                        , "$title"   : title
                        }

updateGraphTitle :: GraphId -> String -> DBConnection -> Aff (Either String Unit)
updateGraphTitle graphId newTitle =
  let
    queryStr = "UPDATE graphs SET title = $title \
               \WHERE graphid = $graphid;"
  in
    dbAction queryStr { "$graphid" : UUID.toString graphId
                      , "$title"   : newTitle
                      }

insertNode :: Node -> DBConnection -> Aff (Either String Unit)
insertNode node =
  let
    -- We have to handle subgraphid specially becuase we can't pass NULL as the
    -- value of a named parameter
    subgraphid = case node.subgraph of
      Nothing -> "NULL"
      Just subgraphId -> UUID.toString subgraphId
    queryStr = "INSERT INTO nodes (  nodeid,  graphid,      subgraphid,       positionx,  positiony,  nodetext ) \
               \VALUES            ( $nodeid, $graphid, " <> subgraphid <> ", $positionx, $positiony, $nodetext );"

    GraphSpacePoint2D pos = node.position
    params = { "$nodeid"     : UUID.toString node.id
             , "$graphid"    : UUID.toString node.graphId
             , "$positionx"  : pos.x
             , "$positiony"  : pos.y
             , "$nodetext"   : node.text
             }
  in
    dbAction queryStr params

deleteNode :: NodeId -> DBConnection -> Aff (Either String Unit)
deleteNode nodeId =
  let
    queryStr = "DELETE FROM nodes WHERE nodeid = $nodeid;"
    params = { "$nodeid" : UUID.toString nodeId }
  in
    dbAction queryStr params

moveNode :: NodeId -> GraphSpacePoint2D -> DBConnection -> Aff (Either String Unit)
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

updateNodeText :: NodeId -> String -> DBConnection -> Aff (Either String Unit)
updateNodeText nodeId text =
  let
    queryStr = "UPDATE nodes SET nodetext = $nodetext \
               \WHERE nodeid = $nodeid;"
    params = { "$nodeid"   : UUID.toString nodeId
             , "$nodetext" : text
             }
  in
    dbAction queryStr params

updateNodeSubgraph :: NodeId -> Maybe GraphId -> DBConnection -> Aff (Either String Unit)
updateNodeSubgraph nodeId maybeSubgraphId =
  let
    subgraphid = case maybeSubgraphId of
      Nothing -> "NULL"
      Just subgraphId -> UUID.toString subgraphId
    queryStr = "UPDATE nodes SET subgraphid = " <> subgraphid <> " \
               \WHERE nodeid = $nodeid;"
    params = { "$nodeid"   : UUID.toString nodeId }
  in
    dbAction queryStr params

insertEdge :: Edge -> DBConnection -> Aff (Either String Unit)
insertEdge edge =
  let
    queryStr = "INSERT INTO edges (  sourcenodeid,  targetnodeid,  edgetext ) \
               \VALUES            ( $sourcenodeid, $targetnodeid, $edgetext );"

    params = { "$sourcenodeid" : UUID.toString edge.id.source
             , "$targetnodeid" : UUID.toString edge.id.target
             , "$edgetext"     : edge.text
             }
  in
    dbAction queryStr params

deleteEdge :: EdgeId -> DBConnection -> Aff (Either String Unit)
deleteEdge edgeId =
  let
    queryStr = "DELETE FROM edges WHERE ( sourcenodeid, targetnodeid ) = ( $sourcenodeid, $targetnodeid );"
    params = { "$sourcenodeid" : UUID.toString edgeId.source
             , "$targetnodeid" : UUID.toString edgeId.target
             }
  in
    dbAction queryStr params

updateEdgeText :: EdgeId -> String -> DBConnection -> Aff (Either String Unit)
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

dbAction :: forall params. String -> Record params -> DBConnection -> Aff (Either String Unit)
dbAction queryStr params db = do
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

dbQuery :: forall params. String -> Record params -> DBConnection -> Aff (Either String (Array String))
dbQuery queryStr params db = do
  resultEither <- lmap show <$> (try $ SQLite.queryObjectDB db queryStr params)
  pure do
    result <- resultEither
    valArrForeign <- runExcept $ withExcept showForeignError $ readArray result
    valArr <- traverse (readString >>> withExcept showForeignError >>> runExcept) valArrForeign
    pure valArr

showForeignError :: NonEmptyList ForeignError -> String
showForeignError = show <<< map renderForeignError
