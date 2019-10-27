module Server.GraphDB.ParseRow where

import Prelude

import AppOperation (AppOperation)
import Control.Monad.Except (runExcept)
import Core (Edge, GraphId, GraphSpacePoint2D(..), Node, freshEdge, freshNode)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe)
import Foreign.Generic (decodeJSON)
import Foreign.Utils (parseUUIDEither, showForeignError)
import Server.GraphDB.Schema (GraphRow, NodeRow, EdgeRow, HistoryRow)


parseGraphRow :: GraphRow -> Either String { graphId :: GraphId, title :: String }
parseGraphRow row = do
  graphId <- parseUUIDEither row.graphid
  pure { graphId : graphId
       , title   : row.title
       }

parseNodeRow :: NodeRow -> Either String Node
parseNodeRow row = do
  nodeId <- parseUUIDEither row.nodeid
  graphId <- parseUUIDEither row.graphid
  subgraph <- parseSubgraph row.subgraphid
  let position = { x : row.positionx, y : row.positiony }
  pure $ (freshNode graphId nodeId)
           { subgraph = subgraph
           , position = GraphSpacePoint2D position
           , text     = row.nodetext
           }
    where
      parseSubgraph subgraphid = case toMaybe subgraphid of
        Nothing -> Right Nothing
        Just subgraphidStr -> Just <$> parseUUIDEither subgraphidStr

parseEdgeRow :: EdgeRow -> Either String Edge
parseEdgeRow row = do
  sourceId      <- parseUUIDEither row.sourcenodeid
  sourceGraphId <- parseUUIDEither row.sourcegraphid
  targetId      <- parseUUIDEither row.targetnodeid
  targetGraphId <- parseUUIDEither row.targetgraphid
  pure $ (freshEdge { source      : sourceId
                    , sourceGraph : sourceGraphId
                    , target      : targetId
                    , targetGraph : targetGraphId
                    })
           { text = row.edgetext }

parseHistoryRow :: HistoryRow -> Either String (AppOperation Unit)
parseHistoryRow row = lmap showForeignError $ runExcept $ decodeJSON row.operation
