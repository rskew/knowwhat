module Server.GraphDB.Schema where

import Core (GraphId)
import Data.Nullable (Nullable)
import Data.UUID as UUID
import Prelude

type GraphRow
  = { graphid :: String
    , title   :: String
    }

graphsTableSchema :: String
graphsTableSchema = """
CREATE TABLE IF NOT EXISTS graphs (
  graphid       TEXT PRIMARY KEY NOT NULL, -- UUID
  title         TEXT NOT NULL UNIQUE
);
"""

type NodeRow
  = { nodeid     :: String
    , graphid    :: String
    , subgraphid :: Nullable String
    , positionx  :: Number
    , positiony  :: Number
    , nodetext   :: String
    }

nodesTableSchema :: String
nodesTableSchema = """
CREATE TABLE IF NOT EXISTS nodes (
  nodeid        TEXT PRIMARY KEY NOT NULL, -- UUID
  graphid       TEXT NOT NULL,             -- UUID
  subgraphid    TEXT,                      -- Maybe UUID
  positionx     REAL NOT NULL,
  positiony     REAL NOT NULL,
  nodetext      TEXT NOT NULL,
  FOREIGN KEY(graphid) REFERENCES graphs(graphid),
  FOREIGN KEY(subgraphid) REFERENCES graphs(graphid)
);
"""

type EdgeRow
  = { sourcenodeid  :: String
    , sourcegraphid :: String
    , targetnodeid  :: String
    , targetgraphid :: String
    , edgetext      :: String
    }

edgesTableSchema :: String
edgesTableSchema = """
CREATE TABLE IF NOT EXISTS edges (
  id            INTEGER PRIMARY KEY NOT NULL, -- ID
  sourcenodeid  TEXT NOT NULL, -- UUID
  sourcegraphid TEXT NOT NULL, -- UUID
  targetnodeid  TEXT NOT NULL, -- UUID
  targetgraphid TEXT NOT NULL, -- UUID
  edgetext      TEXT NOT NULL,
  FOREIGN KEY(sourcenodeid) REFERENCES nodes(nodeid),
  FOREIGN KEY(sourcegraphid) REFERENCES graphs(graphid),
  FOREIGN KEY(targetnodeid) REFERENCES nodes(nodeid)
  FOREIGN KEY(targetgraphid) REFERENCES graphs(graphid),
);
"""

type HistoryRow
  = { historyindex :: Int
    , operation    :: String
    }

historyTableSchema :: GraphId -> String
historyTableSchema graphId = "\
\CREATE TABLE IF NOT EXISTS \"history_" <> UUID.toString graphId <> "\"" <> """ (
  historyindex INTEGER PRIMARY KEY,
  operation    TEXT NOT NULL -- AppOperation Unit
);
"""

undoneTableSchema :: GraphId -> String
undoneTableSchema graphId = "\
\CREATE TABLE IF NOT EXISTS \"undone_" <> UUID.toString graphId <> "\"" <> """ (
  historyindex INTEGER PRIMARY KEY,
  operation    TEXT NOT NULL -- AppOperation Unit
);
"""

type mappingEdgesRow
  = { sourceedgeid  :: String
    , sourcegraphid :: String
    , targetedgeid  :: String
    , targetgraphid :: String
    , edgetext      :: String
    }

mappingEdgesTableSchema :: String

edgesTableSchema :: String
edgesTableSchema = """
CREATE TABLE IF NOT EXISTS mappingedges (
  sourceedgeid  INTEGER NOT NULL, -- edge ID
  sourcegraphid TEXT NOT NULL, -- UUID
  targetedgeid  INTEGER NOT NULL, -- edge ID
  targetgraphid TEXT NOT NULL, -- UUID
  edgetext      TEXT NOT NULL,
  FOREIGN KEY(sourceedgeid) REFERENCES edges(id),
  FOREIGN KEY(sourcegraphid) REFERENCES graphs(graphid),
  FOREIGN KEY(targetedgeid) REFERENCES edges(id)
  FOREIGN KEY(targetgraphid) REFERENCES graphs(graphid),
);
"""
