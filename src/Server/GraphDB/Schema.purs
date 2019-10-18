module Server.GraphDB.Schema where

graphsTableSchema :: String
graphsTableSchema = """
CREATE TABLE IF NOT EXISTS graphs (
  graphid       TEXT PRIMARY KEY NOT NULL, -- UUID
  title         TEXT NOT NULL UNIQUE
)
"""

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
)
"""

edgesTableSchema :: String
edgesTableSchema = """
CREATE TABLE IF NOT EXISTS edges (
  sourcenodeid  TEXT NOT NULL, -- UUID
  targetnodeid  TEXT NOT NULL, -- UUID
  edgetext      TEXT NOT NULL,
  FOREIGN KEY(sourcenodeid) REFERENCES nodes(nodeid),
  FOREIGN KEY(targetnodeid) REFERENCES nodes(nodeid)
)
"""
