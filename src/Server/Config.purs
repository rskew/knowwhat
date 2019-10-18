module Server.Config where

import Prelude
import SQLite3 as SQLite

webSocketPort :: Int
webSocketPort = 8764

webSocketDomain :: String
webSocketDomain = "localhost"

config :: { fileServerPort   :: Int
          , dbFile           :: SQLite.FilePath
          , webSocketPort    :: Int
          , webSocketAddress :: String
          }
config = { fileServerPort   : 8763
         , dbFile           : "../server/testdb.sqlite"
         , webSocketPort    : webSocketPort
         , webSocketAddress : "ws://" <> webSocketDomain <> ":" <> show webSocketPort
         }
