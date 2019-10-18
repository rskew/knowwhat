module Server.Config where

import SQLite3 as SQLite

config :: { port   :: Int
          , dbFile :: SQLite.FilePath
          }
config = { port   : 8080
         , dbFile : "../server/testdb.sqlite"
         }
