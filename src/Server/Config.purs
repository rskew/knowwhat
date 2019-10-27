module Server.Config where

import Core (GraphId)
import Prelude
import SQLite3 as SQLite
import Unsafe.Coerce (unsafeCoerce)

webSocketPort :: Int
webSocketPort = 8112

webSocketDomain :: String
webSocketDomain = "localhost"

knowledgeNavigatorId :: GraphId
knowledgeNavigatorId = unsafeCoerce "72a4ef5e-ade2-479f-ba8b-6c89e7ff1b63"

config :: { fileServerPort       :: Int
          , dbFile               :: SQLite.FilePath
          , webSocketPort        :: Int
          , webSocketAddress     :: String
          , knowledgeNavigatorId :: GraphId
          }
config = { fileServerPort       : 8111
         , dbFile               : "../server/testdb.sqlite"
         , webSocketPort        : webSocketPort
         , webSocketAddress     : "ws://" <> webSocketDomain <> ":" <> show webSocketPort
         , knowledgeNavigatorId : knowledgeNavigatorId
         }
