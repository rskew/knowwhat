module Server.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Server.Config (config)
import Server.FileServer (startFileServer)
import Server.WebSocketServer (startWebSocketServer)
import Server.GraphDB (startDB)


main :: Effect Unit
main = unit <$ launchAff do
  db <- startDB config.dbFile
  _ <- liftEffect startFileServer
  liftEffect $ startWebSocketServer db
