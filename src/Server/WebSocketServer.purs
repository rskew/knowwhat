module Server.WebSocketServer where

import Prelude

import AppOperation (AppOperation)
import Control.Monad.Except (runExcept)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Nullable (toNullable)
import Effect (Effect)
import Effect.Aff (Aff, launchAff, try)
import Effect.Class.Console as Console
import Foreign (renderForeignError)
import Foreign as Foreign
import Foreign.Generic (decodeJSON)
import Node.HTTP (listen)
import Node.HTTP as HTTP
import Node.Websocket (ConnectionClose, ConnectionMessage, EventProxy(EventProxy), Request, on)
import Node.Websocket.Connection (remoteAddress)--, sendMessage)
import Node.Websocket.Request (accept, origin)
import Node.Websocket.Server (newWebsocketServer)
import Node.Websocket.Types (TextFrame(..), defaultServerConfig)
import Server.Config (config)
import Server.GraphDB.Interpreter (interpretAppOperation)
import SQLite3 (DBConnection)


-- Echo websocket server
-- From https://github.com/karshan/purescript-node-websocket/blob/master/test/Main.purs
startWebSocketServer :: DBConnection -> Effect Unit
startWebSocketServer db = do
  httpServer <- HTTP.createServer \ _ _ -> Console.log "Server created"
  listen
    httpServer
    {hostname: "localhost", port: config.webSocketPort, backlog: Nothing} do
      Console.log $ "WebSocket server now listening on port " <> show config.webSocketPort

  wsServer <- newWebsocketServer (defaultServerConfig httpServer)

  on request wsServer \ req -> do
    Console.log do
      "New connection from: " <> show (origin req)
    conn <- accept req (toNullable Nothing) (origin req)
    Console.log "New connection accepted"

    on message conn \ msg -> do
      case msg of
        Left (TextFrame {utf8Data}) -> do
          Console.log ("Received message: " <> utf8Data)
          _ <- launchAff $ handleReceivedOperation utf8Data db
          pure unit
        Right _ -> pure unit

    on close conn \ _ _ -> do
      Console.log ("Peer disconnected " <> remoteAddress conn)
  where
    close   = EventProxy :: EventProxy ConnectionClose
    message = EventProxy :: EventProxy ConnectionMessage
    request = EventProxy :: EventProxy Request

handleReceivedOperation :: String -> DBConnection -> Aff Unit
handleReceivedOperation encodedOp db =
  case
    lmap (show <<< map renderForeignError)
    $ runExcept $ (decodeJSON encodedOp :: Foreign.F (AppOperation Unit))
  of
    Left errors -> do
      Console.log $ "received operation but could not decode: " <> errors
    Right operation -> do
      Console.log $ "received operation: " <> show operation
      result <- try $ interpretAppOperation db operation
      case result of
        Left error -> do
          Console.log "Failed to interpret operation"
        Right _ ->
          Console.log ":D"
