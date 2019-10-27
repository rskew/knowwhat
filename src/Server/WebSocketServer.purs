module Server.WebSocketServer where

import Prelude

import AppOperation (AppOperation)
import Control.Monad.Except (runExcept, except)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Nullable (toNullable)
import Effect (Effect)
import Effect.Aff (launchAff, try)
import Effect.Class.Console as Console
import Foreign (renderForeignError)
import Foreign as Foreign
import Foreign.Generic (decodeJSON)
import Node.HTTP (listen)
import Node.HTTP as HTTP
import Node.Websocket (ConnectionClose, ConnectionMessage, EventProxy(EventProxy), Request, on)
import Node.Websocket.Connection (remoteAddress)
import Node.Websocket.Request (accept, origin)
import Node.Websocket.Server (newWebsocketServer)
import Node.Websocket.Types (WSConnection, TextFrame(..), defaultServerConfig)
import SQLite3 (DBConnection)
import Server.Config (config)
import Server.GraphDB.Interpreter (filteredHistoryUpdate, interpretAppOperation, serveGraph)


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

    -- Serve knowledge neighborhood as startup graph
    _ <- launchAff $ runExceptT do
      result <- try $ serveGraph config.knowledgeNavigatorId db conn
      case result of
        Left error -> do
          ExceptT $ Right <$> do
            Console.log "Error serving knowledge neighborhood:"
            Console.log error
          ExceptT $ pure $ Left error
        Right _ -> pure unit

    on message conn \ msg -> do
      _ <- launchAff do
        stuff <- runExceptT do
          case msg of
            Left (TextFrame {utf8Data}) -> do -- ExceptT String Aff
              ExceptT $ Right <$> (Console.log $ "Received message: " <> utf8Data)
              operation <- except $ decodeMessage db conn utf8Data
              ExceptT $ Right <$> (Console.log $ "received operation: " <> show operation)
              interpretAppOperation db conn operation
              filteredHistoryUpdate operation db
            Right _ -> pure unit
        case stuff of
          Left error -> do
            Console.log "error doing a thingo:"
            Console.log error
          Right _ -> pure unit
      pure unit

    on close conn \ _ _ -> do
      Console.log ("Peer disconnected " <> remoteAddress conn)
  where
    close   = EventProxy :: EventProxy ConnectionClose
    message = EventProxy :: EventProxy ConnectionMessage
    request = EventProxy :: EventProxy Request

decodeMessage :: DBConnection -> WSConnection -> String -> Either String (AppOperation Unit)
decodeMessage db conn message =
  lmap (show <<< map renderForeignError) $ runExcept (decodeJSON message :: Foreign.F (AppOperation Unit))
