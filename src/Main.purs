module Main where

import Prelude

import AppState (Message(..), Query(..))
import Control.Coroutine as CR
import Control.Coroutine.Aff (emit)
import Control.Coroutine.Aff as CRA
import Control.Monad.Except (runExcept)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..), either)
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.UUID as UUID
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Foreign (F, Foreign, unsafeToForeign, readString)
import Foreign.Generic (encodeJSON)
import GraphComponent (graphComponent)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import HasuraQuery (GraphQLWebsocketResponse)
import Web.Event.EventTarget as EET
import Web.HTML (window)
import Web.HTML.Window (innerWidth, innerHeight)
import Web.Socket.Event.EventTypes as WSET
import Web.Socket.Event.MessageEvent as ME
import Web.Socket.WebSocket as WS

---- Websocket coroutine stuff from
---- https://github.com/slamdata/purescript-halogen/blob/master/examples/driver-websockets/src/Main.purs

-- A producer coroutine that emits messages that arrive from the websocket.
wsProducer :: WS.WebSocket -> CR.Producer String Aff Unit
wsProducer socket = CRA.produce \emitter -> do
  listener <- EET.eventListener \ev -> do
    for_ (ME.fromEvent ev) \msgEvent ->
      for_ (readHelper readString (ME.data_ msgEvent)) \msg ->
        emit emitter msg
  EET.addEventListener
    WSET.onMessage
    listener
    false
    (WS.toEventTarget socket)
  where
    readHelper :: forall a b. (Foreign -> F a) -> b -> Maybe a
    readHelper read =
      either (const Nothing) Just <<< runExcept <<< read <<< unsafeToForeign

-- A consumer coroutine that takes the `query` function from our component IO
-- record and sends `ReceiveMessage` queries in when it receives inputs from the
-- producer.
-- GraphQL over websocket protocol described in
-- https://github.com/apollographql/subscriptions-transport-ws/blob/master/PROTOCOL.md
wsConsumer :: (forall a. Query a -> Aff (Maybe a)) -> CR.Consumer String Aff Unit
wsConsumer query = CR.consumer \msg -> do
  case jsonParser msg
       >>= decodeJson of
    Right (response :: GraphQLWebsocketResponse) ->
      void $ query $ H.tell $ ReceiveOperation response.id response.payload
    Left err ->
      case (jsonParser msg >>= decodeJson) :: Either String {type :: String, id :: String} of
        Right response ->
          case response.type of
            "complete" ->
              Console.log $ "message with id: \"" <> response.id <> "\" complete"
            _ -> Console.log $ show response
        Left err' ->
          case (jsonParser msg >>= decodeJson) :: Either String {type :: String} of
            Right response ->
              case response.type of
                "connection_ack" ->
                  Console.log "received connection ack"
                "ka" ->
                  Console.log "received keep-alive"
                _ -> Console.log $ show response
            Left err'' -> do
              Console.log err''
              Console.log msg
  pure Nothing

-- A consumer coroutine that takes output messages from our component IO
-- and sends them using the websocket
wsSender :: WS.WebSocket -> CR.Consumer Message Aff Unit
wsSender socket = CR.consumer \msg -> do
  case msg of
    SendOperation message -> do
      -- Initialise the connection as per the graphql-over-websocket protocol
      -- https://github.com/apollographql/subscriptions-transport-ws/blob/master/PROTOCOL.md
      -- TODO do this in the connection init, not in app logic
      liftEffect $ WS.sendString socket $ encodeJSON {type: "connection_init"}

      liftEffect $ WS.sendString socket message
  pure Nothing

main :: Effect Unit
main = do
  -- TODO
  -- connection <- WS.create config.webSocketAddress []
  -- connection <- WS.create "ws://localhost:8111" []
  connection <- WS.create "ws://localhost:8080/v1/graphql" []
  HA.runHalogenAff do
    -- Give the connection a moment to connect
    body <- HA.awaitBody
    w <- H.liftEffect window
    windowWidth <- H.liftEffect $ innerWidth w
    windowHeight <- H.liftEffect $ innerHeight w
    ui <- runUI
          graphComponent
          { width  : toNumber windowWidth
          , height : toNumber windowHeight
          }
          body

    -- The wsSender consumer subscribes to all output messages
    -- from our component
    ui.subscribe $ wsSender connection

    -- Once the websocket connection is open, load the home graph
    listener <- H.liftEffect $ EET.eventListener \ev -> do
      ---- TODO use knowledge-neighborhood graph id from config
      case UUID.parseUUID "7b2bf7f2-151c-412b-ad23-4ae1a3b22688" of
        Nothing -> pure unit
        Just graphId -> launchAff_ $ ui.query $ H.tell $ QLoadGraph graphId
    H.liftEffect $ EET.addEventListener WSET.onOpen listener false (WS.toEventTarget connection)

    -- Connecting the consumer to the producer initializes both,
    -- feeding queries back to our component as messages are received.
    H.liftEffect $ launchAff_ $ CR.runProcess (wsProducer connection CR.$$ wsConsumer ui.query)
