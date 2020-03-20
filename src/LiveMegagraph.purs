module LiveMegagraph where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', (.~), (%~), (?~))
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (trim, joinWith)
import Data.Symbol (SProxy(..))
import Data.Traversable (for_)
import Data.UUID (UUID)
import Data.UUID as UUID
import Effect.Aff (Aff)
import Effect.Console as Console
import Foreign (readString)
import Foreign.Class (class Encode, class Decode)
import Foreign.Generic (genericEncode, genericDecode, defaultOptions, encodeJSON)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import HasuraQuery (GraphQLQuery, GraphQLWebsocketResponse, renderMutation, renderQuery)
import Interpreter (interpretMegagraphStateUpdate, megagraphUpdateToQuery)
import Megagraph (GraphId, Megagraph, Node, NodeId, _graph, _graphs, _isValid, _mappings, _node, _title, emptyMegagraph)
import MegagraphStateUpdate (MegagraphComponent(..), MegagraphStateUpdate(..), encodeGraphAsMegagraphStateUpdates, encodeMappingAsMegagraphStateUpdates, invertMegagraphStateUpdates)
import Query (MegagraphSchema, graphFetchQuery, graphIdWithTitleQuery, nodesWithSubgraphQuery, parseGraphFetchResponse, parseGraphIdWithTitleResponse, parseGraphUpsertResponse, parseMegagraphUpsertResponse, parseNodeUpsertResponse, parseNodesWithSubgraphResponse)
import Web.Event.Event as EE
import Web.Event.EventTarget as ET
import Web.Socket.Event.EventTypes as WSET
import Web.Socket.Event.MessageEvent as ME
import Web.Socket.ReadyState (ReadyState(..))
import Web.Socket.WebSocket as WS

  -- | State updates that require specific interactions with the backend.
data MegagraphMutation
  = UpdateNodeText Node String
  | UpdateTitleText GraphId String String
  | LinkNodeSubgraph Node
  | StateUpdate (Array MegagraphStateUpdate)

derive instance genericMegagraphMutation :: Generic MegagraphMutation _
instance decodeMegagraphMutation :: Decode MegagraphMutation where
  decode = genericDecode defaultOptions
instance encodeMegagraphMutation :: Encode MegagraphMutation where
  encode = genericEncode defaultOptions

instance showMegagraphMutation :: Show MegagraphMutation where
  show = case _ of
    UpdateNodeText node text ->
      "UpdateNodeText for node: " <> show node <> " with text: " <> text
    UpdateTitleText graphId from to ->
      "UpdateTitleText for graph: " <> show graphId <> " from: " <> from <> " to: " <> to
    LinkNodeSubgraph node ->
      "LinkNodeSubgraph for node: " <> show node
    StateUpdate op ->
      "StateUpdate op: " <> show op

invertMegagraphMutation :: MegagraphMutation -> MegagraphMutation
invertMegagraphMutation = case _ of
  UpdateNodeText node text -> UpdateNodeText (node {text = text}) node.text
  UpdateTitleText graphId from to -> UpdateTitleText graphId to from
  LinkNodeSubgraph node ->  LinkNodeSubgraph $ node {subgraph = Nothing}
  StateUpdate op -> StateUpdate $ invertMegagraphStateUpdates op

type CallbackId = UUID

type State pa
  = { megagraph :: Megagraph
    , callbacks :: Map CallbackId (Json -> Action pa)
    , webSocketConnection :: WS.WebSocket
    , messageQueue :: Array String
    }

emptyState :: forall pa. WS.WebSocket -> State pa
emptyState connection
  = { megagraph: emptyMegagraph
    , callbacks: Map.empty
    , webSocketConnection: connection
    , messageQueue: []
    }

_megagraph :: forall pa. Lens' (State pa) Megagraph
_megagraph = prop (SProxy :: SProxy "megagraph")

_callbacks :: forall pa. Lens' (State pa) (Map CallbackId (Json -> Action pa))
_callbacks = prop (SProxy :: SProxy "callbacks")

_messageQueue :: forall pa. Lens' (State pa) (Array String)
_messageQueue = prop (SProxy :: SProxy "messageQueue")

data Action pa
  = Init
  | WebSocketConnectionInit
  | DoMany (Array (Action pa))
  | InterpretStateUpdates (Array MegagraphStateUpdate)
  | SendMutation CallbackId (Json -> Action pa) (Array MegagraphStateUpdate)
  | SendQuery CallbackId (Json -> Action pa) (GraphQLQuery MegagraphSchema)
  | SendMessage String
  | RegisterCallback CallbackId (Json -> Action pa)
  | CallCallback CallbackId Json
  | RaiseParentAction pa
  | DecodeWebSocketEvent EE.Event
  | MessageParseError String Json
  | UpdateNodeValidity GraphId NodeId Boolean
  | UpdateTitleValidity GraphId Boolean

data Query pa a
  = Mutate MegagraphMutation {onFail :: String -> pa, onSuccess :: pa} a
  | LoadGraph GraphId a
  | Drop MegagraphComponent a

data Message pa
  = ReturnAction pa
  | MegagraphUpdated Megagraph

type Slot pa = H.Slot (Query pa) (Message pa)

type Input = WS.WebSocket

type Slots = ()

-- | The parent component's action type returned from callbacks
liveMegagraph :: forall pa. H.Component HH.HTML (Query pa) Input (Message pa) Aff
liveMegagraph =
  H.mkComponent
    { initialState : \connection -> emptyState connection
    , render : render
    , eval: H.mkEval (H.defaultEval { handleAction = handleAction
                                    , handleQuery  = handleQuery
                                    , initialize   = Just Init
                                    })
    }

editorRef :: H.RefLabel
editorRef = H.RefLabel "editor"

render :: forall pa. State pa -> H.ComponentHTML (Action pa) Slots Aff
render state =
  let
    classes = joinWith " " $ Array.catMaybes
              [ Just "pendingIndicator"
              , if not Map.isEmpty state.callbacks
                then Just "pending"
                else Nothing
              ]
  in
    HH.div
    [ HP.class_ $ HH.ClassName classes
    ]
    []

handleAction :: forall pa. Action pa -> H.HalogenM (State pa) (Action pa) Slots (Message pa) Aff Unit
handleAction = case _ of
  Init -> do
    state <- H.get
    -- Run connection initialisation when socket is open
    subscribeWithAction WSET.onOpen (WS.toEventTarget state.webSocketConnection) (const WebSocketConnectionInit)
    -- Subscribe to messages from the websocket
    subscribeWithAction WSET.onMessage (WS.toEventTarget state.webSocketConnection) DecodeWebSocketEvent

  WebSocketConnectionInit -> do
    state <- H.get
    -- Initialise the connection as per the graphql-over-websocket protocol
    -- https://github.com/apollographql/subscriptions-transport-ws/blob/master/PROTOCOL.md
    H.liftEffect $ WS.sendString state.webSocketConnection $ encodeJSON {type: "connection_init"}
    -- Send any messages that have queued up
    for_ (Array.reverse state.messageQueue) \msg ->
      H.liftEffect $ WS.sendString state.webSocketConnection msg
    H.modify_ _{ messageQueue = [] }

  DoMany actions -> for_ actions handleAction

  InterpretStateUpdates megagraphStateUpdates -> do
    for_ megagraphStateUpdates \op ->
      H.modify_ $ _megagraph %~ interpretMegagraphStateUpdate op
    -- tell parent
    state <- H.get
    H.raise $ MegagraphUpdated state.megagraph

  SendMutation callbackId callback megagraphUpdate -> do
    handleAction $ RegisterCallback callbackId callback
    handleAction $ SendMessage $ renderMutation callbackId $ megagraphUpdateToQuery megagraphUpdate

  SendQuery callbackId callback query -> do
    handleAction $ RegisterCallback callbackId callback
    handleAction $ SendMessage $ renderQuery callbackId query

  SendMessage message -> do
    state <- H.get
    webSocketState <- H.liftEffect $ WS.readyState state.webSocketConnection
    case webSocketState of
      Open -> do
        H.liftEffect $ WS.sendString state.webSocketConnection message
      -- If connection isn't open yet, enqueue message
      _ -> H.modify_ $ _messageQueue %~ Array.cons message

  RegisterCallback callbackId callback ->
    H.modify_ $ _callbacks <<< at callbackId ?~ callback

  CallCallback callbackId msgJson -> do
    state <- H.get
    case Map.lookup callbackId state.callbacks of
      Nothing -> H.liftEffect $ Console.log
                 $ "Mesage reveived by no callback registered, msg: " <> stringify msgJson
                 <> " for callbackId: " <> show callbackId
      Just callback -> handleAction $ callback msgJson
    H.modify_ $ _callbacks <<< at callbackId .~ Nothing

  RaiseParentAction parentAction -> H.raise $ ReturnAction parentAction

  DecodeWebSocketEvent event ->
    case do
      ME.fromEvent event
      >>= ME.data_ >>> readString >>> runExcept >>> either (const Nothing) Just
    of
      Nothing -> pure unit
      Just msg ->
        -- TODO argonaut type with optional fields for less nested casing
        case jsonParser msg
             >>= decodeJson
        of
          Right (response :: GraphQLWebsocketResponse) ->
            handleAction $ CallCallback response.id response.payload
          Left err ->
            case (jsonParser msg >>= decodeJson) :: Either String {type :: String, id :: String} of
              Right response ->
                case response.type of
                  "complete" ->
                    H.liftEffect $ Console.log $ "message with id: \"" <> response.id <> "\" complete"
                  _ -> H.liftEffect $ Console.log $ show response
              Left err' ->
                case (jsonParser msg >>= decodeJson) :: Either String {type :: String} of
                  Right response ->
                    case response.type of
                      "connection_ack" ->
                        H.liftEffect $ Console.log "received connection ack"
                      "ka" ->
                        H.liftEffect $ Console.log "received keep-alive"
                      _ -> H.liftEffect $ Console.log $ show response
                  Left err'' -> do
                    H.liftEffect $ Console.log err''
                    H.liftEffect $ Console.log msg

  MessageParseError err msgJson ->
    H.liftEffect $ Console.log $ "Message parse error: " <> err <> " for message " <> stringify msgJson

  UpdateNodeValidity graphId nodeId isValid -> do
    H.modify_ $ _megagraph <<< _graph graphId <<< _node nodeId <<< _isValid .~ isValid
    state <- H.get
    H.raise $ MegagraphUpdated state.megagraph

  UpdateTitleValidity graphId isValid -> do
    H.modify_ $ _megagraph <<< _graph graphId <<< _title <<< _isValid .~ isValid
    state <- H.get
    H.raise $ MegagraphUpdated state.megagraph

handleQuery :: forall pa a.
               Query pa a -> H.HalogenM (State pa) (Action pa) Slots (Message pa) Aff (Maybe a)
handleQuery = case _ of
  Mutate megagraphMutation {onFail, onSuccess} a -> Just a <$
    case megagraphMutation of
      -- When updating node text:
      -- - update local node text state
      -- - if node has subgraph:
      --   - check if new text is a valid (unique) title
      --   - if so, update title locally and in db, update node text in db
      --   - if not, indicate local version of node is invalid
      -- - if node has no subgraph, update db with new node text
      UpdateNodeText node text ->
        case node.subgraph of
          Nothing -> pure unit
          Just subgraphId ->
            let
              updateTitleOp = [UpdateTitle subgraphId (trim node.text) (trim text)]
              updateNodeTextOp = [UpdateNodes [node] [node {text = trim text, isValid = true}]]
              updateNodeValidityOp isValid = [UpdateNodes [node] [node {isValid = isValid}]]
            in do
              updateNodeTextCallbackId <- H.liftEffect UUID.genUUID
              let
                updateNodeTextCallback = \msg ->
                  case parseNodeUpsertResponse msg of
                    Right 1 -> RaiseParentAction onSuccess
                    _ -> RaiseParentAction $ onFail "Couldn't update node text in database"
              updateTitleTextCallbackId <- H.liftEffect UUID.genUUID
              let
                -- Check uniqueness of title in the database
                callback = \msg ->
                  case parseGraphUpsertResponse msg of
                    -- If title can be updated, update node text in db, update title text locally
                    Right 1 -> DoMany [ SendMutation updateNodeTextCallbackId updateNodeTextCallback updateNodeTextOp
                                      , InterpretStateUpdates updateTitleOp
                                      ]
                    _ -> DoMany [ UpdateNodeValidity node.graphId node.id false
                                , RaiseParentAction $ onFail "Graph title update failed"
                                ]
              handleAction $ SendMutation updateTitleTextCallbackId callback updateTitleOp
              handleAction $ InterpretStateUpdates updateNodeTextOp

      -- | When updating a graph title:
      -- | - update local state
      -- | - check if title can be updated in db
      -- |   - if so, query for all nodes that have the updated graph as their subgraph, then update their text
      -- |   - if not, set local title to invalid
      UpdateTitleText graphId from to ->
        let
          updateTitleOp = [UpdateTitle graphId from (trim to)]
          updateNodesTextOp :: Array Node -> Array MegagraphStateUpdate
          updateNodesTextOp nodes = [UpdateNodes nodes (nodes <#> _{text = trim to, isValid = true})]
        in do
          updateNodesTextCallbackId <- H.liftEffect UUID.genUUID
          let
            updateNodesTextCallback nodes = \msg ->
              case parseNodeUpsertResponse msg of
                Right n -> if n == Array.length nodes
                           then DoMany [ InterpretStateUpdates $ updateNodesTextOp nodes
                                       , RaiseParentAction onSuccess
                                       ]
                           else RaiseParentAction $ onFail "Not all nodes could be updated with subgraph title"
                _ -> RaiseParentAction $ onFail "Failed to update node text with subgraph title"
          getNodesWithSubgraphCallbackId <- H.liftEffect UUID.genUUID
          let
            getNodesWithSubgraphCallback = \msg ->
              case parseNodesWithSubgraphResponse msg of
                Right nodes -> DoMany [ SendMutation
                                          updateNodesTextCallbackId
                                          (updateNodesTextCallback nodes)
                                          (updateNodesTextOp nodes)
                                      ]
                _ -> RaiseParentAction $ onFail "Failed to query nodes with updated subgraph"
          updateTitleCallbackId <- H.liftEffect UUID.genUUID
          let
            updateTitleCallback = \msg ->
              case parseGraphUpsertResponse msg of
                Right 1 -> DoMany [ UpdateTitleValidity graphId true
                                  , SendQuery getNodesWithSubgraphCallbackId getNodesWithSubgraphCallback $ nodesWithSubgraphQuery graphId
                                  ]
                _ -> DoMany [ UpdateTitleValidity graphId false
                            , RaiseParentAction $ onFail "Graph title update failed"
                            ]
          handleAction $ SendMutation updateTitleCallbackId updateTitleCallback updateTitleOp
          handleAction $ InterpretStateUpdates updateTitleOp

      LinkNodeSubgraph node -> do
        updateNodeSubgraphCallbackId <- H.liftEffect UUID.genUUID
        let
          updateNodeSubgraphCallback = \msg ->
            case parseNodeUpsertResponse msg of
              Right 1 -> RaiseParentAction onSuccess
              _ -> RaiseParentAction $ onFail "Error updating node subgraph"
        graphIdWithTitleCallbackId <- H.liftEffect UUID.genUUID
        let
          graphIdWithTitleCallback = \msg ->
            case parseGraphIdWithTitleResponse msg of
              Left err -> RaiseParentAction $ onFail err
              Right maybeGraphRow -> case maybeGraphRow of
                Nothing -> RaiseParentAction $ onSuccess
                Just graphRow ->
                  let
                    op = [UpdateNodes [node] [node {subgraph = Just graphRow.id}]]
                  in
                    DoMany [ SendMutation updateNodeSubgraphCallbackId updateNodeSubgraphCallback op
                           , InterpretStateUpdates op
                           ]
        handleAction $ SendQuery graphIdWithTitleCallbackId graphIdWithTitleCallback $ graphIdWithTitleQuery $ trim node.text

      StateUpdate megagraphStateUpdates -> do
        callbackId <- H.liftEffect UUID.genUUID
        let
          callback = \msg ->
            parseMegagraphUpsertResponse msg
            # either (RaiseParentAction <<< onFail) (const $ RaiseParentAction onSuccess)
        handleAction $ DoMany
          [ SendMutation callbackId callback megagraphStateUpdates
          , InterpretStateUpdates megagraphStateUpdates
          ]

  LoadGraph graphId a -> Just a <$ do
    callbackId <- H.liftEffect UUID.genUUID
    let
      callback = \msgJson -> do
        case parseGraphFetchResponse msgJson of
          Left err -> MessageParseError err msgJson
          Right {graph, mappings} ->
            let
              graphOp = encodeGraphAsMegagraphStateUpdates graph
              mappingUpdates = mappings # Array.concatMap \mapping ->
                encodeMappingAsMegagraphStateUpdates mapping
            in
              InterpretStateUpdates $ graphOp <> mappingUpdates
    state <- H.get
    let
      currentGraphIds = state.megagraph.graphs
                        # Map.keys # Array.fromFoldable
    H.liftEffect $ Console.log $ "Loading graph " <> show graphId
    handleAction $ SendQuery callbackId callback $ graphFetchQuery graphId currentGraphIds

  Drop (GraphComponent graphId) a -> Just a <$ do
    H.modify_ $ _megagraph <<< _graphs <<< at graphId .~ Nothing

  Drop (MappingComponent mappingId) a -> Just a <$ do
    H.modify_ $ _megagraph <<< _mappings <<< at mappingId .~ Nothing


-- Utils

subscribeWithAction :: forall pa.
                       EE.EventType
                       -> ET.EventTarget
                       -> (EE.Event -> Action pa)
                       -> H.HalogenM (State pa) (Action pa) Slots (Message pa) Aff Unit
subscribeWithAction eventType eventTarget action =
  H.subscribe' \subscriptionId ->
    ES.effectEventSource \emitter -> do
      listener <- ET.eventListener \event ->
          ES.emit emitter $ action event
      ET.addEventListener eventType listener false eventTarget
      pure mempty
