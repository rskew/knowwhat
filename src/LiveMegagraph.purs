-- | A LiveMegagraph is a megagraph that is synced with a database
-- | on the other end of a GraphQL-over-websocket connection.
-- |
-- | The GraphQL-over-websocket details should be separated into their own
-- | library, but that should ideally come with some type-checking of queries
-- | and mutations, which will be some work to implement for the general case.
-- | So, for now, this module does everything, including registering and
-- | calling channels for individual messages over the websocket connection :D
module LiveMegagraph where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', (.~), (%~), (?~), (^?))
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.List.Types (toList)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (trim, joinWith)
import Data.Symbol (SProxy(..))
import Data.Traversable (for_)
import Data.UUID (UUID)
import Data.UUID as UUID
import Effect.Aff (Aff)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Console as Console
import Foreign (Foreign, F, MultipleErrors, readString, renderForeignError, typeOf, unsafeFromForeign)
import Foreign.Class (class Encode, class Decode)
import Foreign.Generic (decode, decodeJSON, genericEncode, genericDecode, defaultOptions, encodeJSON)
import FunctorialDataMigration.Core.SignatureMapping (mappingIsWellFormed)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import HasuraQuery (GraphQLQuery, GraphQLWebsocketResponse, renderMutation, renderQuery)
import Interpreter (interpretMegagraphStateUpdate, megagraphUpdateToQuery)
import Megagraph (GraphId, Megagraph, Node, NodeId, _graph, _graphs, _isValid, _mapping, _mappings, _node, _title, emptyMegagraph, mappingToSignatureMapping)
import MegagraphStateUpdate (MegagraphComponent(..), MegagraphStateUpdate(..), encodeGraphAsMegagraphStateUpdates, encodeMappingAsMegagraphStateUpdates, invertMegagraphStateUpdates)
import Query (MegagraphSchema, graphFetchQuery, graphIdWithTitleQuery, nodesWithSubgraphQuery, parseGraphFetchResponse, parseGraphIdWithTitleResponse, parseGraphUpsertResponse, parseNodeUpsertResponse, parseNodesWithSubgraphResponse)
import Web.Event.Event as EE
import Web.Event.EventTarget as ET
import Web.Socket.Event.EventTypes as WSET
import Web.Socket.Event.MessageEvent as ME
import Web.Socket.ReadyState (ReadyState(..))
import Web.Socket.WebSocket as WS

-- | State updates requiring different interactions with the backend.
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

type ChannelId = UUID

type State
  = { megagraph :: Megagraph
    , channels :: Map ChannelId (AVar Foreign)
    , webSocketConnection :: WS.WebSocket
    , messageQueue :: Array String
    }

emptyState :: WS.WebSocket -> State
emptyState connection
  = { megagraph: emptyMegagraph
    , channels: Map.empty
    , webSocketConnection: connection
    , messageQueue: []
    }

_megagraph :: Lens' State Megagraph
_megagraph = prop (SProxy :: SProxy "megagraph")

_channels :: Lens' State (Map ChannelId (AVar Foreign))
_channels = prop (SProxy :: SProxy "channels")

_messageQueue :: Lens' State (Array String)
_messageQueue = prop (SProxy :: SProxy "messageQueue")

data Action pa
  = Init
  | WebSocketConnectionInit
  | DoMany (Array (Action pa))
  | InterpretStateUpdates (Array MegagraphStateUpdate)
  | RegisterChannel ChannelId (AVar Foreign)
  | PutInChannel ChannelId Foreign
  | RaiseParentAction pa
  | DecodeWebSocketEvent EE.Event
  | UpdateNodeValidity GraphId NodeId Boolean
  | UpdateTitleValidity GraphId Boolean

data Query pa a
  = Mutate MegagraphMutation {onFail :: String -> pa, onSuccess :: pa} a
  | LoadGraph GraphId {onFail :: String -> pa, onSuccess :: pa} a
  | LoadGraphWithTitle String {onFail :: String -> pa, onSuccess :: pa} a
  | NodesWithSubgraph GraphId {onFail :: String -> pa, onSuccess :: pa} (Array Node -> a)
  | Drop MegagraphComponent a

data Message pa
  = ReturnAction pa
  | MegagraphUpdated Megagraph

type Slot pa = H.Slot (Query pa) (Message pa)

type Input = WS.WebSocket

type Slots = ()

-- | The parent component's action type returned from channels
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

render :: forall pa. State -> H.ComponentHTML (Action pa) Slots Aff
render state =
  let
    classes = joinWith " " $ Array.catMaybes
              [ Just "pendingIndicator"
              , if not Map.isEmpty state.channels
                then Just "pending"
                else Nothing
              ]
  in
    HH.div
    [ HP.class_ $ HH.ClassName classes
    ]
    []

handleAction :: forall pa. Action pa -> H.HalogenM State (Action pa) Slots (Message pa) Aff Unit
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
    -- Tell parent
    state <- H.get
    H.raise $ MegagraphUpdated state.megagraph

  RegisterChannel channelId avar ->
    H.modify_ $ _channels <<< at channelId ?~ avar

  PutInChannel channelId msg -> do
    state <- H.get
    case Map.lookup channelId state.channels of
      Nothing -> H.liftEffect $ Console.log
                 $ "Mesage received by no channel registered, msg: " <> unsafeFromForeign msg
                 <> " of type " <> typeOf msg
                 <> " for channelId: " <> show channelId
      Just avar -> H.liftAff $ AVar.put msg avar
    H.modify_ $ _channels <<< at channelId .~ Nothing

  RaiseParentAction parentAction -> H.raise $ ReturnAction parentAction

  DecodeWebSocketEvent event ->
    case do
      ME.fromEvent event
      >>= ME.data_ >>> readString >>> runExcept >>> either (const Nothing) Just
    of
      Nothing -> pure unit
      Just msg ->
        case runExcept $ decodeJSON msg
        of
          Right (response :: GraphQLWebsocketResponse) -> do
            handleAction $ PutInChannel response.id response.payload.data
          Left err ->
            case (runExcept $ decodeJSON msg) :: Either MultipleErrors {type :: String, id :: String} of
              Right response ->
                case response.type of
                  "complete" -> pure unit
                  _ -> H.liftEffect $ Console.log $ show response
              Left err' ->
                case (runExcept $ decodeJSON msg) :: Either MultipleErrors {type :: String} of
                  Right response ->
                    case response.type of
                      "connection_ack" ->
                        H.liftEffect $ Console.log "received connection ack"
                      "ka" ->
                        H.liftEffect $ Console.log "received keep-alive"
                      _ -> H.liftEffect $ Console.log $ show response
                  Left err'' -> do
                    H.liftEffect $ Console.log $ "Unhandled websocket message: " <> (joinWith " " $ Array.fromFoldable $ toList $ renderForeignError <$> err'')
                    H.liftEffect $ Console.log msg

  UpdateNodeValidity graphId nodeId isValid -> do
    H.modify_ $ _megagraph <<< _graph graphId <<< _node nodeId <<< _isValid .~ isValid
    state <- H.get
    H.raise $ MegagraphUpdated state.megagraph

  UpdateTitleValidity graphId isValid -> do
    H.modify_ $ _megagraph <<< _graph graphId <<< _title <<< _isValid .~ isValid
    state <- H.get
    H.raise $ MegagraphUpdated state.megagraph

handleQuery :: forall pa a.
               Query pa a -> H.HalogenM State (Action pa) Slots (Message pa) Aff (Maybe a)
handleQuery = case _ of
  Mutate megagraphMutation {onFail, onSuccess} a -> Just a <$
    case megagraphMutation of
      -- | When updating node text:
      -- | - update local node text state
      -- | - if node has subgraph:
      -- |   - check if new text is a valid (unique) title
      -- |   - if so, update title locally and in db, update node text in db
      -- |   - if not, indicate local version of node is invalid
      -- | - if node has no subgraph, update db with new node text
      UpdateNodeText node text ->
        let
          updateNodeTextOp = [UpdateNodes [node] [node {text = trim text, isValid = true}]]
          updateNodeText = do
            response <- sendMutation updateNodeTextOp
            case runExcept $ parseNodeUpsertResponse response of
              Right 1 -> handleAction $ RaiseParentAction onSuccess
              _ -> handleAction $ RaiseParentAction $ onFail "Couldn't update node text in database"
        in do
          handleAction $ InterpretStateUpdates updateNodeTextOp
          case node.subgraph of
            Nothing -> updateNodeText
            Just subgraphId ->
              let
                updateTitleOp = [UpdateTitle subgraphId (trim node.text) (trim text)]
                updateNodeValidityOp isValid = [UpdateNodes [node] [node {isValid = isValid}]]
              in do
                response <- sendMutation updateTitleOp
                case runExcept $ parseGraphUpsertResponse response of
                  -- If title can be updated, update node text in db, update title text locally
                  Right 1 -> do
                    handleAction $ InterpretStateUpdates updateTitleOp
                    updateNodeText
                  _ -> do
                    handleAction $ UpdateNodeValidity node.graphId node.id false
                    handleAction $ RaiseParentAction $ onFail "Graph title update failed"

      -- | When updating a graph title:
      -- | - update local state
      -- | - check if title can be updated in db
      -- |   - if so, query for all nodes that have the updated graph as their subgraph, then update their text
      -- |   - if not, set local title to invalid
      UpdateTitleText graphId from to ->
        let
          updateTitleOp = [UpdateTitle graphId from (trim to)]
          updateNodesTextOp nodes = [UpdateNodes nodes (nodes <#> _{text = trim to, isValid = true})]
        in do
          handleAction $ InterpretStateUpdates updateTitleOp
          response <- sendMutation updateTitleOp
          case runExcept $ parseGraphUpsertResponse response of
            Right 1 -> do
              handleAction $ UpdateTitleValidity graphId true
              response' <- sendQuery $ nodesWithSubgraphQuery graphId
              case runExcept $ parseNodesWithSubgraphResponse response' of
                Right nodes -> do
                  response'' <- sendMutation $ updateNodesTextOp nodes
                  case runExcept $ parseNodeUpsertResponse response'' of
                    Right n -> if n == Array.length nodes
                               then do
                                 handleAction $ InterpretStateUpdates $ updateNodesTextOp nodes
                                 handleAction $ RaiseParentAction onSuccess
                               else
                                 handleAction $ RaiseParentAction $ onFail "Not all nodes could be updated with subgraph title"
                    _ -> handleAction $ RaiseParentAction $ onFail "Failed to update node text with subgraph title"
                _ -> handleAction $ RaiseParentAction $ onFail "Failed to query nodes with updated subgraph"
            _ -> do
              handleAction $ UpdateTitleValidity graphId false
              handleAction $ RaiseParentAction $ onFail "Graph title update failed"

      LinkNodeSubgraph node -> do
        response <- sendQuery $ graphIdWithTitleQuery $ trim node.text
        case runExcept $ parseGraphIdWithTitleResponse response of
          Left err -> handleAction $ RaiseParentAction $ onFail
                      $ joinWith " " $ Array.fromFoldable $ toList $ renderForeignError <$> err
          Right maybeGraphRow -> case maybeGraphRow of
            Nothing -> handleAction $ RaiseParentAction onSuccess
            Just graphRow ->
              let
                op = [UpdateNodes [node] [node {subgraph = Just graphRow.id}]]
              in do
                response' <- sendMutation op
                case runExcept $ parseNodeUpsertResponse response' of
                  Right 1 -> handleAction $ RaiseParentAction onSuccess
                  _ -> handleAction $ RaiseParentAction $ onFail "Error updating node subgraph"
                handleAction $ InterpretStateUpdates op

      StateUpdate megagraphStateUpdates -> do
        handleAction $ InterpretStateUpdates megagraphStateUpdates
        response <- sendMutation megagraphStateUpdates
        case runExcept $ (decode response :: F {data :: Foreign}) of
          Left err -> handleAction $ RaiseParentAction $ onFail
                      $ joinWith " " $ Array.fromFoldable $ toList $ renderForeignError <$> err
          Right _ -> handleAction $ RaiseParentAction onSuccess

  LoadGraph graphId {onFail, onSuccess} a -> Just a <$ do
    state <- H.get
    let
      currentGraphIds = state.megagraph.graphs
                        # Map.keys # Array.fromFoldable
    H.liftEffect $ Console.log $ "Loading graph " <> show graphId
    response <- sendQuery $ graphFetchQuery graphId currentGraphIds
    case runExcept $ parseGraphFetchResponse response of
      Left err -> handleAction $ RaiseParentAction $ onFail
                  $ "Unable to parse graphFetchResponse: " <> (joinWith " " $ Array.fromFoldable $ toList $ renderForeignError <$> err)
      Right {graph, mappings} ->
        let
          graphOp = encodeGraphAsMegagraphStateUpdates graph
          mappingUpdates = mappings # Array.concatMap \mapping ->
            encodeMappingAsMegagraphStateUpdates mapping
        in do
          handleAction $ InterpretStateUpdates $ graphOp <> mappingUpdates
          -- Check validity of mappings
          state' <- H.get
          for_ (state'.megagraph.mappings) \mapping ->
            case state'.megagraph ^? _graph mapping.sourceGraph, state'.megagraph ^? _graph mapping.targetGraph of
              Just sourceGraph, Just targetGraph -> do
                let mappingIsValid = mappingIsWellFormed $ mappingToSignatureMapping mapping sourceGraph targetGraph
                H.modify_ $ _megagraph <<< _mapping mapping.id <<< _isValid .~ mappingIsValid
              _, _ ->
                pure unit
          state'' <- H.get
          H.raise $ MegagraphUpdated state''.megagraph
          handleAction $ RaiseParentAction onSuccess

  LoadGraphWithTitle title {onFail, onSuccess} a -> Just a <$ do
    response <- sendQuery $ graphIdWithTitleQuery $ trim title
    case runExcept $ parseGraphIdWithTitleResponse response of
      Left err -> handleAction $ RaiseParentAction $ onFail
                    $ "Unable to parse graphIdWithTitleResponse: " <> (joinWith " " $ Array.fromFoldable $ toList $ renderForeignError <$> err)
      Right maybeGraphRow -> case maybeGraphRow of
        Nothing -> handleAction $ RaiseParentAction $ onFail $ "No graph with title " <> title
        Just graphRow -> void $ handleQuery $ LoadGraph graphRow.id {onFail, onSuccess} a

  NodesWithSubgraph graphId {onFail, onSuccess} next -> do
    response <- sendQuery $ nodesWithSubgraphQuery graphId
    case runExcept $ parseNodesWithSubgraphResponse response of
      Left err -> do
        handleAction $ RaiseParentAction $ onFail
          $ "Unable to parse nodesWithSubgraphResponse: " <> (joinWith " " $ Array.fromFoldable $ toList $ renderForeignError <$> err)
        pure Nothing
      Right nodesWithSubgraph -> do
        handleAction $ RaiseParentAction $ onSuccess
        pure $ Just $ next nodesWithSubgraph

  Drop (GraphComponent graphId) a -> Just a <$ do
    H.modify_ $ _megagraph <<< _graphs <<< at graphId .~ Nothing

  Drop (MappingComponent mappingId) a -> Just a <$ do
    H.modify_ $ _megagraph <<< _mappings <<< at mappingId .~ Nothing

type HalogenApp pa = H.HalogenM State (Action pa) Slots (Message pa) Aff

sendQuery :: forall pa. GraphQLQuery MegagraphSchema -> HalogenApp pa Foreign
sendQuery query = sendGraphQL $ renderQuery query

sendMutation :: forall pa. Array MegagraphStateUpdate -> HalogenApp pa Foreign
sendMutation op = sendGraphQL $ renderMutation $ megagraphUpdateToQuery op

sendGraphQL :: forall pa. (UUID -> String) -> HalogenApp pa Foreign
sendGraphQL query = do
  channelId <- H.liftEffect UUID.genUUID
  avar <- H.liftAff AVar.empty
  handleAction $ RegisterChannel channelId avar
  sendMessage $ query channelId
  -- Wait for channel to be called with response
  H.liftAff $ AVar.take avar

sendMessage :: forall pa. String -> HalogenApp pa Unit
sendMessage message = do
  state <- H.get
  webSocketState <- H.liftEffect $ WS.readyState state.webSocketConnection
  case webSocketState of
    Open -> do
      H.liftEffect $ WS.sendString state.webSocketConnection message
    -- If connection isn't open yet, enqueue message
    _ -> H.modify_ $ _messageQueue %~ Array.cons message


-- Utils

subscribeWithAction :: forall pa.
                       EE.EventType
                       -> ET.EventTarget
                       -> (EE.Event -> Action pa)
                       -> H.HalogenM State (Action pa) Slots (Message pa) Aff Unit
subscribeWithAction eventType eventTarget action =
  H.subscribe' \subscriptionId ->
    ES.effectEventSource \emitter -> do
      listener <- ET.eventListener \event ->
          ES.emit emitter $ action event
      ET.addEventListener eventType listener false eventTarget
      pure mempty
