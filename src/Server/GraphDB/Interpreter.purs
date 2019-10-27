module Server.GraphDB.Interpreter where

import Prelude

import AppOperation (AppOperation(..), UndoOpF(..))
import AppOperation.GraphOp (GraphOpF(..), _graphOp, connectSubgraph, invertGraphOp, setTitleValidity)
import AppOperation.GraphOp as GraphOp
import AppOperation.Interpreter (collapseAppOperation)
import AppOperation.QueryServerOp (QueryServerOpF(..))
import AppOperation.UIOp (UIOpF(..))
import Control.Monad.Except.Trans (ExceptT(..))
import Core (GraphId, freshNode, freshEdge)
import Data.Array (length)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Traversable (for_, sequence_)
import Data.Tuple (Tuple(..))
import Data.UUID as UUID
import Effect (Effect)
import Effect.Aff (Aff, try)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Foreign.Generic (encodeJSON)
import Node.Websocket.Connection (sendMessage)
import Node.Websocket.Types (WSConnection, TextFrame(..))
import Run as Run
import SQLite3 (DBConnection)
import Server.Config (config)
import Server.GraphDB.ParseRow (parseHistoryRow)
import Server.GraphDB.Query (consHistory, consUndone, dbAction, deleteEdge, deleteNode, graphWithTitle, insertEdge, insertGraph, insertNode, lastOpFromHistory, lastOpFromUndone, loadGraphAsAppOperation, lookupRepresentativeInKnowledgeNavigator, moveNode, removeOpFromHistory, removeOpFromUndone, replaceLastOpInHistory, selectEdgesBetweenGraphs, selectEdgesBetweenNodes, selectNode, updateEdgeText, updateGraphTitle, updateNodeSubgraph, updateNodeText)
import Server.GraphDB.Schema (historyTableSchema, undoneTableSchema)

interpretAppOperation :: DBConnection -> WSConnection -> AppOperation Unit -> ExceptT String Aff Unit
interpretAppOperation db wsConn appOp =
  let
    AppOperation graphId op = appOp
  in do -- ExceptT String Aff
    op # Run.interpret
      (Run.match
         { graphOp       : handleGraphOp db wsConn
         , uiOp          : handleUIOp db wsConn
         , undoOp        : handleUndoOp db wsConn
         , queryServerOp : handleQueryServerOp db wsConn
         })

filteredHistoryUpdate :: AppOperation Unit -> DBConnection -> ExceptT String Aff Unit
filteredHistoryUpdate appOp db =
  let
    AppOperation graphId op = appOp
  in
    case Run.peel op of
      -- Empty op
      Right _ -> ExceptT $ pure $ Left "Tried to add empty operation to history"
      -- Non-empty op
      Left opV -> opV # Run.on _graphOp (\_ -> addToHistory appOp db)
        -- If not graphOp, don't record the action
        (Run.default $ pure unit)

addToHistory :: AppOperation Unit -> DBConnection -> ExceptT String Aff Unit
addToHistory appOp db =
  let
    AppOperation graphId op = appOp
  in do
    lastOpRowEither <- try $ lastOpFromHistory graphId db
    case lastOpRowEither of
      -- Couldn't read history.
      -- Make sure table exists then insert operation.
      Left error -> do
        ExceptT $ Right <$> do
          Console.log "Could not read last operation from history:"
          Console.log $ show error
          Console.log "Creating table if not exists..."
        dbAction (historyTableSchema graphId) {} db
        consHistory appOp db
      -- Can read last operation from history.
      -- Check if it can be collapased with the new op before inserting
      Right lastOpRow -> do
        lastOp <- ExceptT $ pure $ parseHistoryRow lastOpRow
        case collapseAppOperation appOp lastOp of
          Nothing -> do
            ExceptT $ Right <$> do
              Console.log "couldn't collapse app operation with history"
              Console.log "new operation:"
              Console.log $ show appOp
              Console.log "last operation in history:"
              Console.log $ show lastOp
            consHistory appOp db
          Just collapsedOp -> do
            replaceLastOpInHistory collapsedOp db
            ExceptT $ Right <$> do
              Console.log "replaced last op in hisotry with collapsed op"


handleGraphOp :: forall a. DBConnection -> WSConnection -> GraphOpF a -> ExceptT String Aff a
handleGraphOp db wsConn = case _ of
  InsertNode graphId nodeId next -> do
    _ <- insertNode (freshNode graphId nodeId) db
    pure next

  DeleteNode graphId nodeId next -> do
    _ <- deleteNode nodeId db
    pure next

  InsertEdge edgeId next -> do
    _ <- insertEdge (freshEdge edgeId) db
    if edgeId.sourceGraph /= edgeId.targetGraph
    then do
      -- Edge is a mapping between graphs
      -- Keep knowledge neighborhood up-to-date with mappings
      sourceNodes <- lookupRepresentativeInKnowledgeNavigator edgeId.sourceGraph db
      targetNodes <- lookupRepresentativeInKnowledgeNavigator edgeId.targetGraph db
      sequence_ do -- Array
        sourceNode <- sourceNodes
        targetNode <- targetNodes
        pure do -- ExceptT String Aff
          rows <- selectEdgesBetweenNodes sourceNode.id targetNode.id db
          if length rows == 0
          then
            let
              knnEdgeId = { source      : sourceNode.id
                          , sourceGraph : config.knowledgeNavigatorId
                          , target      : targetNode.id
                          , targetGraph : config.knowledgeNavigatorId
                          }
            in do
              insertEdge (freshEdge knnEdgeId) db
              -- TODO: check if client is subscribed to knowledge neighborhood before sending
              liftEffect $ sendOperation wsConn $ AppOperation edgeId.sourceGraph $ GraphOp.insertEdge knnEdgeId
          else pure unit
    else
      pure unit
    pure next

  DeleteEdge edgeId next -> do
    _ <- deleteEdge edgeId db
    if edgeId.sourceGraph /= edgeId.targetGraph
    then do
      -- Edge is a mapping between graphs
      -- Keep knowledge neighborhood up-to-date with mappings
      rows <- selectEdgesBetweenGraphs edgeId.sourceGraph edgeId.targetGraph db
      if length rows == 0
      then do
        sourceNodes <- lookupRepresentativeInKnowledgeNavigator edgeId.sourceGraph db
        targetNodes <- lookupRepresentativeInKnowledgeNavigator edgeId.targetGraph db
        sequence_ do -- Array
          sourceNode <- sourceNodes
          targetNode <- targetNodes
          let knnEdgeId = { source      : sourceNode.id
                          , sourceGraph : config.knowledgeNavigatorId
                          , target      : targetNode.id
                          , targetGraph : config.knowledgeNavigatorId
                          }
          pure do
            deleteEdge knnEdgeId db
            -- TODO: check if client is subscribed to knowledge neighborhood before sending
            liftEffect $ sendOperation wsConn $ AppOperation edgeId.sourceGraph $ GraphOp.deleteEdge $ freshEdge knnEdgeId
      else pure unit
    else
      pure unit
    pure next

  MoveNode nodeId _ to next -> do
    _ <- moveNode nodeId to db
    pure next

  UpdateNodeText nodeId _ to next -> do
    _ <- updateNodeText nodeId to db
    pure next

  UpdateEdgeText edgeId _ to next -> do
    _ <- updateEdgeText edgeId to db
    pure next

  -- Mirror updates to the title in the graphs representative node
  -- in the knowledge neighborhood (if it has one)
  UpdateTitle graphId _ to next -> do
    result <- try $ updateGraphTitle graphId to db
    case result of
      Left error ->
        let
          responseOp = AppOperation graphId $ setTitleValidity graphId false
        in ExceptT $ map Right do
          Console.log "Failed to update title"
          liftEffect $ sendOperation wsConn responseOp
          pure next
      Right _ ->
        let
          responseOp = AppOperation graphId $ setTitleValidity graphId true
        in do
          ExceptT $ map Right $ liftEffect $ sendOperation wsConn responseOp
          representativeNodes <- lookupRepresentativeInKnowledgeNavigator graphId db
          for_ representativeNodes \node -> do
            updateNodeText node.id to db
            -- TODO: check if client is subscribed to knowledge neighborhood before sending
            ExceptT $ map Right $ liftEffect $ sendOperation wsConn $ AppOperation config.knowledgeNavigatorId $
              GraphOp.updateNodeText (freshNode node.graphId node.id) to
          pure next

  SetTitleValidity graphId newValidity next -> do
    pure next

  ConnectSubgraph nodeId old new next -> do
    _ <- updateNodeSubgraph nodeId new db
    pure next

handleUIOp :: forall a. DBConnection -> WSConnection -> UIOpF a -> ExceptT String Aff a
handleUIOp db wsConn = case _ of
  MoveGraphOrigin graphId newGraphOrigin next ->
    pure next

  UpdateZoom graphId newZoom next ->
    pure next

  InsertPane graphId next -> do
    result <- try $ serveGraph graphId db wsConn
    case result of
      Left error -> do
        Console.log error
        title <- UUID.toString <$> liftEffect UUID.genUUID
        _ <- insertGraph graphId title db
        pure unit
      Right _ -> pure unit
    pure next

  RemovePane graphId next ->
    pure next

  RescalePane graphId rect next ->
    pure next

handleUndoOp :: forall a. DBConnection -> WSConnection -> UndoOpF a -> ExceptT String Aff a
handleUndoOp db wsConn = case _ of
  Undo graphId next -> do
    -- if there is a last action to fetch,
    -- reverse it, apply the reverse and add it to undone (creating undone if it doens't exist)
    lastOpRow <- lastOpFromHistory graphId db
    lastOp <- ExceptT $ pure $ parseHistoryRow lastOpRow
    let
      AppOperation graphId lastOpRun = lastOp
      reversedOpRun = invertGraphOp lastOpRun
    interpretAppOperation db wsConn $ AppOperation graphId reversedOpRun
    removeOpFromHistory graphId lastOpRow db
    result <- try $ consUndone lastOp db
    case result of
      Left error -> do
        ExceptT $ Right <$> do
          Console.log "Error inserting op into undone table."
          Console.log "Making sure undone table exists then trying again"
        dbAction (undoneTableSchema graphId) {} db
        consUndone lastOp db
      Right _ -> pure unit
    pure next

  Redo graphId next -> do
    -- if there is a last action to fetch,
    -- apply it and add it to history
    lastOpRow <- lastOpFromUndone graphId db
    lastOp <- ExceptT $ pure $ parseHistoryRow lastOpRow
    interpretAppOperation db wsConn lastOp
    let AppOperation graphId _ = lastOp
    removeOpFromUndone graphId lastOpRow db
    consHistory lastOp db
    pure next

  SetHistory graphId history next ->
    pure next

  SetUndone graphId undone next ->
    pure next

serveGraph :: GraphId -> DBConnection -> WSConnection -> ExceptT String Aff Unit
serveGraph graphId db wsConn = do
  Console.log $ "Serving graph: " <> show graphId
  result <- try $ loadGraphAsAppOperation graphId db
  case result of
    Left error -> ExceptT $ Left <$> do
      Console.log "Error loading graph:"
      pure error
    Right appOp -> liftEffect $ sendOperation wsConn appOp

handleQueryServerOp :: forall a. DBConnection -> WSConnection -> QueryServerOpF a -> ExceptT String Aff a
handleQueryServerOp db wsConn = case _ of
   ConnectSubgraphIfTitleExists nodeId title next -> do
     maybeGraphId <- graphWithTitle (String.trim title) db
     maybeNode <- selectNode nodeId db
     case do
       graphId <- maybeGraphId
       node <- maybeNode
       pure $ Tuple graphId node
     of
       Nothing -> pure next
       Just (Tuple graphId node) ->
         let
           op = AppOperation graphId $ connectSubgraph node (Just graphId)
         in do
           interpretAppOperation db wsConn op
           liftEffect $ sendOperation wsConn op
           pure next

sendOperation :: WSConnection -> AppOperation Unit -> Effect Unit
sendOperation wsConn op =
  sendMessage wsConn $ Left $ TextFrame
    { type     : "utf8"
    , utf8Data : encodeJSON op
    }
