module Server.GraphDB.Interpreter where

import Prelude

import AppOperation (AppOperation(..))
import AppOperation.GraphOp (GraphOpF(..), setTitleValidity)
import AppOperation.UIOp (UIOpF(..))
import AppOperation.UndoOp (UndoOpF(..))
import Core (freshNode, freshEdge)
import Data.Either (Either(..))
import Data.Newtype (unwrap)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Foreign.Generic (encodeJSON)
import Run as Run
import SQLite3 (DBConnection)
import Server.Query (deleteNode, insertGraph, insertNode, moveNode, updateGraphTitle, updateNodeText, insertEdge, deleteEdge, updateEdgeText)
import Node.Websocket.Connection (sendMessage)
import Node.Websocket.Types (WSConnection, TextFrame(..))

interpretAppOperation :: forall a. DBConnection -> WSConnection -> AppOperation a -> Aff a
interpretAppOperation db wsConn =
  unwrap >>>
  Run.interpret
    (Run.match
      { graphOp : handleGraphOp db wsConn
      , uiOp    : handleUIOp db
      , undoOp  : handleUndoOp db
      })

handleGraphOp :: forall a. DBConnection -> WSConnection -> GraphOpF a -> Aff a
handleGraphOp db wsConn = case _ of
  NewGraph graphId title next -> do
    _ <- insertGraph graphId title db
    pure next

  InsertNode graphId nodeId next -> do
    _ <- insertNode (freshNode graphId nodeId) db
    pure next

  DeleteNode graphId nodeId next -> do
    _ <- deleteNode nodeId db
    pure next

  InsertEdge edgeId next -> do
    _ <- insertEdge (freshEdge edgeId) db
    pure next

  DeleteEdge edgeId next -> do
    _ <- deleteEdge edgeId db
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

  UpdateTitle graphId _ to next -> do
    result <- updateGraphTitle graphId to db
    case result of
      Left error ->
        let
          responseOp = AppOperation $ setTitleValidity graphId false
        in do
          Console.log "Failed to update title"
          liftEffect $ sendMessage wsConn $ Left
            $ TextFrame { type : "utf8"
                        , utf8Data : encodeJSON responseOp
                        }
          pure next
      Right _ ->
        let
          responseOp = AppOperation $ setTitleValidity graphId true
        in do
          liftEffect $ sendMessage wsConn $ Left
            $ TextFrame { type : "utf8"
                        , utf8Data : encodeJSON responseOp
                        }
          pure next

  SetTitleValidity graphId newValidity next -> do
    pure next

handleUIOp :: forall a. DBConnection -> UIOpF a -> Aff a
handleUIOp db = case _ of
  MoveGraphOrigin graphId newGraphOrigin next ->
    pure next

  UpdateZoom graphId newZoom next ->
    pure next

  InsertPane graphId next ->
    pure next

  RemovePane graphId next ->
    pure next

  RescalePane graphId rect next ->
    pure next

  RescaleWindow rect next ->
    pure next

handleUndoOp :: forall a. DBConnection -> UndoOpF a -> Aff a
handleUndoOp db = case _ of
  Undo graphId next ->
    pure next

  Redo graphId next ->
    pure next
