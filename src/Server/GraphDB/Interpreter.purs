module Server.GraphDB.Interpreter where

import Prelude

import AppOperation (AppOperation)
import AppOperation.GraphOp (GraphOpF(..))
import AppOperation.UIOp (UIOpF(..))
import AppOperation.UndoOp (UndoOpF(..))
import Core (freshNode, freshEdge)
import Data.Newtype (unwrap)
import Effect.Aff (Aff)
import Run as Run
import SQLite3 (DBConnection)
import Server.Query (deleteNode, insertGraph, insertNode, moveNode, updateGraphTitle, updateNodeText, insertEdge, deleteEdge, updateEdgeText)

interpretAppOperation :: DBConnection -> AppOperation Unit -> Aff Unit
interpretAppOperation db =
  unwrap >>>
  Run.interpret
    (Run.match
      { graphOp : handleGraphOp db
      , uiOp    : handleUIOp db
      , undoOp  : handleUndoOp db
      })

handleGraphOp :: forall a. DBConnection -> GraphOpF a -> Aff a
handleGraphOp db = case _ of
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
    _ <- updateGraphTitle graphId to db
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
