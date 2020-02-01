-- | Purescript data structure implementation
module Interpreter where

import Prelude

import AppOperation (AppOperation(..), HistoryUpdate(..), MegagraphElement(..))
import AppState (MegagraphState, _graph, _graphState, _graphs, _mapping, _mappingState, _mappings, _pane, emptyGraphState, emptyMappingState)
import Data.Array as Array
import Data.Foldable (foldl)
import Data.Lens ((%~), (?~), (^?), traversed)
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Megagraph (Graph, Mapping, _boundingRect, _height, connectSubgraph, deleteEdge, deleteEdgeMappingEdge, deleteNode, deleteNodeMappingEdge, deletePathEquation, insertEdgeMappingEdge, insertNewEdge, insertNewNode, insertNodeMappingEdge, insertPathEquation, moveNode, setTitleValidity, updateEdgeMidpoint, updateEdgeText, updateNodeText, updateTitle)
import MegagraphOperation (EquationOperation(..), GraphOperation(..), MappingOperation(..), MegagraphOperation(..), MegagraphUpdate, collapseMegagraphOperations)

--import UI.Panes (arrangePanes, insertPaneImpl, rescalePaneImpl)


interpretGraphOperation :: GraphOperation -> Graph -> Graph
interpretGraphOperation = case _ of
  InsertNode nodeId ->               insertNewNode nodeId
  DeleteNode nodeId ->               deleteNode nodeId
  InsertEdge edgeMetadata ->         insertNewEdge edgeMetadata
  DeleteEdge edgeMetadata ->         deleteEdge edgeMetadata.id
  MoveNode nodeId from to ->         moveNode nodeId to
  UpdateNodeText nodeId from to ->   updateNodeText nodeId to
  UpdateEdgeText edgeId from to ->   updateEdgeText edgeId to
  MoveEdgeMidpoint edgeId from to -> updateEdgeMidpoint edgeId to
  UpdateTitle from to ->             updateTitle to
  SetTitleValidity old new ->        setTitleValidity new
  ConnectSubgraph nodeId old new ->  connectSubgraph nodeId new

interpretEquationOperation :: EquationOperation -> Graph -> Graph
interpretEquationOperation = case _ of
  InsertPathEquation pathEquation -> insertPathEquation pathEquation
  DeletePathEquation pathEquation -> deletePathEquation pathEquation

interpretMappingOperation :: MappingOperation -> Mapping -> Mapping
interpretMappingOperation = case _ of
  InsertNodeMappingEdge nodeMappingEdge ->
    insertNodeMappingEdge nodeMappingEdge
  DeleteNodeMappingEdge nodeMappingEdge ->
    deleteNodeMappingEdge nodeMappingEdge
  InsertEdgeMappingEdge edgeMappingEdge ->
    insertEdgeMappingEdge edgeMappingEdge
  DeleteEdgeMappingEdge edgeMappingEdge ->
    deleteEdgeMappingEdge edgeMappingEdge

interpretMegagraphOperation :: MegagraphOperation -> MegagraphState -> MegagraphState
interpretMegagraphOperation = case _ of
  GraphElementOperation graphId graphOp ->
    _graphState graphId <<< traversed <<< _graph %~ interpretGraphOperation graphOp
  GraphElementEquationOperation graphId equationOp ->
    _graphState graphId <<< traversed <<< _graph %~ interpretEquationOperation equationOp
  MappingElementOperation mappingId mappingOp ->
    _mappingState mappingId <<< traversed <<< _mapping %~ interpretMappingOperation mappingOp

collapseHistory :: MegagraphUpdate -> MegagraphUpdate -> Maybe MegagraphUpdate
collapseHistory opA opB =
  case Array.uncons opA, Array.uncons opB of
    Just unconsA, Just unconsB ->
      -- Only collapse single-operation updates
      if Array.null unconsA.tail && Array.null unconsB.tail
      then Array.singleton <$> collapseMegagraphOperations unconsA.head unconsB.head
      else Nothing
    _, _ -> Nothing

interpretAppOperation :: AppOperation -> MegagraphState -> MegagraphState
interpretAppOperation (AppOperation {op, target, historyUpdate, undoneUpdate}) megagraph =
  let
    megagraphHeight = fromMaybe 0.0 (megagraph ^? _graphs <<< traversed <<< _pane <<< _boundingRect <<< _height)
    newRect = { height : megagraphHeight, bottom : megagraphHeight, width : 0.0, top : 0.0, left : 0.0, right : 0.0 }
    createTargetIfNotExists (GraphElement graphId) = case Map.lookup graphId megagraph.graphs of
      Just _ -> identity
      Nothing -> _graphs <<< at graphId ?~ emptyGraphState graphId newRect
    createTargetIfNotExists (MappingElement mappingId from to) = case Map.lookup mappingId megagraph.mappings of
      Just _ -> identity
      Nothing -> _mappings <<< at mappingId ?~ emptyMappingState mappingId from to
    applyMegagraphUpdate = \op' megagraph' -> foldl (flip interpretMegagraphOperation) megagraph' op'
    historyUpdater = case _ of
      Insert op'' -> \history -> fromMaybe (Array.cons op'' history) do
        {head, tail} <- Array.uncons history
        collapsedOp <- collapseHistory op'' head
        pure $ Array.cons collapsedOp tail
      Pop -> Array.drop 1
      Replace newHistory -> const newHistory
      NoOp -> identity
    updateComponentHistory target' = case target' of
      GraphElement graphId ->
        (_graphState graphId <<< traversed <<< prop (SProxy :: SProxy "history") %~ historyUpdater historyUpdate)
        >>>
        (_graphState graphId <<< traversed <<< prop (SProxy :: SProxy "undone") %~ historyUpdater undoneUpdate)
      MappingElement mappingId from to ->
        (_mappingState mappingId <<< traversed <<< prop (SProxy :: SProxy "history") %~ historyUpdater historyUpdate)
        >>>
        (_mappingState mappingId <<< traversed <<< prop (SProxy :: SProxy "undone") %~ historyUpdater undoneUpdate)
  in
    megagraph
    # createTargetIfNotExists target
    # applyMegagraphUpdate op
    # updateComponentHistory target


--removePaneImpl :: GraphId -> AppState -> AppState
--removePaneImpl graphId =
--  removeGraphData graphId
--  >>>
--  arrangePanes
--  -- Focus on the rightmost pane
--  >>> \appState -> case appState.graphData.panes
--                        # Map.values >>> Array.fromFoldable
--                        # Array.sortBy (comparing _.boundingRect.right)
--                        # Array.head of
--        Nothing -> appState
--        Just nextPane -> appState { focusedPane = Just nextPane.graphId }
