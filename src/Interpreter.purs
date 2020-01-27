-- | Purescript data structure implementation
module Interpreter where

import Prelude

import AppOperation (AppOperation(..), UndoOpF(..), _undoOp, UNDOOP)
import AppOperation.GraphOp (_graphOp, collapseGraphOpF, encodeGraphDataAsGraphOp, interpretGraphOp, invertGraphOp)
import AppOperation.QueryServerOp (interpretQueryServerOpOnClient)
import AppOperation.UIOp (UIOpF(..), _uiOp, UIOP)
import AppState (AppState, _graphState, _mappingState)
import Graph (GraphId)
import Data.Array (cons, uncons)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Lens ((.~), (%~))
import Data.Lens.At (at)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst)
import Run (Run, Step(..))
import Run as Run
import UI.Panes (arrangePanes, insertPaneImpl, rescalePaneImpl)



interpretGraphOperation :: GraphOperation -> SubMegagraph -> SubMegagraph
interpretGraphOperation = case _ of
  InsertNode nodeId ->                   insertNewNode nodeId
  DeleteNode nodeId ->                   deleteNode nodeId
  InsertEdge edgeMetadata ->             insertNewEdge edgeMetadata
  DeleteEdge edgeMetadata ->             deleteEdge edgeMetadata
  MoveNode nodeId from to ->             moveNode nodeId to
  UpdateNodeText nodeId from to ->       updateNodeText nodeId to
  UpdateEdgeText edgeMetadata from to -> updateEdgeText edgeMetadata to
  UpdateTitle from to ->                 updateTitle to
  SetTitleValidity validity ->           setTitleValidity validity
  ConnectSubgraph nodeId old new ->      connectSubgraph graphId nodeId new

interpretEquationOperation :: EquationOperation -> SubMegagraph -> SubMegagraph
interpretEquationOperation = case _ of
  InsertPathEquation pathEquation -> insertPathEquation pathEquation
  DeletePathEquation pathEquation -> deletePathEquation pathEquation

interpretMappingOperation :: MappingOperation -> SubMegagraph -> SubMegagraph
interpretMappingOperation = case _ of
  InsertNodeMappingEdge sourceGraph targetGraph sourceNode targetNode ->
    insertNodeMappingEdge sourceNode targetNode
  DeleteNodeMappingEdge sourceGraph targetGraph sourceNode targetNode ->
    deleteNodeMappingEdge sourceNode targetNode
  InsertEdgeMappingEdge sourceGraph targetGraph sourceEdge targetEdge ->
    insertMappingEdge sourceEdge targetEdge
  DeleteEdgeMappingEdge sourceGraph targetGraph sourceEdge targetEdge ->
    deleteEdgeMappingEdge sourceEdge targetEdge

interpretSubMegagraphOperation :: SubMegagraphOperation -> SubMegagraph -> SubMegagraph
interpretSubMegagraphOperation = case _ of
  SubGraphOperation graphId graphOp ->
    _graphState graphId <<< _graph %~ interpretGraphOperation graphOp
  SubGraphEquationOperation graphId equationOp
    _graphState graphId <<< _graph %~ interpretEquationOperation equationOp
  SubMappingOperation mappingId mappingOp ->
    _mappingState mappingId <<< _mapping %~ interpretMappingOperation mappingOp

interpretUIOperation :: UIOperation -> SubMegagraph -> SubMegagraph
interpretUIOperation {op, target, historyUpdate, undoneUpdate} subMegagraph =
  let
    applySubMegagraphUpdate = \op' subMegagraph' -> foldl (flip interpretSubMegagraphOperation) subMegagraph' op'
    historyUpdater = case _ of
      Insert -> Array.cons op
      Pop -> Array.drop 1
      NoOp -> id
    updateComponentHistory target' = case target' of
      GraphComponent graphId ->
        (_graphState graphId <<< traverse <<< prop (SProxy :: SProxy "history") %~ historyUpdater)
        >>>
        (_graphState graphId <<< traverse <<< prop (SProxy :: SProxy "undone") %~ historyUpdater)
      MappingComponent mappingId ->
        (_mappingState mappingId <<< traverse <<< prop (SProxy :: SProxy "history" %~ historyUpdater))
        >>>
        (_mappingState mappingId <<< traverse <<< prop (SProxy :: SProxy "undone" %~ historyUpdater))
  in
    subMegagraph
    # applySubMegagraphUpdate op
    # updateComponentHistory target


-- | Interpret the operation and push it onto the history stack
doAppOperation :: AppOperation Unit -> AppState -> AppState
doAppOperation appOp appState =
  let
    AppOperation graphId op = appOp
    history = case Map.lookup graphId appState.history of
      Nothing -> []
      Just graphHistory -> graphHistory
    newHistory = filteredHistoryUpdate appOp history
  in
    appState { history = Map.insert graphId newHistory appState.history }
      # interpretAppOperation appOp

interpretAppOperation :: forall a.
                         AppOperation a -> (AppState -> AppState)
interpretAppOperation (AppOperation graphId uiUndoGraphOp) =
  let
    undoGraphOp =
      interpretUIOp uiUndoGraphOp
      -- Discard the leaf of the original op
      # map fst

    graphOp =
      interpretUndoOp undoGraphOp
      # map (\(Tuple undoUpdate uIUpdate) ->
               uIUpdate >>> undoUpdate)

    queryServerOp =
      interpretGraphOp graphOp
      # map \(Tuple graphUpdate uIUndoUpdate) ->
              uIUndoUpdate >>> (_graphData %~ graphUpdate)

    noOp = interpretQueryServerOpOnClient queryServerOp
           # map \(Tuple noUpdate allUpdates) -> noUpdate >>> allUpdates
  in
    noOp
    -- All sublangs have now been interpreted so we can grab the accumulated result
    # Run.extract

filteredHistoryUpdate :: AppOperation Unit
                           -> Array (AppOperation Unit)
                           -> Array (AppOperation Unit)
filteredHistoryUpdate appOp history =
  let
    AppOperation graphId op = appOp
  in
    case Run.peel op of
      Right _ -> history
      Left opV -> opV # Run.on _graphOp (\_ ->
        case Array.uncons history of
          Nothing -> [ appOp ]
          Just unconsHistory ->
            case collapseAppOperation appOp unconsHistory.head of
              Nothing -> Array.cons appOp history
              Just collapsedOp -> Array.cons collapsedOp unconsHistory.tail)
        (Run.default history)

collapseAppOperation :: forall a.
                        AppOperation a
                        -> AppOperation a
                        -> Maybe (AppOperation Unit)
collapseAppOperation (AppOperation graphId newOp) (AppOperation graphId' historyHeadOp) =
  if graphId /= graphId'
  then Nothing
  else
    case Tuple (Run.peel newOp) (Run.peel historyHeadOp) of
      Tuple (Left newOpV) (Left historyHeadOpV) ->
        newOpV # ((Run.default Nothing) # Run.on _graphOp (\newOpF ->
          historyHeadOpV # ((Run.default Nothing) # Run.on _graphOp (\historyHeadOpF ->
            case collapseGraphOpF newOpF historyHeadOpF of
              Nothing -> Nothing
              Just collapsed ->
                -- Call recursively to check that the whole of the ops can be
                -- collapsed together
                let
                  collapsedRun = collapsed.collapsedOp # Run.lift _graphOp
                in
                  collapseAppOperation (AppOperation graphId  collapsed.nextNext) (AppOperation graphId collapsed.prevNext)
                  <#> (\(AppOperation graphId'' op) -> AppOperation graphId'' (collapsedRun >>= const op))))))
      -- Base case for the recursion
      Tuple (Right nextLeaf) ( Right prevLeaf) ->
        Just $ AppOperation graphId $ pure unit
      _ ->
        Nothing


------
-- UndoOp

handleUndoOp :: forall a. UndoOpF a -> Tuple (AppState -> AppState) a
handleUndoOp = case _ of
  Undo graphId next ->
    Tuple (\appState -> case uncons =<< Map.lookup graphId appState.history of
            Nothing -> appState
            Just unconsHistory ->
              let
                AppOperation graphId lastOp = unconsHistory.head
                undoOp     = invertGraphOp lastOp
                newHistory = unconsHistory.tail
                newUndone  = case Map.lookup graphId appState.undone of
                  Nothing -> [ unconsHistory.head ]
                  Just undone -> cons unconsHistory.head undone
              in
                appState
                # interpretAppOperation (AppOperation graphId undoOp)
                # _{ history = Map.insert graphId newHistory appState.history
                   , undone  = Map.insert graphId newUndone  appState.undone
                   }
          )
          next

  Redo graphId next ->
    Tuple (\appState -> case uncons =<< Map.lookup graphId appState.undone of
              Nothing -> appState
              Just unconsUndone ->
                let
                  redoOp    = unconsUndone.head
                  newUndone = unconsUndone.tail
                in
                  appState
                  # interpretAppOperation redoOp
                  # _{ history = Map.update (Just <<< cons unconsUndone.head) graphId appState.history
                     , undone  = Map.insert graphId newUndone appState.undone
                     }
          )
          next

  ConsHistory graphId op next ->
    Tuple (\appState ->
            appState { history = Map.update (Just <<< cons op) graphId appState.history })
          next

  ConsUndone graphId op next ->
    Tuple (\appState ->
            appState { undone = Map.update (Just <<< cons op) graphId appState.undone })
          next

  SetHistory graphId history next ->
    Tuple (_history %~ Map.insert graphId history) next

  SetUndone graphId undone next ->
    Tuple (_undone %~ Map.insert graphId undone) next

interpretUndoOp :: forall r a.
                   Run (undoOp :: UNDOOP | r) a -> Run r (Tuple (AppState -> AppState) a)
interpretUndoOp =
  Run.runAccumPure
  (\accumulator ->
    Run.on _undoOp (Loop <<< lmap (\updater -> accumulator >>> updater) <<< handleUndoOp) Done)
  Tuple
  identity


------
-- UIOp

handleUIOp :: forall a. UIOpF a -> Tuple (AppState -> AppState) a
handleUIOp = case _ of
  MoveGraphOrigin graphId newGraphOrigin next ->
    Tuple (_graphData <<< _pane graphId <<< _origin .~ newGraphOrigin) next

  UpdateZoom graphId newZoom next ->
    Tuple (_graphData <<< _pane graphId <<< _zoom .~ newZoom) next

  InsertPane graphId next ->
    Tuple (insertPaneImpl graphId) next

  RemovePane graphId next ->
    Tuple (removePaneImpl graphId >>> (_graphData <<< _title graphId .~ Nothing)) next

  RescalePane graphId rect next ->
    Tuple (rescalePaneImpl graphId rect) next

interpretUIOp :: forall r a.
                 Run (uiOp :: UIOP | r) a -> Run r (Tuple (AppState -> AppState) a)
interpretUIOp =
  Run.runAccumPure
  (\accumulator ->
    Run.on _uiOp (Loop <<< lmap ((>>>) accumulator) <<< handleUIOp) Done)
  Tuple
  identity

removeGraphData :: GraphId -> AppState -> AppState
removeGraphData graphId appState =
  case selectGraphData graphId appState.graphData of
    Nothing -> appState
    Just graphToRemove ->
      let
        remover = encodeGraphDataAsGraphOp graphToRemove
                  # invertGraphOp
                  # AppOperation graphId
                  # interpretAppOperation
        appState' = remover appState
      in
        appState' { history = Map.delete graphId appState'.history
                  , undone  = Map.delete graphId appState'.undone
                  }
          # _graphData <<< _panes <<< at graphId .~ Nothing

removePaneImpl :: GraphId -> AppState -> AppState
removePaneImpl graphId =
  removeGraphData graphId
  >>>
  arrangePanes
  -- Focus on the rightmost pane
  >>> \appState -> case appState.graphData.panes
                        # Map.values >>> Array.fromFoldable
                        # Array.sortBy (comparing _.boundingRect.right)
                        # Array.head of
        Nothing -> appState
        Just nextPane -> appState { focusedPane = Just nextPane.graphId }
