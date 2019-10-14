module AppOperation.Interpreter where

import Prelude

import Affjax as AX
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import AppConfig (operationPostURL)
import AppOperation (AppOperation(..))
import AppOperation.GraphOp (_graphOp, collapseGraphOpF, encodeGraphDataAsGraphOp, interpretGraphOp, invertGraphOp)
import AppOperation.UIOp (UIOpF(..), _uiOp, UIOP)
import AppOperation.UndoOp (UndoOpF(..), _undoOp, UNDOOP)
import AppState (AppState, _graphData)
import Core (GraphId, selectGraphData, _panes, _pane, _origin, _zoom)
import Data.Array (cons, uncons)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Lens ((.~), (%~))
import Data.Lens.At (at)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap, unwrap)
import Data.Tuple (Tuple(..), fst)
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class.Console as Console
import Foreign.Generic (encodeJSON)
import Run (Run, Step(..))
import Run as Run
import UI.Panes (arrangePanes, insertPaneImpl, rescalePaneImpl, rescaleWindowImpl)

-- | Interpret the operation and push it onto the history stack
doAppOperation :: GraphId -> AppOperation Unit -> AppState -> Effect AppState
doAppOperation graphId op appState =
  let
    history = case Map.lookup graphId appState.history of
      Nothing -> []
      Just graphHistory -> graphHistory
    newHistory = filteredHistoryUpdate op history
  in do
    _ <- launchAff do
      res <- AX.post ResponseFormat.string operationPostURL (RequestBody.string $ encodeJSON op)
      case res.body of
        Left err -> Console.log $ "POST /operation response failed to decode: " <> AX.printResponseFormatError err
        Right response -> Console.log "Operation POSTed to server successfully"
    pure $ appState { history = Map.insert graphId newHistory appState.history }
           # interpretAppOperation op

interpretAppOperation :: forall a.
                         AppOperation a -> (AppState -> AppState)
interpretAppOperation (AppOperation uiUndoGraphOp) =
  let
    undoGraphOp =
      interpretUIOp uiUndoGraphOp
      -- Discard the leaf of the original op
      # map fst

    graphOp =
      interpretUndoOp undoGraphOp
      # map (\(Tuple undoUpdate uIUpdate) ->
               uIUpdate >>> undoUpdate)

    noOp =
      interpretGraphOp graphOp
      # map \(Tuple graphUpdate uIUndoUpdate) ->
              uIUndoUpdate >>> (_graphData %~ graphUpdate)
  in
    noOp
    -- All sublangs have now been interpreted so we can grab the accumulated result
    # Run.extract

filteredHistoryUpdate :: AppOperation Unit
                           -> Array (AppOperation Unit)
                           -> Array (AppOperation Unit)
filteredHistoryUpdate op history =
  case Run.peel (unwrap op) of
    Right _ -> history
    Left opV -> opV # Run.on _graphOp (\_ ->
      case Array.uncons history of
        Nothing -> [ op ]
        Just unconsHistory ->
          case collapseAppOperation op unconsHistory.head of
            Nothing -> Array.cons op history
            Just collapsedOp -> Array.cons collapsedOp unconsHistory.tail)
      (Run.default history)

collapseAppOperation :: forall a.
                        AppOperation a
                        -> AppOperation a
                        -> Maybe (AppOperation Unit)
collapseAppOperation (AppOperation newOp) (AppOperation historyHeadOp) =
  map wrap
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
                collapseAppOperation (wrap collapsed.nextNext) (wrap collapsed.prevNext)
                <#> (\op -> collapsedRun >>= const (unwrap op))))))
    -- Base case for the recursion
    Tuple (Right nextLeaf) ( Right prevLeaf) ->
      Just $ pure unit
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
                undoOp     = unconsHistory.head # unwrap >>> invertGraphOp >>> wrap
                newHistory = unconsHistory.tail
                newUndone  = case Map.lookup graphId appState.undone of
                  Nothing -> [ unconsHistory.head ]
                  Just undone -> cons unconsHistory.head undone
              in
                appState
                # interpretAppOperation undoOp
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
    Tuple (removePaneImpl graphId) next

  RescalePane graphId rect next ->
    Tuple (rescalePaneImpl graphId rect) next

  RescaleWindow rect next ->
    Tuple (rescaleWindowImpl rect) next

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
                  # wrap
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
