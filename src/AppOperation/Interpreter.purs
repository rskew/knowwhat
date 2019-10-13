module AppOperation.Interpreter where

import Prelude

import AppOperation (AppOperation(..))
import AppOperation.GraphOp (_graphOp, collapseGraphOpF, encodeGraphDataAsGraphOp, interpretGraphOp, invertGraphOp)
import AppOperation.UIOp (encodeGraphViewsAsUIOp)
import AppOperation.UIOp.Interpreter (interpretUIOp)
import AppOperation.UndoOp (UndoOpF(..), _undoOp, UNDOOP)
import AppState (AppState, _graphData)
import Core (GraphId, GraphData, selectGraphData, _panes)
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
import Effect.Console as Console
import Run (Run, Step(..))
import Run as Run


-- | Interpret the operation and push it onto the history stack
doAppOperation :: GraphId -> AppOperation Unit -> (AppState -> Effect AppState)
doAppOperation graphId op =
  let
    historyUpdater = \appState ->
      let
        history = case Map.lookup graphId appState.history of
          Nothing -> []
          Just graphHistory -> graphHistory
        newHistory = filteredHistoryUpdate op history
      in do
        -- TODO: clean up console logs
        Console.log ""
        Console.log $ "New history: " <> show (show <$> newHistory)
        Console.log ""
        pure $ appState { history = Map.insert graphId newHistory appState.history }
  in
    interpretAppOperation op >>> historyUpdater

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
-- Encoding AppState for serialisation/deserialisation

encodeGraphDataAsAppOperation :: GraphData -> AppOperation Unit
encodeGraphDataAsAppOperation graphData =
  AppOperation do
    encodeGraphDataAsGraphOp graphData
    encodeGraphViewsAsUIOp graphData.panes

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
