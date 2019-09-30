module AppOperation.Interpreter where

import Prelude

import AppOperation (AppOperation(..))
import AppOperation.GraphOp (GRAPHOP, _graphOp, collapseGraphOpF, encodeGraphDataAsGraphOp, handleGraphOp, invertGraphOp)
import AppOperation.UIOp (encodeGraphViewsAsUIOp)
import AppOperation.UIOp.Interpreter (interpretUIOp)
import AppOperation.UndoOp (UndoOpF(..), _undoOp, UNDOOP)
import AppState (AppState, _graphData, _synth)
import Core (GraphId, GraphData, selectGraphData, _panes)
import Data.Array (cons, uncons)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Lens (Lens', over, (^.), (.~))
import Data.Lens.At (at)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap, unwrap)
import Data.NonEmpty (NonEmpty(..))
import Data.NonEmpty as NonEmpty
import Data.Symbol (SProxy, class IsSymbol)
import Data.Traversable (foldl)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Console as Console
import Prim.Row (class Cons)
import Run (Run, FProxy, Step(..))
import Run as Run
import Synth (SynthParams)
import Synth.SynthOp (encodeSynthParamsAsSynthOp, handleGraphOpAsSynthUpdate, interpretSynthOp)


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
    interpretAppOperation op >=> historyUpdater

interpretAppOperation :: forall a.
                         AppOperation a -> (AppState -> Effect AppState )
interpretAppOperation (AppOperation synthUIUndoGraphOp) =
  let
    uiUndoGraphOp =
      interpretSynthOp synthUIUndoGraphOp
      -- Discard the leaf of the original op
      # map fst
      -- lift the synth update function to an AppState update function
      # map (overM _synth)

    undoGraphOp =
      interpretUIOp uiUndoGraphOp
      # map (\(Tuple uiUpdate synthUpdate) ->
               synthUpdate >=> (uiUpdate >>> pure))

    graphOp =
      interpretUndoOp undoGraphOp
      # map (\(Tuple undoUpdate synthUIUpdate) ->
               synthUIUpdate >=> undoUpdate)

    noOp =
      interpretGraphOpAsGraphAndSynthUpdate graphOp
      # map \(Tuple graphSynthUpdate synthUIUndoUpdate) ->
              synthUIUndoUpdate >=> graphSynthUpdate
  in
    noOp
    -- All sublangs have now been interpreted so we can grab the accumulated result
    # Run.extract

interpretGraphOpAsGraphAndSynthUpdate :: forall r a.
                                         Run (graphOp :: GRAPHOP | r) a
                                         -> Run r (Tuple (AppState -> Effect AppState ) a)
interpretGraphOpAsGraphAndSynthUpdate =
  interpretOpMulti
  _graphOp
  (NonEmpty
    (handleGraphOpAsSynthUpdate >>> lmap (overM _synth))
    [ handleGraphOp >>> lmap (over _graphData >>> compose pure) ])
  (>=>)
  pure


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

handleUndoOp :: forall a. UndoOpF a -> Tuple (AppState -> Effect AppState) a
handleUndoOp = case _ of
  Undo graphId next ->
    Tuple (\appState -> case uncons =<< Map.lookup graphId appState.history of
            Nothing -> pure appState
            Just unconsHistory ->
              let
                undoOp      = unconsHistory.head # unwrap >>> invertGraphOp >>> wrap
                newHistory  = unconsHistory.tail
                newUndone   = case Map.lookup graphId appState.undone of
                  Nothing -> [ unconsHistory.head ]
                  Just undone -> cons unconsHistory.head undone
              in do
                undoneState <- appState # interpretAppOperation undoOp
                pure $ undoneState
                         { history   = Map.insert graphId newHistory appState.history
                         , undone    = Map.insert graphId newUndone  appState.undone
                         }
          )
          next

  Redo graphId next ->
    Tuple (\appState -> case uncons =<< Map.lookup graphId appState.undone of
              Nothing -> pure appState
              Just unconsUndone ->
                let
                  redoOp      = unconsUndone.head
                  newUndone   = unconsUndone.tail
                in do
                  redoneState <- appState # interpretAppOperation redoOp
                  pure $ redoneState
                           { history   = Map.update (Just <<< cons unconsUndone.head) graphId appState.history
                           , undone    = Map.insert graphId newUndone appState.undone
                           }
          )
          next

interpretUndoOp :: forall r a.
                   Run (undoOp :: UNDOOP | r) a -> Run r (Tuple (AppState -> Effect AppState) a)
interpretUndoOp =
  Run.runAccumPure
  (\accumulator ->
    Run.on _undoOp (Loop <<< lmap (\updater -> accumulator >>= const updater) <<< handleUndoOp) Done)
  Tuple
  pure


------
-- Encoding AppState for serialisation/deserialisation

encodeGraphDataAsAppOperation :: GraphData -> SynthParams -> AppOperation Unit
encodeGraphDataAsAppOperation graphData synthParams =
  AppOperation do
    encodeGraphDataAsGraphOp graphData
    encodeGraphViewsAsUIOp graphData.panes
    encodeSynthParamsAsSynthOp synthParams

removeGraphData :: GraphId -> AppState -> Effect AppState
removeGraphData graphId appState =
  case selectGraphData graphId appState.graphData of
    Nothing -> pure appState
    Just graphToRemove -> do
      let remover = encodeGraphDataAsGraphOp graphToRemove
                    # invertGraphOp
                    # wrap
                    # interpretAppOperation
      appState' <- remover appState
      pure $ appState' { history = Map.delete graphId appState'.history
                       , undone  = Map.delete graphId appState'.undone
                       }
             # _graphData <<< _panes <<< at graphId .~ Nothing


------
-- Utilities

overM :: forall s a' m. Monad m => Lens' s a' -> (a' -> m a') -> s -> m s
overM lens_ f val = do
  newSubVal <- f $ val ^. lens_
  pure $ (val # (lens_ .~ newSubVal))

interpretOpMulti :: forall sym f a b r r'.
                    Cons sym (FProxy f) r' r
                    => IsSymbol sym
                    => SProxy sym
                    -> NonEmpty Array (f (Run r a) -> Tuple b (Run r a))
                    -> (b -> b -> b)
                    -> b
                    -> Run r a
                    -> Run r' (Tuple b a)
interpretOpMulti sym handlers joiner =
  Run.runAccumPure
  (\accumulator ->
    Run.on
    sym
    (\symOp ->
      let
        appliedHandlers = handlers <#> \handler -> handler symOp
        handlerFuncs = fst <$> appliedHandlers
        next = snd $ NonEmpty.head appliedHandlers
        handlerAccumulator = foldl joiner (NonEmpty.head handlerFuncs) (NonEmpty.tail handlerFuncs)
        combinedHandler = joiner accumulator handlerAccumulator
      in
       Loop $ Tuple combinedHandler next)
    Done)
  (\accumulator a -> Tuple accumulator a)
