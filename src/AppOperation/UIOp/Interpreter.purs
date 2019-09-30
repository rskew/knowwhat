module AppOperation.UIOp.Interpreter where

import Prelude

import AppOperation.UIOp (UIOP, UIOpF(..), _uiOp)
import AppState (AppState, _graphData)
import Core (_pane, _origin, _zoom)
import Data.Bifunctor (lmap)
import Data.Lens ((.~))
import Data.Tuple (Tuple(..))
import Run (Run, Step(..))
import Run as Run
import UI.Panes (insertPaneImpl, rescalePaneImpl, rescaleWindowImpl, removePaneImpl)


------
-- Interpreter

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
