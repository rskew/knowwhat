module Undoable where

import Data.List (List(..))
import Data.List as List
import Data.Group (class Group, ginverse)
import Data.Maybe (Maybe(..))
import Data.Monoid.Action (class Action, act)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Symbol (SProxy(..))


type Undoable val action
  = { current :: val
    , history :: List action
    , undone :: List action
    }

_current :: forall v a. Lens' (Undoable v a) v
_current = prop (SProxy :: SProxy "current")

initUndoState :: forall val action. val -> Undoable val action
initUndoState val = { current : val, history : Nil, undone : Nil }

-- | Don't reset the record of undone actions when doing a new action.
-- | This means you can undo some actions, do some more actions,
-- | Then redo the old undone actions.
-- |
-- | This may not be good or intuitive and may change in future versions :upside_down_face:
doo :: forall val action. Action action val => Group action =>
       action -> Undoable val action -> Undoable val action
doo action undoState =
  { current : act action undoState.current
  , history : Cons action undoState.history
  , undone : undoState.undone
  }

undo :: forall val action. Action action val => Group action =>
        Undoable val action -> Undoable val action
undo undoState =
  case List.uncons undoState.history of
    Nothing -> undoState
    Just history ->
      let
        lastAction = history.head
        restHistory = history.tail
      in
        { current : act (ginverse lastAction) undoState.current
        , history : restHistory
        , undone : Cons lastAction undoState.undone
        }

redo :: forall val action. Action action val => Group action =>
        Undoable val action -> Undoable val action
redo undoState =
  case List.uncons undoState.undone of
    Nothing -> undoState
    Just undone ->
      let
        lastUndoneAction = undone.head
        restUndone = undone.tail
      in
        { current : act lastUndoneAction undoState.current
        , history : Cons lastUndoneAction undoState.history
        , undone : restUndone
        }
