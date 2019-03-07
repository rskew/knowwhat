module Undoable where

---- things that preserve their change history including undos
--class Group a <= Undoable a where
--  undoLastOp :: a -> a
--  redoLastUndo :: a -> a
--
--
--data OpOrUndo op = op | Undo | Redo
--
---- Trivial implementation of just recording that the operations
---- happened. We trust the operation list to be interpreted
---- properly :/
--instance undoableList :: Undoable (List (OpOrUndo op)) where
--  undoLastOp xs = Undo : xs
--  redoLastUndo xs = Redo : xs


