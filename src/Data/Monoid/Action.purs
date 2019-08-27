module Data.Monoid.Action where

-- | directly from https://hackage.haskell.org/package/monoid-extras-0.5/docs/src/Data.Monoid.Action.html
class Action m s where
  -- | Convert a value of type @m@ to an action on @s@ values.
  act :: m -> s -> s
