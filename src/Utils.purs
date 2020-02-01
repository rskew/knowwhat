module Utils where

import Data.Tuple


tupleApply :: forall a b c. (a -> b -> c) -> Tuple a b -> c
tupleApply f (Tuple a b) = f a b
