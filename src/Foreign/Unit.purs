module Foreign.Unit where

import Prelude

import Data.Generic.Rep (class Generic)
import Foreign.Class (class Encode, class Decode)
import Foreign.Generic (genericEncode, genericDecode, defaultOptions)

data ForeignUnit = ForeignUnit
derive instance genericForeignUnit :: Generic ForeignUnit _
instance encodeForeignUnit :: Encode ForeignUnit where
  encode x = genericEncode defaultOptions x
instance decodeForeignUnit :: Decode ForeignUnit where
  decode x = genericDecode defaultOptions x

toForeignUnit :: Unit -> ForeignUnit
toForeignUnit = const ForeignUnit

fromForeignUnit :: ForeignUnit -> Unit
fromForeignUnit = const unit
