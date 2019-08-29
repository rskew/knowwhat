module Workflow.UIGraph.ForeignUIGraph where

import Prelude

import Workflow.Core (EdgeId)
import Point2D
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity(..))
import Data.List.NonEmpty (NonEmptyList)
import Data.Map (Map)
import Data.Map as Map
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.UUID (UUID, parseUUID)
import Data.UUID as UUID
import Foreign (ForeignError, renderForeignError)
import Foreign.Class (class Encode, class Decode)
import Foreign.Generic (defaultOptions, genericDecodeJSON, genericEncodeJSON, genericEncode, genericDecode)
import Foreign.Generic.Types (SumEncoding)
import Foreign.Object (Object)
import Foreign.Object as Object


genericEncodeOpts ::
      { unwrapSingleConstructors :: Boolean
      , fieldTransform :: String -> String
      , sumEncoding :: SumEncoding
      , unwrapSingleArguments :: Boolean
      }
genericEncodeOpts = defaultOptions { unwrapSingleConstructors = true }

type ForeignNodeId = String

newtype ForeignEdgeId =
  ForeignEdgeId
  { source :: ForeignNodeId
  , target :: ForeignNodeId
  }
derive instance genericForeignEdgeId :: Generic ForeignEdgeId _
instance encodeForeignEdgeId :: Encode ForeignEdgeId where
  encode = genericEncode genericEncodeOpts
instance decodeForeignEdgeId :: Decode ForeignEdgeId where
  decode = genericDecode genericEncodeOpts

newtype ForeignUIEdge =
  ForeignUIEdge
  { id :: ForeignEdgeId
  , text :: String
  , isValid :: Boolean
  }
derive instance genericForeignUIEdge :: Generic ForeignUIEdge _
instance encodeForeignUIEdge :: Encode ForeignUIEdge where
  encode = genericEncode genericEncodeOpts
instance decodeForeignUIEdge :: Decode ForeignUIEdge where
  decode = genericDecode genericEncodeOpts

newtype ForeignUINode =
  ForeignUINode
  { id :: ForeignNodeId
  , children :: Object ForeignUIEdge
  , parents :: Object ForeignUIEdge
  , subgraph :: ForeignUIGraph
  , position :: Point2D
  , text :: String
  , isValid :: Boolean
  }
derive instance genericForeignUINode :: Generic ForeignUINode _
instance encodeForeignUINode :: Encode ForeignUINode where
  encode = genericEncode genericEncodeOpts
instance decodeForeignUINode :: Decode ForeignUINode where
  decode = genericDecode genericEncodeOpts

data ForeignFocus
  = ForeignFocusNode ForeignNodeId
  | ForeignFocusEdge ForeignEdgeId (Array ForeignEdgeId)
  | ForeignNoFocus
derive instance genericForeignFocus :: Generic ForeignFocus _
instance encodeForeignFocus :: Encode ForeignFocus where
  encode = genericEncode genericEncodeOpts
instance decodeForeignFocus :: Decode ForeignFocus where
  decode = genericDecode genericEncodeOpts

newtype ForeignUIGraph =
  ForeignUIGraph
  { nodes :: Object ForeignUINode
  , isDual :: Boolean
  , focus :: ForeignFocus
  , highlighted :: Array ForeignNodeId
  }
derive instance genericForeignUIGraph :: Generic ForeignUIGraph _
instance encodeForeignUIGraph :: Encode ForeignUIGraph where
  encode x = genericEncode genericEncodeOpts x
instance decodeForeignUIGraph :: Decode ForeignUIGraph where
  decode x = genericDecode genericEncodeOpts x

type ForeignUIGraphMeta = { version :: String
                          }

newtype ForeignUIGraphWithMeta =
  ForeignUIGraphWithMeta
  { graph :: ForeignUIGraph
  , metadata :: ForeignUIGraphMeta
  }
derive instance genericForeignUIGraphWithMeta :: Generic ForeignUIGraphWithMeta _
instance encodeForeignUIGraphWithMeta :: Encode ForeignUIGraphWithMeta where
  encode x = genericEncode genericEncodeOpts x
instance decodeForeignUIGraphWithMeta :: Decode ForeignUIGraphWithMeta where
  decode x = genericDecode genericEncodeOpts x
