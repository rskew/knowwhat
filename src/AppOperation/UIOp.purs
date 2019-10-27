module AppOperation.UIOp where

import Prelude
import Core (GraphId, PageSpacePoint2D, GraphView)

import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.UUID as UUID
import Foreign (Foreign)
import Foreign.Class (class Encode, class Decode)
import Foreign.Generic (genericEncode, genericDecode, defaultOptions)
import Foreign.Utils (parseUUIDEither, toExceptT)
import Run (Run, FProxy)
import Run as Run
import Web.HTML.HTMLElement as WHE


------
-- UIOp DSL for UI interactions that are not undoable, but are streamable

data UIOpF next
  = MoveGraphOrigin GraphId PageSpacePoint2D next
  | UpdateZoom      GraphId Number           next
  | InsertPane      GraphId                  next
  | RemovePane      GraphId                  next
  | RescalePane     GraphId WHE.DOMRect      next

derive instance functorUIOpF :: Functor UIOpF

type UIOP = FProxy UIOpF

_uiOp :: SProxy "uiOp"
_uiOp = SProxy

showUIOp :: forall a. UIOpF a -> Tuple String a
showUIOp = case _ of
  MoveGraphOrigin graphId position next -> Tuple ("MoveGraphOrigin " <> show graphId <> " to: " <> show position) next
  UpdateZoom      graphId zoom     next -> Tuple ("UpdateZoom "      <> show graphId <> " to: " <> show zoom)     next
  InsertPane      graphId          next -> Tuple ("InsertPane "      <> show graphId)                             next
  RemovePane      graphId          next -> Tuple ("DeletePane "      <> show graphId)                             next
  RescalePane     graphId rect     next -> Tuple ("RescalePane "     <> show graphId <> " to: " <> show rect)     next


------
-- Interface

moveGraphOrigin :: forall r. GraphId -> PageSpacePoint2D -> Run (uiOp :: UIOP | r) Unit
moveGraphOrigin graphId newPosition = Run.lift _uiOp $ MoveGraphOrigin graphId newPosition unit

updateZoom :: forall r. GraphId -> Number -> Run (uiOp :: UIOP | r) Unit
updateZoom graphId newZoom = Run.lift _uiOp $ UpdateZoom graphId newZoom unit

insertPane :: forall r. GraphId -> Run (uiOp :: UIOP | r) Unit
insertPane graphId = Run.lift _uiOp $ InsertPane graphId unit

removePane :: forall r. GraphId -> Run (uiOp :: UIOP | r) Unit
removePane graphId = Run.lift _uiOp $ RemovePane graphId unit

rescalePane :: forall r. GraphId -> WHE.DOMRect -> Run (uiOp :: UIOP | r) Unit
rescalePane graphId rect = Run.lift _uiOp $ RescalePane graphId rect unit

encodeGraphViewAsUIOp :: forall r. GraphView -> Run (uiOp :: UIOP | r) Unit
encodeGraphViewAsUIOp pane = do
  insertPane pane.graphId
  moveGraphOrigin pane.graphId pane.origin
  updateZoom pane.graphId pane.zoom

encodeGraphViewsAsUIOp :: forall r. Map GraphId GraphView -> Run (uiOp :: UIOP | r) Unit
encodeGraphViewsAsUIOp panes =
  let
    maybeOps = (Array.fromFoldable $ Map.keys panes)
      <#> \graphId -> do
        pane  <- Map.lookup graphId panes
        pure $ encodeGraphViewAsUIOp pane
    opFuncs = const <$> Array.catMaybes maybeOps
  in
    foldl bind (pure unit) opFuncs


------
-- Serialisation/deserialisation

instance decodeUIOpF :: Decode (UIOpF Unit) where
  decode x = x # genericDecode defaultOptions >>= toExceptT <<< fromForeignUIOpF

data ForeignUIOpF
  = ForeignMoveGraphOrigin String PageSpacePoint2D
  | ForeignUpdateZoom      String Number
  | ForeignInsertPane      String
  | ForeignRemovePane      String
  | ForeignRescalePane     String WHE.DOMRect

derive instance genericForeignUIOpF :: Generic ForeignUIOpF _

instance encodeForeignUIOpF :: Encode ForeignUIOpF where
  encode x = x # genericEncode defaultOptions

instance decodeForeignUIOpF :: Decode ForeignUIOpF where
  decode x = x # genericDecode defaultOptions

toForeignUIOpF :: forall a. UIOpF a -> Tuple Foreign a
toForeignUIOpF = lmap (genericEncode defaultOptions) <<< case _ of
  MoveGraphOrigin graphId point next ->
    Tuple (ForeignMoveGraphOrigin (UUID.toString graphId) point) next
  UpdateZoom graphId zoom next ->
    Tuple (ForeignUpdateZoom (UUID.toString graphId) zoom) next
  InsertPane graphId next ->
    Tuple (ForeignInsertPane (UUID.toString graphId)) next
  RemovePane graphId next ->
    Tuple (ForeignRemovePane (UUID.toString graphId)) next
  RescalePane graphId rect next ->
    Tuple (ForeignRescalePane (UUID.toString graphId) rect) next

fromForeignUIOpF :: ForeignUIOpF -> Either String (UIOpF Unit)
fromForeignUIOpF = case _ of
  ForeignMoveGraphOrigin graphIdStr point -> do
    graphId <- parseUUIDEither graphIdStr
    pure $ MoveGraphOrigin graphId point unit
  ForeignUpdateZoom graphIdStr zoom -> do
    graphId <- parseUUIDEither graphIdStr
    pure $ UpdateZoom graphId zoom unit
  ForeignInsertPane graphIdStr -> do
    graphId <- parseUUIDEither graphIdStr
    pure $ InsertPane graphId unit
  ForeignRemovePane graphIdStr -> do
    graphId <- parseUUIDEither graphIdStr
    pure $ RemovePane graphId unit
  ForeignRescalePane graphIdStr rect -> do
    graphId <- parseUUIDEither graphIdStr
    pure $ RescalePane graphId rect unit
