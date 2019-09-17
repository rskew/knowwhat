module AppState.Foreign where

import Prelude

import AppState (UninitializedAppState, AppState, AppOperation, DrawingEdge, HoveredElementId(..), GraphSpacePos(..), PageSpacePos(..), Shape, _graph, _drawingEdges, _graphOrigin, _hoveredElementId, _boundingRect, _zoom)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Data.Array ((!!))
import Data.Bifunctor (lmap)
import Data.DateTime.Instant (Instant, instant, unInstant)
import Data.Either (Either, note)
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity(..))
import Data.Lens ((^.), (.~), (%~))
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe)
import Data.String (Pattern(..), split)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Data.UUID (parseUUID)
import Data.UUID as UUID
import Data.Undoable (Undoable, mapActions, _current)
import Foreign (Foreign, ForeignError, renderForeignError)
import Foreign.Class (class Encode, class Decode)
import Foreign.Generic (genericDecodeJSON, genericEncodeJSON, genericEncode, genericDecode, defaultOptions)
import Foreign.Object (Object)
import Foreign.Unit (ForeignUnit, toForeignUnit, fromForeignUnit)
import Point2D (Point2D)
import Web.HTML.HTMLElement as WHE
import Workflow.Core (EdgeId)
import Workflow.UIGraph.Types (UIGraph, ForeignNodeId, ForeignEdgeId(..), toForeignMap, fromForeignMap, parseUUIDEither)


-- | A whole bunch of boilerplate to use generic JSON serialisation/deserialisation


newtype ForeignDrawingEdge =
  ForeignDrawingEdge
  { source :: ForeignNodeId
  , pos :: ForeignPos
  }
derive instance genericForeignDrawingEdge :: Generic ForeignDrawingEdge _
instance encodeForeignDrawingEdge :: Encode ForeignDrawingEdge where
  encode x = genericEncode defaultOptions x
instance decodeForeignDrawingEdge :: Decode ForeignDrawingEdge where
  decode x = genericDecode defaultOptions x

data ForeignGraphElementId
  = ForeignNodeHaloId ForeignNodeId
  | ForeignNodeBorderId ForeignNodeId
  | ForeignEdgeBorderId ForeignEdgeId
derive instance genericForeignGraphElementId :: Generic ForeignGraphElementId _
instance encodeForeignGraphElementId :: Encode ForeignGraphElementId where
  encode x = genericEncode defaultOptions x
instance decodeForeignGraphElementId :: Decode ForeignGraphElementId where
  decode x = genericDecode defaultOptions x

newtype ForeignShape = ForeignShape Shape
derive instance genericForeignShape :: Generic ForeignShape _
instance encodeForeignShape :: Encode ForeignShape where
  encode :: ForeignShape -> Foreign
  encode x = genericEncode defaultOptions x
instance decodeForeignShape :: Decode ForeignShape where
  decode x = genericDecode defaultOptions x

newtype ForeignPos = ForeignPos Point2D
derive instance genericForeignPos :: Generic ForeignPos _
instance encodeForeignPos :: Encode ForeignPos where
  encode x = genericEncode defaultOptions x
instance decodeForeignPos :: Decode ForeignPos where
  decode x = genericDecode defaultOptions x

type ForeignAppStateInner =
  { graph :: UIGraph
  , drawingEdges :: Object ForeignDrawingEdge
  , hoveredElementId :: Maybe ForeignGraphElementId
  , boundingRect :: WHE.DOMRect
  , graphOrigin :: ForeignPos
  , zoom :: Number
  }

type UndoableForeignAppState = Undoable ForeignAppStateInner (AppOperation ForeignUnit)

newtype ForeignAppState =
  ForeignAppState UndoableForeignAppState
derive instance genericForeignAppState :: Generic ForeignAppState _
instance encodeForeignAppState :: Encode ForeignAppState where
  encode x = genericEncode defaultOptions x
instance decodeForeignAppState :: Decode ForeignAppState where
  decode x = genericDecode defaultOptions x

type AppStateMeta = { version :: String
                    , timestamp :: Instant
                    }

newtype ForeignAppStateMeta =
  ForeignAppStateMeta
  { version :: String
  , timestamp :: Number
  }
derive instance genericForeignAppStateMeta :: Generic ForeignAppStateMeta _
instance encodeForeignAppStateMeta :: Encode ForeignAppStateMeta where
  encode x = genericEncode defaultOptions x
instance decodeForeignAppStateMeta :: Decode ForeignAppStateMeta where
  decode x = genericDecode defaultOptions x

type UninitializedAppStateWithMeta =
  { uninitializedAppState :: UninitializedAppState
  , metadata :: AppStateMeta
  }

newtype ForeignAppStateWithMeta =
  ForeignAppStateWithMeta
  { appState :: ForeignAppState
  , metadata :: ForeignAppStateMeta
  }
derive instance genericForeignAppStateWithMeta :: Generic ForeignAppStateWithMeta _
instance encodeForeignAppStateWithMeta :: Encode ForeignAppStateWithMeta where
  encode x = genericEncode defaultOptions x
instance decodeForeignAppStateWithMeta :: Decode ForeignAppStateWithMeta where
  decode x = genericDecode defaultOptions x

------
-- Serialisation

toForeignDrawingEdge :: DrawingEdge -> ForeignDrawingEdge
toForeignDrawingEdge drawingEdge =
  let
    GraphSpacePos drawingEdgePos = drawingEdge.pos
  in
   ForeignDrawingEdge $
    drawingEdge { source = UUID.toString drawingEdge.source
                , pos = ForeignPos drawingEdgePos
                }

toForeignGraphElementId :: HoveredElementId -> ForeignGraphElementId
toForeignGraphElementId (NodeHaloId nodeId) = ForeignNodeHaloId $ UUID.toString nodeId
toForeignGraphElementId (NodeBorderId nodeId) = ForeignNodeBorderId $ UUID.toString nodeId
toForeignGraphElementId (EdgeBorderId edgeId) =
  ForeignEdgeBorderId $ ForeignEdgeId
  { source : UUID.toString edgeId.source
  , target : UUID.toString edgeId.target
  }

edgeIdToString :: EdgeId -> String
edgeIdToString edgeId = UUID.toString edgeId.source
                        <> " "
                        <> UUID.toString edgeId.target

toForeignAppState :: AppState -> ForeignAppState
toForeignAppState state =
  let
    foreignHistory = mapActions (map toForeignUnit) state
    foreignDrawingEdges = toForeignMap toForeignDrawingEdge UUID.toString $ state ^. _drawingEdges
    foreignHoveredElementId = toForeignGraphElementId <$> (state ^. _hoveredElementId)
    PageSpacePos graphOrigin = state ^. _graphOrigin
    foreignGraphOrigin = ForeignPos graphOrigin
  in
    ForeignAppState
    $ foreignHistory
    # _current .~ { graph : state ^. _graph
                  , drawingEdges : foreignDrawingEdges
                  , hoveredElementId : foreignHoveredElementId
                  , graphOrigin : foreignGraphOrigin
                  , boundingRect : state ^. _boundingRect
                  , zoom : state ^. _zoom
                  }

toForeignAppStateMeta :: AppStateMeta -> ForeignAppStateMeta
toForeignAppStateMeta metadata =
  let
    Milliseconds millis = unInstant metadata.timestamp
  in
    ForeignAppStateMeta $
    metadata { timestamp = millis }

appStateToJSON :: AppState -> AppStateMeta -> String
appStateToJSON appState metadata =
  let
    foreignAppStateWithMeta =
      ForeignAppStateWithMeta
      { appState : toForeignAppState appState
      , metadata : toForeignAppStateMeta metadata
      }
    appState' = appState
  in
    genericEncodeJSON defaultOptions foreignAppStateWithMeta


------
-- Deserialisation

fromForeignDrawingEdge :: ForeignDrawingEdge -> Either String DrawingEdge
fromForeignDrawingEdge (ForeignDrawingEdge foreignDrawingEdge) =
  let
    ForeignPos foreignDrawingEdgePos = foreignDrawingEdge.pos
    drawingEdgePos = GraphSpacePos foreignDrawingEdgePos
  in do
    source <- parseUUIDEither foreignDrawingEdge.source
    pure $ { source : source
           , pos : drawingEdgePos
           }

fromForeignGraphElementId :: ForeignGraphElementId -> Either String HoveredElementId
fromForeignGraphElementId (ForeignNodeHaloId foreignNodeId) = NodeHaloId <$> parseUUIDEither foreignNodeId
fromForeignGraphElementId (ForeignNodeBorderId foreignNodeId) = NodeBorderId <$> parseUUIDEither foreignNodeId
fromForeignGraphElementId (ForeignEdgeBorderId (ForeignEdgeId foreignEdgeId)) = do
  source <- parseUUIDEither foreignEdgeId.source
  target <- parseUUIDEither foreignEdgeId.target
  pure $ EdgeBorderId { source : source, target : target }

parseEdgeIdEither :: String -> Either String EdgeId
parseEdgeIdEither edgeIdStr =
  let
    edgeIdStrs = split (Pattern " ") edgeIdStr
  in
  note "Failed to parse EdgeId" do
    sourceId <- edgeIdStrs !! 0 >>= parseUUID
    targetId <- edgeIdStrs !! 1 >>= parseUUID
    pure { source : sourceId, target : targetId }

fromForeignAppState :: ForeignAppState -> Either String UninitializedAppState
fromForeignAppState (ForeignAppState foreignState) = do
  let undoableHistory = mapActions (map fromForeignUnit) foreignState
  drawingEdges <- fromForeignMap fromForeignDrawingEdge parseUUIDEither (foreignState ^. _current).drawingEdges
  hoveredElementId <- traverse fromForeignGraphElementId (foreignState ^. _current).hoveredElementId
  let ForeignPos foreignGraphOrigin = (foreignState ^. _current).graphOrigin
  let graphOrigin = PageSpacePos foreignGraphOrigin
  pure $
    undoableHistory
    # _current %~ _{ drawingEdges = drawingEdges
                   , hoveredElementId = hoveredElementId
                   , graphOrigin = graphOrigin
                   }

fromForeignAppStateMeta :: ForeignAppStateMeta -> Either String AppStateMeta
fromForeignAppStateMeta (ForeignAppStateMeta foreignMeta) = do
  timestamp <- note "Failed to convert timestamp from foreign"
               $ instant $ Milliseconds foreignMeta.timestamp
  pure $ foreignMeta { timestamp = timestamp }

fromForeignAppStateWithMeta :: ForeignAppStateWithMeta -> Either String UninitializedAppStateWithMeta
fromForeignAppStateWithMeta (ForeignAppStateWithMeta foreignAppStateWithMeta) = do
  uninitializedAppState <- fromForeignAppState foreignAppStateWithMeta.appState
  metadata <- fromForeignAppStateMeta foreignAppStateWithMeta.metadata
  pure $ { uninitializedAppState : uninitializedAppState
         , metadata : metadata
         }

appStateFromJSON :: String -> Either String UninitializedAppStateWithMeta
appStateFromJSON json =
  let
    exceptTForeignAppStateWithMeta :: ExceptT (NonEmptyList ForeignError) Identity ForeignAppStateWithMeta
    exceptTForeignAppStateWithMeta = genericDecodeJSON defaultOptions json
    Identity (eitherForeignAppStateWithMeta) = runExceptT exceptTForeignAppStateWithMeta
  in
   eitherForeignAppStateWithMeta
   # lmap ((map renderForeignError) >>> show)
   # (flip bind) fromForeignAppStateWithMeta
