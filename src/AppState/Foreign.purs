module AppState.Foreign where

import Prelude

import AppState (AppState, DrawingEdge, HoveredElementId(..), GraphSpacePos(..), PageSpacePos(..), Shape)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Data.Array ((!!))
import Data.Bifunctor (lmap)
import Data.DateTime.Instant (Instant, instant, unInstant)
import Data.Either (Either, note)
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity(..))
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe)
import Data.String (Pattern(..), split)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Data.UUID (parseUUID)
import Data.UUID as UUID
import Data.Undoable (Undoable, mapActions)
import Foreign (Foreign, ForeignError, renderForeignError)
import Foreign.Class (class Encode, class Decode)
import Foreign.Generic (genericDecodeJSON, genericEncodeJSON, genericEncode, genericDecode, defaultOptions)
import Foreign.Object (Object)
import Web.HTML.HTMLElement as WHE
import Point2D (Point2D)
import Workflow.Core (EdgeId)
import Workflow.UIGraph.Types (UIGraph, ForeignNodeId, ForeignEdgeId(..), toForeignMap, fromForeignMap, parseUUIDEither)
import Workflow.UIGraph.UIGraphOp (UIGraphOp)


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

data ForeignUnit = ForeignUnit
derive instance genericForeignUnit :: Generic ForeignUnit _
instance encodeForeignUnit :: Encode ForeignUnit where
  encode x = genericEncode defaultOptions x
instance decodeForeignUnit :: Decode ForeignUnit where
  decode x = genericDecode defaultOptions x

newtype ForeignAppState =
  ForeignAppState
  { graph :: Undoable UIGraph (UIGraphOp ForeignUnit)
  , drawingEdges :: Object ForeignDrawingEdge
  , hoveredElementId :: Maybe ForeignGraphElementId
  , boundingRect :: WHE.DOMRect
  , graphOrigin :: ForeignPos
  , zoom :: Number
  }
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

type AppStateWithMeta = { appState :: AppState
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

objectifyDrawingEdge :: DrawingEdge -> ForeignDrawingEdge
objectifyDrawingEdge drawingEdge =
  let
    GraphSpacePos drawingEdgePos = drawingEdge.pos
  in
   ForeignDrawingEdge $
    drawingEdge { source = UUID.toString drawingEdge.source
                , pos = ForeignPos drawingEdgePos
                }

objectifyGraphElementId :: HoveredElementId -> ForeignGraphElementId
objectifyGraphElementId (NodeHaloId nodeId) = ForeignNodeHaloId $ UUID.toString nodeId
objectifyGraphElementId (NodeBorderId nodeId) = ForeignNodeBorderId $ UUID.toString nodeId
objectifyGraphElementId (EdgeBorderId edgeId) =
  ForeignEdgeBorderId $ ForeignEdgeId
  { source : UUID.toString edgeId.source
  , target : UUID.toString edgeId.target
  }

edgeIdToString :: EdgeId -> String
edgeIdToString edgeId = UUID.toString edgeId.source
                        <> " "
                        <> UUID.toString edgeId.target

toForeignUnit :: Unit -> ForeignUnit
toForeignUnit _ = ForeignUnit

objectifyAppState :: AppState -> ForeignAppState
objectifyAppState state =
  let
    foreignGraph = mapActions (map toForeignUnit) state.graph
    foreignDrawingEdges = toForeignMap objectifyDrawingEdge UUID.toString state.drawingEdges
    foreignHoveredElementId = objectifyGraphElementId <$> state.hoveredElementId
    PageSpacePos graphOrigin = state.graphOrigin
    foreignGraphOrigin = ForeignPos graphOrigin
  in
    ForeignAppState $
    state { graph = foreignGraph
          , drawingEdges = foreignDrawingEdges
          , hoveredElementId = foreignHoveredElementId
          , graphOrigin = foreignGraphOrigin
          }

objectifyAppStateMeta :: AppStateMeta -> ForeignAppStateMeta
objectifyAppStateMeta metadata =
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
      { appState : objectifyAppState appState
      , metadata : objectifyAppStateMeta metadata
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

fromForeignUnit :: ForeignUnit -> Unit
fromForeignUnit _ = unit

fromForeignAppState :: ForeignAppState -> Either String AppState
fromForeignAppState (ForeignAppState foreignState) = do
  let graph = mapActions (map fromForeignUnit) foreignState.graph
  drawingEdges <- fromForeignMap fromForeignDrawingEdge parseUUIDEither foreignState.drawingEdges
  hoveredElementId <- traverse fromForeignGraphElementId foreignState.hoveredElementId
  let ForeignPos foreignGraphOrigin = foreignState.graphOrigin
  let graphOrigin = PageSpacePos foreignGraphOrigin
  pure $
    foreignState { graph = graph
                 , drawingEdges = drawingEdges
                 , hoveredElementId = hoveredElementId
                 , graphOrigin = graphOrigin
                 }


fromForeignAppStateMeta :: ForeignAppStateMeta -> Either String AppStateMeta
fromForeignAppStateMeta (ForeignAppStateMeta foreignMeta) = do
  timestamp <- note "Failed to convert timestamp from foreign"
               $ instant $ Milliseconds foreignMeta.timestamp
  pure $ foreignMeta { timestamp = timestamp }

fromForeignAppStateWithMeta :: ForeignAppStateWithMeta -> Either String AppStateWithMeta
fromForeignAppStateWithMeta (ForeignAppStateWithMeta foreignAppStateWithMeta) = do
  appState <- fromForeignAppState foreignAppStateWithMeta.appState
  metadata <- fromForeignAppStateMeta foreignAppStateWithMeta.metadata
  pure $ { appState : appState, metadata : metadata }

appStateFromJSON :: String -> Either String AppStateWithMeta
appStateFromJSON json =
  let
    exceptTForeignAppStateWithMeta :: ExceptT (NonEmptyList ForeignError) Identity ForeignAppStateWithMeta
    exceptTForeignAppStateWithMeta = genericDecodeJSON defaultOptions json
    Identity (eitherForeignAppStateWithMeta) = runExceptT exceptTForeignAppStateWithMeta
  in
   eitherForeignAppStateWithMeta
   # lmap ((map renderForeignError) >>> show)
   # (flip bind) fromForeignAppStateWithMeta
