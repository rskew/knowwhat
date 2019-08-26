module AppState.JSON where

import Prelude (bind, flip, map, pure, show, (#), ($), (<$>), (>>>), (<>), (>>=))
import AppState (AppState(..), DrawingEdge, HoveredElementId(..), GraphSpacePos(..), PageSpacePos(..), Shape)
import Workflow.UIGraph (Point2D)
import Workflow.UIGraph.JSON
import Workflow.Core (EdgeId)

import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Data.Array ((!!))
import Data.DateTime.Instant (Instant, instant, unInstant)
import Data.Either (Either(..), note)
import Data.Bifunctor (lmap)
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity(..))
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe)
import Data.Traversable (traverse)
import Data.Time.Duration (Milliseconds(..))
import Data.String (Pattern(..), split)
import Data.UUID as UUID
import Data.UUID (parseUUID)
import Foreign (Foreign, ForeignError, renderForeignError)
import Foreign.Class (class Encode, class Decode)
import Foreign.Generic (genericDecodeJSON, genericEncodeJSON, genericEncode, genericDecode)
import Foreign.Object (Object)
import Web.HTML.HTMLElement as WHE


-- | A whole bunch of boilerplate to use generic JSON serialisation/deserialisation


newtype ForeignDrawingEdge =
  ForeignDrawingEdge
  { source :: ForeignNodeId
  , pos :: ForeignPos
  }
derive instance genericForeignDrawingEdge :: Generic ForeignDrawingEdge _
instance encodeForeignDrawingEdge :: Encode ForeignDrawingEdge where
  encode x = genericEncode genericEncodeOpts x
instance decodeForeignDrawingEdge :: Decode ForeignDrawingEdge where
  decode x = genericDecode genericEncodeOpts x

data ForeignGraphElementId
  = ForeignNodeHaloId ForeignNodeId
  | ForeignNodeBorderId ForeignNodeId
  | ForeignEdgeBorderId ForeignEdgeId
derive instance genericForeignGraphElementId :: Generic ForeignGraphElementId _
instance encodeForeignGraphElementId :: Encode ForeignGraphElementId where
  encode x = genericEncode genericEncodeOpts x
instance decodeForeignGraphElementId :: Decode ForeignGraphElementId where
  decode x = genericDecode genericEncodeOpts x

newtype ForeignShape = ForeignShape Shape
derive instance genericForeignShape :: Generic ForeignShape _
instance encodeForeignShape :: Encode ForeignShape where
  encode :: ForeignShape -> Foreign
  encode x = genericEncode genericEncodeOpts x
instance decodeForeignShape :: Decode ForeignShape where
  decode x = genericDecode genericEncodeOpts x

newtype ForeignPos = ForeignPos Point2D
derive instance genericForeignPos :: Generic ForeignPos _
instance encodeForeignPos :: Encode ForeignPos where
  encode x = genericEncode genericEncodeOpts x
instance decodeForeignPos :: Decode ForeignPos where
  decode x = genericDecode genericEncodeOpts x

newtype ForeignAppState =
  ForeignAppState
  { graph :: ForeignUIGraph
  , nodeTextFieldShapes :: Object ForeignShape
  , edgeTextFieldShapes :: Object ForeignShape
  , drawingEdges :: Object ForeignDrawingEdge
  , hoveredElementId :: Maybe ForeignGraphElementId
  , boundingRect :: WHE.DOMRect
  , graphOrigin :: ForeignPos
  , zoom :: Number
  }
derive instance genericForeignAppState :: Generic ForeignAppState _
instance encodeForeignAppState :: Encode ForeignAppState where
  encode x = genericEncode genericEncodeOpts x
instance decodeForeignAppState :: Decode ForeignAppState where
  decode x = genericDecode genericEncodeOpts x

type AppStateMeta = { version :: String
                    , timestamp :: Instant
                    , graphMetadata :: UIGraphMeta
                    }

newtype ForeignAppStateMeta =
  ForeignAppStateMeta
  { version :: String
  , timestamp :: Number
  , graphMetadata :: ForeignUIGraphMeta
  }
derive instance genericForeignAppStateMeta :: Generic ForeignAppStateMeta _
instance encodeForeignAppStateMeta :: Encode ForeignAppStateMeta where
  encode x = genericEncode genericEncodeOpts x
instance decodeForeignAppStateMeta :: Decode ForeignAppStateMeta where
  decode x = genericDecode genericEncodeOpts x

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
  encode x = genericEncode genericEncodeOpts x
instance decodeForeignAppStateWithMeta :: Decode ForeignAppStateWithMeta where
  decode x = genericDecode genericEncodeOpts x

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

objectifyAppState :: AppState -> ForeignAppState
objectifyAppState (AppState state) =
  let
    foreignGraph = objectifyUIGraph state.graph
    foreignNodeTextFieldShapes = objectifyMap ForeignShape UUID.toString state.nodeTextFieldShapes
    foreignEdgeTextFieldShapes = objectifyMap ForeignShape edgeIdToString state.edgeTextFieldShapes
    foreignDrawingEdges = objectifyMap objectifyDrawingEdge UUID.toString state.drawingEdges
    foreignHoveredElementId = objectifyGraphElementId <$> state.hoveredElementId
    PageSpacePos graphOrigin = state.graphOrigin
    foreignGraphOrigin = ForeignPos graphOrigin
  in
    ForeignAppState $
    state { graph = foreignGraph
          , nodeTextFieldShapes = foreignNodeTextFieldShapes
          , edgeTextFieldShapes = foreignEdgeTextFieldShapes
          , drawingEdges = foreignDrawingEdges
          , hoveredElementId = foreignHoveredElementId
          , graphOrigin = foreignGraphOrigin
          }

objectifyAppStateMeta :: AppStateMeta -> ForeignAppStateMeta
objectifyAppStateMeta metadata =
  let
    Milliseconds millis = unInstant metadata.timestamp
    foreignGraphMetadata = objectifyUIGraphMeta metadata.graphMetadata
  in
    ForeignAppStateMeta $
    metadata { timestamp = millis
             , graphMetadata = foreignGraphMetadata
             }

appStateToJSON :: AppState -> AppStateMeta -> String
appStateToJSON appState metadata =
  let
    foreignAppStateWithMeta =
      ForeignAppStateWithMeta
      { appState : objectifyAppState appState
      , metadata : objectifyAppStateMeta metadata
      }
    AppState appState' = appState
  in
    genericEncodeJSON genericEncodeOpts foreignAppStateWithMeta


------
-- Deserialisation

unObjectifyDrawingEdge :: ForeignDrawingEdge -> Either String DrawingEdge
unObjectifyDrawingEdge (ForeignDrawingEdge foreignDrawingEdge) =
  let
    ForeignPos foreignDrawingEdgePos = foreignDrawingEdge.pos
    drawingEdgePos = GraphSpacePos foreignDrawingEdgePos
  in do
    source <- parseUUIDEither foreignDrawingEdge.source
    pure $ { source : source
           , pos : drawingEdgePos
           }

unObjectifyGraphElementId :: ForeignGraphElementId -> Either String HoveredElementId
unObjectifyGraphElementId (ForeignNodeHaloId foreignNodeId) = NodeHaloId <$> parseUUIDEither foreignNodeId
unObjectifyGraphElementId (ForeignNodeBorderId foreignNodeId) = NodeBorderId <$> parseUUIDEither foreignNodeId
unObjectifyGraphElementId (ForeignEdgeBorderId (ForeignEdgeId foreignEdgeId)) = do
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

unObjectifyAppState :: ForeignAppState -> Either String AppState
unObjectifyAppState (ForeignAppState foreignState) = do
  graph <- unObjectifyUIGraph foreignState.graph
  nodeTextFieldShapes <- unObjectifyMap (\(ForeignShape x) -> Right x) parseUUIDEither foreignState.nodeTextFieldShapes
  edgeTextFieldShapes <- unObjectifyMap (\(ForeignShape x) -> Right x) parseEdgeIdEither foreignState.edgeTextFieldShapes
  drawingEdges <- unObjectifyMap unObjectifyDrawingEdge parseUUIDEither foreignState.drawingEdges
  hoveredElementId <- traverse unObjectifyGraphElementId foreignState.hoveredElementId
  let ForeignPos foreignGraphOrigin = foreignState.graphOrigin
  let graphOrigin = PageSpacePos foreignGraphOrigin
  pure $ AppState $
    foreignState { graph = graph
                 , nodeTextFieldShapes = nodeTextFieldShapes
                 , edgeTextFieldShapes = edgeTextFieldShapes
                 , drawingEdges = drawingEdges
                 , hoveredElementId = hoveredElementId
                 , graphOrigin = graphOrigin
                 }


unObjectifyAppStateMeta :: ForeignAppStateMeta -> Either String AppStateMeta
unObjectifyAppStateMeta (ForeignAppStateMeta foreignMeta) = do
  timestamp <- note "Failed to convert timestamp from foreign"
               $ instant $ Milliseconds foreignMeta.timestamp
  graphMetadata <- unObjectifyUIGraphMeta foreignMeta.graphMetadata
  pure $ foreignMeta { timestamp = timestamp
                     , graphMetadata = graphMetadata
                     }

unObjectifyAppStateWithMeta :: ForeignAppStateWithMeta -> Either String AppStateWithMeta
unObjectifyAppStateWithMeta (ForeignAppStateWithMeta foreignAppStateWithMeta) = do
  appState <- unObjectifyAppState foreignAppStateWithMeta.appState
  metadata <- unObjectifyAppStateMeta foreignAppStateWithMeta.metadata
  pure $ { appState : appState, metadata : metadata }

appStateFromJSON :: String -> Either String AppStateWithMeta
appStateFromJSON json =
  let
    exceptTForeignAppStateWithMeta :: ExceptT (NonEmptyList ForeignError) Identity ForeignAppStateWithMeta
    exceptTForeignAppStateWithMeta = genericDecodeJSON genericEncodeOpts json
    Identity (eitherForeignAppStateWithMeta) = runExceptT exceptTForeignAppStateWithMeta
  in
   eitherForeignAppStateWithMeta
   # lmap ((map renderForeignError) >>> show)
   # (flip bind) unObjectifyAppStateWithMeta
