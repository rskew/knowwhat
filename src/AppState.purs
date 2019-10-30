module AppState where

import Prelude

import AppOperation (AppOperation)
import Core (GraphData, Edge, GraphId, NodeId, EdgeId, PageSpacePoint2D, GraphSpacePoint2D(..), Focus, GraphView, Point2D, emptyGraphData)
import Data.Lens (Lens', Traversal', lens, traversed)
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Web.HTML.HTMLElement as WHE


appStateVersion :: String
appStateVersion = "0.0.0.0.0.0.0.1"

------
-- Types

type KeyHoldState
  = { spaceDown :: Boolean }

type AppState =
  { graphData          :: GraphData
  , history            :: Map GraphId (Array (AppOperation Unit))
  , undone             :: Map GraphId (Array (AppOperation Unit))
  , windowBoundingRect :: WHE.DOMRect
  , drawingEdges       :: Map DrawingEdgeId DrawingEdge
  , hoveredElementId   :: Maybe HoveredElementId
  , focusedPane        :: Maybe GraphId
  , keyHoldState       :: KeyHoldState
  }

emptyAppState :: AppState
emptyAppState =
  { graphData          : emptyGraphData
  , history            : Map.empty
  , undone             : Map.empty
  , windowBoundingRect : { height : 0.0, width : 0.0, left : 0.0, right : 0.0, top : 0.0, bottom : 0.0 }
  , drawingEdges       : Map.empty
  , hoveredElementId   : Nothing
  , focusedPane        : Nothing
  , keyHoldState       : { spaceDown : false }
  }

type Shape = { width :: Number
             , height :: Number
             }

edgeIdStr :: Edge -> String
edgeIdStr edge = show edge.id.source <> "_" <> show edge.id.target

type DrawingEdge = { source         :: NodeId
                   , sourcePosition :: PageSpacePoint2D
                   , sourceGraph    :: GraphId
                   , pointPosition  :: PageSpacePoint2D
                   , targetGraph    :: GraphId
                   }

type DrawingEdgeId = NodeId

drawingEdgeKey :: DrawingEdgeId -> String
drawingEdgeKey id = "DrawingEdge_" <> show id

data DragSource
  = NodeDrag
  | HaloDrag
  | BackgroundDrag

derive instance eqDragSource :: Eq DragSource

derive instance ordDragSource :: Ord DragSource

instance showDragSource :: Show DragSource where
  show NodeDrag       = "NodeDrag"
  show HaloDrag       = "HaloDrag"
  show BackgroundDrag = "BackgroundDrag"

data HoveredElementId
  = NodeHaloId NodeId
  | NodeBorderId NodeId
  | EdgeBorderId EdgeId

derive instance eqGraphElementId :: Eq HoveredElementId

instance showHoveredElementId :: Show HoveredElementId where
   show = case _ of
     NodeHaloId nodeId   -> "NodeHaloId " <> show nodeId
     NodeBorderId nodeId -> "NodeBorderId " <> show nodeId
     EdgeBorderId edgeId -> "EdgeBorderId " <> show edgeId

------
-- Lenses

_graphView :: GraphId -> Lens' AppState (Maybe GraphView)
_graphView graphId =
  prop (SProxy :: SProxy "graphData")
  <<< prop (SProxy :: SProxy "panes")
  <<< at graphId

_focus :: GraphId -> Traversal' AppState (Maybe Focus)
_focus graphId =
  _graphView graphId
  <<< traversed
  <<< prop (SProxy :: SProxy "focus")

_drawingEdges :: Lens' AppState (Map DrawingEdgeId DrawingEdge)
_drawingEdges = prop (SProxy :: SProxy "drawingEdges")

_drawingEdgePosition :: DrawingEdgeId -> Traversal' AppState PageSpacePoint2D
_drawingEdgePosition drawingEdgeId =
  _drawingEdges <<< at drawingEdgeId <<< traversed <<< prop (SProxy :: SProxy "pointPosition")

_drawingEdgeTargetGraph :: DrawingEdgeId -> Traversal' AppState GraphId
_drawingEdgeTargetGraph drawingEdgeId =
  _drawingEdges <<< at drawingEdgeId <<< traversed <<< prop (SProxy :: SProxy "targetGraph")

_graphData :: Lens' AppState GraphData
_graphData = prop (SProxy :: SProxy "graphData")

_windowBoundingRect :: Lens' AppState WHE.DOMRect
_windowBoundingRect = prop (SProxy :: SProxy "windowBoundingRect")

_coerceToGraphSpace :: Lens' Point2D GraphSpacePoint2D
_coerceToGraphSpace = lens GraphSpacePoint2D (\_ (GraphSpacePoint2D pos) -> pos)

_history :: Lens' AppState (Map GraphId (Array (AppOperation Unit)))
_history = prop (SProxy :: SProxy "history")

_undone :: Lens' AppState (Map GraphId (Array (AppOperation Unit)))
_undone = prop (SProxy :: SProxy "undone")

_focusedPane :: Lens' AppState (Maybe GraphId)
_focusedPane = prop (SProxy :: SProxy "focusedPane")
