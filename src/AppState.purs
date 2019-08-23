module AppState where

import Prelude

import Data.Lens (Lens', Traversal', lens, traversed, (^.))
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Symbol (SProxy(..))
import Data.UUID (UUID)
import Workflow.Core (NodeId, EdgeId, _nodes, _source, _target)
import Workflow.UIGraph (Point2D, UIEdge, UIGraph, _pos)


appStateVersion :: String
appStateVersion = "0.0.0.0.0.0.0.1"

type Shape = { width :: Number, height :: Number }

type GraphId = UUID

-- | A position in graph space, as distinct from a point on the page/window
newtype GraphSpacePos = GraphSpacePos Point2D

-- | PageSpacePos represents a position on the browser window such as
-- | a mouse position, as distinct from a position in graph space.
newtype PageSpacePos = PageSpacePos Point2D

toGraphSpace :: PageSpacePos -> Number -> PageSpacePos -> GraphSpacePos
toGraphSpace (PageSpacePos graphOrigin) zoom (PageSpacePos pagePos) =
  GraphSpacePos $ { x : (pagePos.x - graphOrigin.x) * zoom
                  , y : (pagePos.y - graphOrigin.y) * zoom
                  }

toPageSpace :: PageSpacePos -> Number -> GraphSpacePos -> PageSpacePos
toPageSpace (PageSpacePos graphOrigin) zoom (GraphSpacePos graphPos) =
  PageSpacePos $ { x : (graphPos.x + graphOrigin.x) / zoom
                 , y : (graphPos.y + graphOrigin.y) / zoom
                 }

type Edge = { source :: NodeId
            , target :: NodeId
            }

edgeIdStr :: UIEdge -> String
edgeIdStr edge = show (edge ^. _source) <> "_" <> show (edge ^. _target)

type DrawingEdge = { source :: NodeId
                   , pos :: GraphSpacePos
                   }

type DrawingEdgeId = NodeId

drawingEdgeKey :: DrawingEdgeId -> String
drawingEdgeKey id = "DrawingEdge_" <> show id

data DragSource =
    NodeDrag
  | HaloDrag
  | BackgroundDrag
derive instance eqDragSource :: Eq DragSource
derive instance ordDragSource :: Ord DragSource
instance showDragSource :: Show DragSource where
  show NodeDrag = "NodeDrag"
  show HaloDrag = "HaloDrag"
  show BackgroundDrag = "BackgroundDrag"

data GraphElementId
  = NodeHaloId NodeId
  | NodeBorderId NodeId
  | EdgeBorderId EdgeId
derive instance eqGraphElementId :: Eq GraphElementId

newtype AppState = AppState
                   { graph :: UIGraph
                   , textFieldShapes :: Map NodeId Shape
                   , drawingEdges :: Map DrawingEdgeId DrawingEdge
                   , hoveredElementId :: Maybe GraphElementId
                   , windowSize :: Shape
                   , graphOrigin :: PageSpacePos
                   , zoom :: Number
                   , graphId :: GraphId
                   }

_AppState :: Lens' AppState { graph :: UIGraph
                            , textFieldShapes :: Map NodeId Shape
                            , drawingEdges :: Map DrawingEdgeId DrawingEdge
                            , hoveredElementId :: Maybe GraphElementId
                            , windowSize :: Shape
                            , graphOrigin :: PageSpacePos
                            , zoom :: Number
                            , graphId :: GraphId
                            }
_AppState = lens (\(AppState appState) -> appState) (\_ -> AppState)

_graph :: Lens' AppState UIGraph
_graph = _AppState <<< prop (SProxy :: SProxy "graph")

_textFieldShapes :: Lens' AppState (Map NodeId Shape)
_textFieldShapes = _AppState <<< prop (SProxy :: SProxy "textFieldShapes")

_drawingEdges :: Lens' AppState (Map DrawingEdgeId DrawingEdge)
_drawingEdges = _AppState <<< prop (SProxy :: SProxy "drawingEdges")

_drawingEdgePos :: DrawingEdgeId -> Traversal' AppState GraphSpacePos
_drawingEdgePos drawingEdgePos =
  _drawingEdges <<< at drawingEdgePos <<< traversed <<< prop (SProxy :: SProxy "pos")

_graphNodePos :: NodeId -> Traversal' AppState GraphSpacePos
_graphNodePos nodeId =
  _graph <<<_nodes <<< at nodeId <<< traversed <<< _pos <<< _coerceToGraphSpace

_graphOrigin :: Lens' AppState PageSpacePos
_graphOrigin = _AppState <<< prop (SProxy :: SProxy "graphOrigin")

_zoom :: Lens' AppState Number
_zoom = _AppState <<< prop (SProxy :: SProxy "zoom")

_coerceToGraphSpace :: Lens' Point2D GraphSpacePos
_coerceToGraphSpace = lens GraphSpacePos (\_ (GraphSpacePos pos) -> pos)
