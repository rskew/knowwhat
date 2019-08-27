module AppState where

import Prelude

import Data.Lens (Lens', Traversal', lens, traversed, (^.))
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Symbol (SProxy(..))
import Data.UUID (UUID)
import Web.HTML.HTMLElement as WHE
import Workflow.Core (NodeId, EdgeId, _nodes, _source, _target)
import Workflow.UIGraph (Point2D, UIEdge, UIGraph, _pos)
--import Workflow.UIGraph.UIGraphOp (UIGraphOp)
--import Undoable (Undoable, _current)


appStateVersion :: String
appStateVersion = "0.0.0.0.0.0.0.1"

type Shape = { width :: Number, height :: Number }

type GraphId = UUID

-- | A position in graph space, as distinct from a point on the page/window
newtype GraphSpacePos = GraphSpacePos Point2D

-- | PageSpacePos represents a position on the browser window such as
-- | a mouse position, as distinct from a position in graph space.
newtype PageSpacePos = PageSpacePos Point2D

toGraphSpace :: WHE.DOMRect -> PageSpacePos -> Number -> PageSpacePos -> GraphSpacePos
toGraphSpace boundingRect (PageSpacePos graphOrigin) zoom (PageSpacePos pagePos) =
  GraphSpacePos $ { x : (pagePos.x - boundingRect.left - graphOrigin.x) * zoom
                  , y : (pagePos.y - boundingRect.top - graphOrigin.y) * zoom
                  }

toPageSpace :: WHE.DOMRect -> Number -> GraphSpacePos -> PageSpacePos
toPageSpace boundingRect zoom (GraphSpacePos graphPos) =
  let
    graphOrigin = { x : boundingRect.left, y : boundingRect.top }
  in
    PageSpacePos $ { x : (graphPos.x + boundingRect.left + graphOrigin.x) / zoom
                   , y : (graphPos.y + boundingRect.top + graphOrigin.y) / zoom
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

data DragSource
  = NodeDrag
  | HaloDrag
  | BackgroundDrag
derive instance eqDragSource :: Eq DragSource
derive instance ordDragSource :: Ord DragSource
instance showDragSource :: Show DragSource where
  show NodeDrag = "NodeDrag"
  show HaloDrag = "HaloDrag"
  show BackgroundDrag = "BackgroundDrag"

data HoveredElementId
  = NodeHaloId NodeId
  | NodeBorderId NodeId
  | EdgeBorderId EdgeId
derive instance eqGraphElementId :: Eq HoveredElementId

--type UndoableUIGraph = Undoable UIGraph (UIGraphOp Unit)

type AppStateInner =
  --{ graph :: UndoableUIGraph
  { graph :: UIGraph
  , nodeTextFieldShapes :: Map NodeId Shape
  , edgeTextFieldShapes :: Map EdgeId Shape
  , drawingEdges :: Map DrawingEdgeId DrawingEdge
  , hoveredElementId :: Maybe HoveredElementId
  , boundingRect :: WHE.DOMRect
  , graphOrigin :: PageSpacePos
  , zoom :: Number
  }

newtype AppState = AppState AppStateInner

_AppState :: Lens' AppState AppStateInner
_AppState = lens (\(AppState appState) -> appState) (\_ -> AppState)

_graph :: Lens' AppState UIGraph
--_graph = _AppState <<< prop (SProxy :: SProxy "graph") <<< _current
_graph = _AppState <<< prop (SProxy :: SProxy "graph")

_nodeTextFieldShapes :: Lens' AppState (Map NodeId Shape)
_nodeTextFieldShapes = _AppState <<< prop (SProxy :: SProxy "nodeTextFieldShapes")

_edgeTextFieldShapes :: Lens' AppState (Map EdgeId Shape)
_edgeTextFieldShapes = _AppState <<< prop (SProxy :: SProxy "edgeTextFieldShapes")

_drawingEdges :: Lens' AppState (Map DrawingEdgeId DrawingEdge)
_drawingEdges = _AppState <<< prop (SProxy :: SProxy "drawingEdges")

_drawingEdgePos :: DrawingEdgeId -> Traversal' AppState GraphSpacePos
_drawingEdgePos drawingEdgePos =
  _drawingEdges <<< at drawingEdgePos <<< traversed <<< prop (SProxy :: SProxy "pos")

_graphNodePos :: NodeId -> Traversal' AppState GraphSpacePos
_graphNodePos nodeId =
  _graph <<<_nodes <<< at nodeId <<< traversed <<< _pos <<< _coerceToGraphSpace

_zoom :: Lens' AppState Number
_zoom = _AppState <<< prop (SProxy :: SProxy "zoom")

_boundingRect :: Lens' AppState WHE.DOMRect
_boundingRect  = _AppState <<< prop (SProxy :: SProxy "boundingRect")

_graphOrigin :: Lens' AppState PageSpacePos
_graphOrigin  = _AppState <<< prop (SProxy :: SProxy "graphOrigin")

_coerceToGraphSpace :: Lens' Point2D GraphSpacePos
_coerceToGraphSpace = lens GraphSpacePos (\_ (GraphSpacePos pos) -> pos)
