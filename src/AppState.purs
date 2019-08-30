module AppState where

import Prelude

import Data.Lens (Lens', Traversal', lens, traversed, (^.))
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Symbol (SProxy(..))
import Data.UUID (UUID)
import Point2D (Point2D)
import Web.HTML.HTMLElement as WHE
import Workflow.Core (NodeId, EdgeId, _nodes, _source, _target)
import Workflow.UIGraph.Types (UIEdge, UIGraph, _pos)
import Workflow.UIGraph.UIGraphOp (UIGraphOp)
import Data.Undoable (Undoable, _current)


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

type AppState =
  { graph :: Undoable UIGraph (UIGraphOp Unit)
  , drawingEdges :: Map DrawingEdgeId DrawingEdge
  , hoveredElementId :: Maybe HoveredElementId
  , boundingRect :: WHE.DOMRect
  , graphOrigin :: PageSpacePos
  , zoom :: Number
  }

_undoableGraph :: Lens' AppState (Undoable UIGraph (UIGraphOp Unit))
_undoableGraph = prop (SProxy :: SProxy "graph")

_graph :: Lens' AppState UIGraph
_graph = _undoableGraph <<< _current

_drawingEdges :: Lens' AppState (Map DrawingEdgeId DrawingEdge)
_drawingEdges = prop (SProxy :: SProxy "drawingEdges")

_drawingEdgePos :: DrawingEdgeId -> Traversal' AppState GraphSpacePos
_drawingEdgePos drawingEdgePos =
  _drawingEdges <<< at drawingEdgePos <<< traversed <<< prop (SProxy :: SProxy "pos")

_graphNodePos :: NodeId -> Traversal' AppState GraphSpacePos
_graphNodePos nodeId =
  _graph <<<_nodes <<< at nodeId <<< traversed <<< _pos <<< _coerceToGraphSpace

_zoom :: Lens' AppState Number
_zoom = prop (SProxy :: SProxy "zoom")

_boundingRect :: Lens' AppState WHE.DOMRect
_boundingRect  = prop (SProxy :: SProxy "boundingRect")

_graphOrigin :: Lens' AppState PageSpacePos
_graphOrigin  = prop (SProxy :: SProxy "graphOrigin")

_hoveredElementId :: Lens' AppState (Maybe HoveredElementId)
_hoveredElementId  = prop (SProxy :: SProxy "hoveredElementId")

_coerceToGraphSpace :: Lens' Point2D GraphSpacePos
_coerceToGraphSpace = lens GraphSpacePos (\_ (GraphSpacePos pos) -> pos)
