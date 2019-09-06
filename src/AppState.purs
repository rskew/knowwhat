module AppState where

import Prelude

import Data.Lens (Lens', Traversal', lens, traversed, (^.))
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Monoid.Action (class ActionM)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.UUID (UUID)
import Data.Undoable (Undoable, _current)
import Effect (Effect)
import Point2D (Point2D)
import Web.HTML.HTMLElement as WHE
import Workflow.Core (NodeId, EdgeId, _nodes, _source, _target)
import Workflow.Synth (SynthState, SynthNodeState, interpretSynth)
import Workflow.UIGraph.Types (UIEdge, UIGraph, _pos)
import Workflow.UIGraph.UIGraphOp (UIGraphOp, interpretUIGraphOp)


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

type AppStateInner =
  { graph :: UIGraph
  , drawingEdges :: Map DrawingEdgeId DrawingEdge
  , hoveredElementId :: Maybe HoveredElementId
  , boundingRect :: WHE.DOMRect
  , graphOrigin :: PageSpacePos
  , zoom :: Number
  , synthState :: SynthState
  }
newtype AppStateCurrent = AppStateCurrent AppStateInner
derive instance newtypeAppStateCurrent :: Newtype AppStateCurrent _

type AppState = Undoable AppStateCurrent (UIGraphOp Unit)

_AppStateCurrent :: Lens' AppStateCurrent AppStateInner
_AppStateCurrent = (lens (\(AppStateCurrent appStateCurrent) -> appStateCurrent) (\_ -> AppStateCurrent))

_graph :: Lens' AppState UIGraph
_graph = _current <<< _AppStateCurrent <<< prop (SProxy :: SProxy "graph")

_drawingEdges :: Lens' AppState (Map DrawingEdgeId DrawingEdge)
_drawingEdges = _current <<< _AppStateCurrent <<< prop (SProxy :: SProxy "drawingEdges")

_drawingEdgePos :: DrawingEdgeId -> Traversal' AppState GraphSpacePos
_drawingEdgePos drawingEdgePos =
  _drawingEdges <<< at drawingEdgePos <<< traversed <<< prop (SProxy :: SProxy "pos")

_graphNodePos :: NodeId -> Traversal' AppState GraphSpacePos
_graphNodePos nodeId =
  _graph <<<_nodes <<< at nodeId <<< traversed <<< _pos <<< _coerceToGraphSpace

_synthState :: Lens' AppState SynthState
_synthState = _current <<< _AppStateCurrent <<< prop (SProxy :: SProxy "synthState")

_synthNodeState :: NodeId -> Lens' AppState (Maybe SynthNodeState)
_synthNodeState nodeId = _synthState <<< prop (SProxy :: SProxy "synthNodes") <<< at nodeId

_hoveredElementId :: Lens' AppState (Maybe HoveredElementId)
_hoveredElementId = _current <<< _AppStateCurrent <<< prop (SProxy :: SProxy "hoveredElementId")

_graphOrigin :: Lens' AppState PageSpacePos
_graphOrigin = _current <<< _AppStateCurrent <<< prop (SProxy :: SProxy "graphOrigin")

_boundingRect :: Lens' AppState WHE.DOMRect
_boundingRect = _current <<< _AppStateCurrent <<< prop (SProxy :: SProxy "boundingRect")

_zoom :: Lens' AppState Number
_zoom = _current <<< _AppStateCurrent <<< prop (SProxy :: SProxy "zoom")

_coerceToGraphSpace :: Lens' Point2D GraphSpacePos
_coerceToGraphSpace = lens GraphSpacePos (\_ (GraphSpacePos pos) -> pos)

------
-- Top-level interpreter for UIGraphOp actions

interpretAppState :: forall a. UIGraphOp a -> (AppStateCurrent -> Effect AppStateCurrent)
interpretAppState op (AppStateCurrent appStateCurrent) = do
  let updatedGraph = interpretUIGraphOp op $ appStateCurrent.graph
  updatedSynthState <- interpretSynth op $ appStateCurrent.synthState
  pure $ AppStateCurrent $ appStateCurrent { graph = updatedGraph
                                           , synthState = updatedSynthState
                                           }

-- | Action instance required by Undoable
instance actUIGraphOpAppState :: Monoid a => ActionM Effect (UIGraphOp a) AppStateCurrent where
  actM op = interpretAppState op

type UninitializedAppStateInner =
  { graph :: UIGraph
  , drawingEdges :: Map DrawingEdgeId DrawingEdge
  , hoveredElementId :: Maybe HoveredElementId
  , boundingRect :: WHE.DOMRect
  , graphOrigin :: PageSpacePos
  , zoom :: Number
  }

type UninitializedAppState =
  Undoable UninitializedAppStateInner (UIGraphOp Unit)
