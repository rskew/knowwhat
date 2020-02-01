module AppState where

import Prelude

import Data.Lens (Lens', Traversal', lens, traversed)
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Megagraph (Edge, EdgeId, Graph, GraphId, GraphSpacePoint2D(..), GraphView, Mapping, MappingId, NodeId, PageSpacePoint2D, Point2D, emptyGraph, emptyMapping, freshPane)
import MegagraphOperation (MegagraphUpdate)
import Web.HTML.HTMLElement as WHE


appStateVersion :: String
appStateVersion = "0.0.0.0.0.0.0.1"

------
-- Types

type KeyHoldState
  = { spaceDown :: Boolean
    }

type GraphState
  = { graph :: Graph
    , view :: GraphView
    , history :: Array MegagraphUpdate
    , undone :: Array MegagraphUpdate
    }

emptyGraphState :: GraphId -> WHE.DOMRect -> GraphState
emptyGraphState graphId rect
  = { graph : emptyGraph graphId
    , view : freshPane graphId rect
    , history : []
    , undone : []
    }

type MappingState
  = { mapping :: Mapping
    , history :: Array MegagraphUpdate
    , undone  :: Array MegagraphUpdate
    }

emptyMappingState :: MappingId -> GraphId -> GraphId -> MappingState
emptyMappingState mappingId from to = { mapping : emptyMapping mappingId from to
                                      , history : []
                                      , undone : []
                                      }

-- | A selection of graphs and mappings from the larger megagraph.
-- | A megagraph is a collection of graphs and mappings between graphs
-- | where each node has a subgraph and each edge has a submapping.
-- | A mapping is a set of mappingEdges from nodes to nodes and edges to edges.
-- | The UI represents a sub-megagraph, where there is only a single mapping
-- | between any pairs of graphs. This is a view on a larger megagraph.
type MegagraphState
  = { graphs :: Map GraphId GraphState
    , mappings :: Map MappingId MappingState
    }

emptyMegagraph :: MegagraphState
emptyMegagraph = { graphs : Map.empty
                 , mappings : Map.empty
                 }

type AppState
  = { megagraph          :: MegagraphState
    , windowBoundingRect :: WHE.DOMRect
    , drawingEdges       :: Map DrawingEdgeId DrawingEdge
    , hoveredElementId   :: Maybe HoveredElementId
    , focusedPane        :: Maybe GraphId
    , keyHoldState       :: KeyHoldState
    }

emptyAppState :: WHE.DOMRect -> AppState
emptyAppState rect
  = { megagraph          : emptyMegagraph
    , windowBoundingRect : rect
    , drawingEdges       : Map.empty
    , hoveredElementId   : Nothing
    , focusedPane        : Nothing
    , keyHoldState       : { spaceDown : false }
    }

type Shape
  = { width :: Number
    , height :: Number
    }

edgeIdStr :: Edge -> String
edgeIdStr edge = show edge.source <> "_" <> show edge.target

type DrawingEdge
  = { source         :: NodeId
    , sourcePosition :: PageSpacePoint2D
    , sourceGraph    :: GraphId
    , pointPosition  :: PageSpacePoint2D
    , targetGraph    :: GraphId
    }

type DrawingEdgeId = NodeId

drawingEdgeKey :: DrawingEdgeId -> String
drawingEdgeKey id = "DrawingEdge_" <> show id

data HoveredElementId
  = NodeHaloId NodeId
  | NodeBorderId NodeId
  | EdgeHaloId EdgeId
  | EdgeBorderId EdgeId

derive instance eqGraphElementId :: Eq HoveredElementId

instance showHoveredElementId :: Show HoveredElementId where
   show = case _ of
     NodeHaloId nodeId   -> "NodeHaloId " <> show nodeId
     NodeBorderId nodeId -> "NodeBorderId " <> show nodeId
     EdgeHaloId edgeId   -> "EdgeHaloId " <> show edgeId
     EdgeBorderId edgeId -> "EdgeBorderId " <> show edgeId

------
-- Lenses

--_graphView :: GraphId -> Lens' AppState (Maybe GraphView)
--_graphView graphId =
--  prop (SProxy :: SProxy "graphData")
--  <<< prop (SProxy :: SProxy "panes")
--  <<< at graphId

--_focus :: GraphId -> Traversal' AppState (Maybe Focus)
--_focus graphId =
--  _graphView graphId
--  <<< traversed
--  <<< prop (SProxy :: SProxy "focus")

_drawingEdges :: Lens' AppState (Map DrawingEdgeId DrawingEdge)
_drawingEdges = prop (SProxy :: SProxy "drawingEdges")

_drawingEdgePosition :: DrawingEdgeId -> Traversal' AppState PageSpacePoint2D
_drawingEdgePosition drawingEdgeId =
  _drawingEdges <<< at drawingEdgeId <<< traversed <<< prop (SProxy :: SProxy "pointPosition")

_drawingEdgeTargetGraph :: DrawingEdgeId -> Traversal' AppState GraphId
_drawingEdgeTargetGraph drawingEdgeId =
  _drawingEdges <<< at drawingEdgeId <<< traversed <<< prop (SProxy :: SProxy "targetGraph")

_megagraph :: Lens' AppState MegagraphState
_megagraph = prop (SProxy :: SProxy "megagraph")

_graphs :: Lens' MegagraphState (Map GraphId GraphState)
_graphs = prop (SProxy :: SProxy "graphs")

_graphState :: GraphId -> Lens' MegagraphState (Maybe GraphState)
_graphState graphId =
  prop (SProxy :: SProxy "graphs")
  <<< at graphId

_mappings :: Lens' MegagraphState (Map MappingId MappingState)
_mappings = prop (SProxy :: SProxy "mappings")

_mappingState :: MappingId -> Lens' MegagraphState (Maybe MappingState)
_mappingState mappingId =
  prop (SProxy :: SProxy "mappings")
  <<< at mappingId

_mapping :: Lens' MappingState Mapping
_mapping = prop (SProxy :: SProxy "mapping")

_graph :: Lens' GraphState Graph
_graph = prop (SProxy :: SProxy "graph")

_graphAtId :: GraphId -> Traversal' AppState Graph
_graphAtId graphId = _megagraph <<< _graphState graphId <<< traversed <<< _graph

_pane :: Lens' GraphState GraphView
_pane = prop (SProxy :: SProxy "view")

_paneAtId :: GraphId -> Traversal' AppState GraphView
_paneAtId graphId = _megagraph <<< _graphState graphId <<< traversed <<< _pane

_windowBoundingRect :: Lens' AppState WHE.DOMRect
_windowBoundingRect = prop (SProxy :: SProxy "windowBoundingRect")

_coerceToGraphSpace :: Lens' Point2D GraphSpacePoint2D
_coerceToGraphSpace = lens GraphSpacePoint2D (\_ (GraphSpacePoint2D pos) -> pos)

--_history :: Lens' AppState (Map GraphId (Array (AppOperation Unit)))
--_history = prop (SProxy :: SProxy "history")
--
--_undone :: Lens' AppState (Map GraphId (Array (AppOperation Unit)))
--_undone = prop (SProxy :: SProxy "undone")

_focusedPane :: Lens' AppState (Maybe GraphId)
_focusedPane = prop (SProxy :: SProxy "focusedPane")
