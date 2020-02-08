module AppState where

import Prelude

import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', Traversal', lens, traversed)
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Symbol (SProxy(..))
import Data.UUID (UUID)
import Foreign.Class (class Encode, class Decode)
import Foreign.Generic (genericEncode, genericDecode, defaultOptions)
import Megagraph (Edge, EdgeId, Focus, Graph, GraphId, GraphSpacePoint2D(..), GraphView, Mapping, MappingId, NodeId, PageSpacePoint2D, Point2D, emptyGraph, emptyMapping, freshPane)
import MegagraphOperation (MegagraphUpdate)
import Web.HTML.HTMLElement as WHE


appStateVersion :: String
appStateVersion = "0.0.0.0.0.0.0.1"

------
-- Types

type KeyHoldState
  = { spaceDown   :: Boolean
    , controlDown :: Boolean
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
    , hoveredElements    :: Set HoveredElementId
    , focus              :: Maybe Focus
    , focusedPane        :: Maybe MegagraphElement
    , keyHoldState       :: KeyHoldState
    }

emptyAppState :: WHE.DOMRect -> AppState
emptyAppState rect
  = { megagraph          : emptyMegagraph
    , windowBoundingRect : rect
    , drawingEdges       : Map.empty
    , hoveredElements    : Set.empty
    , focus              : Nothing
    , focusedPane        : Nothing
    , keyHoldState       : { spaceDown : false, controlDown : false }
    }

type Shape
  = { width :: Number
    , height :: Number
    }

edgeIdStr :: Edge -> String
edgeIdStr edge = show edge.source <> "_" <> show edge.target

data EdgeSourceElement
  = NodeSource NodeId
  | EdgeSource EdgeId

derive instance eqEdgeSourceElement :: Eq EdgeSourceElement

instance showEdgeSourceElement :: Show EdgeSourceElement where
  show = case _ of
    NodeSource nodeId -> "EdgeSourceElement NodeSource " <> show nodeId
    EdgeSource edgeId -> "EdgeSourceElement EdgeSource " <> show edgeId

type DrawingEdge
  = { source         :: EdgeSourceElement
    , sourcePosition :: PageSpacePoint2D
    , sourceGraph    :: GraphId
    , pointPosition  :: PageSpacePoint2D
    , targetGraph    :: GraphId
    }

type DrawingEdgeId = UUID

data MegagraphElement
  = GraphElement GraphId
  | MappingElement MappingId GraphId GraphId

derive instance eqMegagraphElement :: Eq MegagraphElement

derive instance ordMegagraphElement :: Ord MegagraphElement

derive instance genericMegagraphElement :: Generic MegagraphElement _
instance decodeMegagraphElement :: Decode MegagraphElement where
  decode = genericDecode defaultOptions
instance encodeMegagraphElement :: Encode MegagraphElement where
  encode = genericEncode defaultOptions

instance showMegagraphElement :: Show MegagraphElement where
  show = case _ of
    GraphElement graphId -> "GraphElement " <> show graphId
    MappingElement mappingId source target ->
      "MappingElement " <> show mappingId <> " from: " <> show source <> " to: " <> show target

data HoveredElementId
  = NodeHaloId   GraphId          NodeId
  | NodeBorderId GraphId          NodeId
  | EdgeHaloId   MegagraphElement EdgeId
  | EdgeBorderId MegagraphElement EdgeId

derive instance eqHoveredElementId :: Eq HoveredElementId

derive instance ordHoveredElementId :: Ord HoveredElementId

instance showHoveredElementId :: Show HoveredElementId where
   show = case _ of
     NodeHaloId   graphId nodeId -> "NodeHaloId in graph: " <> show graphId <> " node: " <> show nodeId
     NodeBorderId graphId nodeId -> "NodeBorderId in graph: " <> show graphId <> " node: " <> show nodeId
     EdgeHaloId   graphId edgeId -> "EdgeHaloId in graph: " <> show graphId <> " edge: " <> show edgeId
     EdgeBorderId graphId edgeId -> "EdgeBorderId in graph: " <> show graphId <> "  edge: " <> show edgeId


------
-- Lenses

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

_mappingAtId :: MappingId -> Traversal' AppState Mapping
_mappingAtId mappingId = _megagraph <<< _mappingState mappingId <<< traversed <<< _mapping

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

_focusedPane :: Lens' AppState (Maybe MegagraphElement)
_focusedPane = prop (SProxy :: SProxy "focusedPane")

_hoveredElements :: Lens' AppState (Set HoveredElementId)
_hoveredElements = prop (SProxy :: SProxy "hoveredElements")

_keyHoldState :: Lens' AppState KeyHoldState
_keyHoldState = prop (SProxy :: SProxy "keyHoldState")

_spaceDown :: Lens' KeyHoldState Boolean
_spaceDown = prop (SProxy :: SProxy "spaceDown")

_controlDown :: Lens' KeyHoldState Boolean
_controlDown = prop (SProxy :: SProxy "controlDown")


------
-- Utils

-- | There is only a sinlge mapping present n in the state between any two graphs,
-- | so mappings can be found uniquely by their source and target graphs.
lookupMapping :: GraphId -> GraphId -> AppState -> Maybe Mapping
lookupMapping sourceGraphId targetGraphId state =
  state.megagraph.mappings
  # Map.values >>> Array.fromFoldable
  <#> _.mapping
  # Array.filter (\mapping -> mapping.sourceGraph == sourceGraphId
                           && mapping.targetGraph == targetGraphId)
  # Array.head
