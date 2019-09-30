module Core where

import Prelude

import Data.Array as Array
import Data.Foldable (foldr)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', Traversal', traversed, (.~), (?~), (%~))
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as String
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.UUID (UUID)
import Foreign.Class (class Encode, class Decode)
import Foreign.Generic (genericEncode, genericDecode, defaultOptions)
import Web.HTML.HTMLElement as WHE


------
-- Types

type GraphId = UUID

type NodeId = UUID

type EdgeId = { source      :: NodeId
              , sourceGraph :: GraphId
              , target      :: NodeId
              , targetGraph :: GraphId
              }

type Point2D = { x :: Number, y :: Number }

-- | A position in graph space, as distinct from a point on the page/window
newtype GraphSpacePoint2D = GraphSpacePoint2D Point2D

derive newtype instance showGraphSpacePoint2D :: Show GraphSpacePoint2D

derive newtype instance eqGraphSpacePoint2D :: Eq GraphSpacePoint2D

derive instance genericGraphSpacePoint2D :: Generic GraphSpacePoint2D _

instance encodeGraphSpacePoint2D :: Encode GraphSpacePoint2D where
  encode x = x # genericEncode defaultOptions

instance decodeGraphSpacePoint2D :: Decode GraphSpacePoint2D where
  decode x = x # genericDecode defaultOptions

-- | PageSpacePos represents a position on the browser window such as
-- | a mouse position, as distinct from a position in graph space.
newtype PageSpacePoint2D = PageSpacePoint2D Point2D

derive newtype instance showPageSpacePoint2D :: Show PageSpacePoint2D

derive newtype instance eqPageSpacePoint2D :: Eq PageSpacePoint2D

derive instance genericPageSpacePoint2D :: Generic PageSpacePoint2D _

instance encodePageSpacePoint2D :: Encode PageSpacePoint2D where
  encode x = x # genericEncode defaultOptions

instance decodePageSpacePoint2D :: Decode PageSpacePoint2D where
  decode x = x # genericDecode defaultOptions

toGraphSpace :: GraphView -> PageSpacePoint2D -> GraphSpacePoint2D
toGraphSpace pane (PageSpacePoint2D pagePosition) =
  let
    -- origin is relative to the pane, not the window :/
    PageSpacePoint2D origin = pane.origin
  in
    GraphSpacePoint2D
      { x : (pagePosition.x - pane.boundingRect.left - origin.x) * pane.zoom
      , y : (pagePosition.y - pane.boundingRect.top  - origin.y) * pane.zoom
      }

toPageSpace :: GraphView -> GraphSpacePoint2D -> PageSpacePoint2D
toPageSpace pane (GraphSpacePoint2D graphPosition) =
  let
    -- origin is relative to the pane, not the window :/
    PageSpacePoint2D origin = pane.origin
  in
    PageSpacePoint2D $ { x : (graphPosition.x / pane.zoom) + pane.boundingRect.left + origin.x
                       , y : (graphPosition.y / pane.zoom) + pane.boundingRect.top  + origin.y
                       }

type Edge =
  { id      :: EdgeId
  , text    :: String
  , isValid :: Boolean
  }

freshEdge :: EdgeId -> Edge
freshEdge edgeId =
  { id      : edgeId
  , text    : ""
  , isValid : true
  }

type Node =
  { id       :: NodeId
  , graphId  :: GraphId
  , subgraph :: Maybe GraphId
  , position :: GraphSpacePoint2D
  , text     :: String
  , isValid  :: Boolean
  }

freshNode :: GraphId -> NodeId -> Node
freshNode graphId id =
  { id       : id
  , graphId  : graphId
  , subgraph : Nothing
  , position : GraphSpacePoint2D { x : 0.0, y : 0.0 }
  , text     : ""
  , isValid  : true
  }

data Focus =
  FocusNode NodeId
  | FocusEdge EdgeId (Array EdgeId)

derive instance eqFocus :: Eq Focus

derive instance ordFocus :: Ord Focus

derive instance genericFocus :: Generic Focus _

instance showFocus :: Show Focus where
  show = genericShow

type GraphView =
  { graphId      :: GraphId
  , origin       :: PageSpacePoint2D
  , zoom         :: Number
  , focus        :: Maybe Focus
  , boundingRect :: WHE.DOMRect
  }

emptyPane :: GraphId -> GraphView
emptyPane graphId =
  { graphId      : graphId
  , origin       : PageSpacePoint2D { x : 0.0, y : 0.0 }
  , zoom         : 1.0
  , focus        : Nothing
  , boundingRect : { width  : 0.0
                   , height : 0.0
                   , left   : 0.0
                   , right  : 0.0
                   , top    : 0.0
                   , bottom : 0.0
                   }
  }

-- | GraphData can hold data for multiple graphs with edges between them
type GraphData =
  { nodes :: Map NodeId Node
  , edges :: { sourceTarget :: Map NodeId (Map NodeId Edge)
             , targetSource :: Map NodeId (Map NodeId Edge)
             }
  , panes :: Map GraphId GraphView
  }

emptyGraphData :: GraphData
emptyGraphData =
  { nodes : Map.empty
  , edges : { sourceTarget : Map.empty
            , targetSource : Map.empty
            }
  , panes : Map.empty
  }


------
-- Lenses

_sourceTarget :: Lens' GraphData (Map NodeId (Map NodeId Edge))
_sourceTarget = prop (SProxy :: SProxy "edges") <<< prop (SProxy :: SProxy "sourceTarget")

_targetSource :: Lens' GraphData (Map NodeId (Map NodeId Edge))
_targetSource = prop (SProxy :: SProxy "edges") <<< prop (SProxy :: SProxy "targetSource")

_position :: NodeId -> Traversal' GraphData GraphSpacePoint2D
_position nodeId = prop (SProxy :: SProxy "nodes") <<< at nodeId <<< traversed <<< prop (SProxy :: SProxy "position")

_nodeText :: NodeId -> Traversal' GraphData String
_nodeText nodeId = prop (SProxy :: SProxy "nodes") <<< at nodeId <<< traversed <<< prop (SProxy :: SProxy "text")

_panes :: Lens' GraphData (Map GraphId GraphView)
_panes = prop (SProxy :: SProxy "panes")

_pane :: GraphId -> Traversal' GraphData GraphView
_pane graphId = _panes <<< at graphId <<< traversed

_zoom :: Lens' GraphView Number
_zoom = prop (SProxy :: SProxy "zoom")

_origin :: Lens' GraphView PageSpacePoint2D
_origin = prop (SProxy :: SProxy "origin")

_boundingRect :: Lens' GraphView WHE.DOMRect
_boundingRect = prop (SProxy :: SProxy "boundingRect")


------
-- Interface

insertNodeImpl :: GraphId -> NodeId -> GraphData -> GraphData
insertNodeImpl graphId nodeId graphData =
  graphData { nodes =
                 Map.insert
                 nodeId
                 (freshNode graphId nodeId)
                 graphData.nodes
            }

deleteNodeImpl :: NodeId -> GraphData -> GraphData
deleteNodeImpl nodeId graphData =
  graphData { nodes = Map.delete nodeId graphData.nodes }

insertEdgeImpl :: EdgeId -> GraphData -> GraphData
insertEdgeImpl edgeId graphData =
  let
    edge = freshEdge edgeId
    sourceTarget =
      case Map.lookup edgeId.source graphData.edges.sourceTarget of
        Nothing ->
          graphData.edges.sourceTarget # at edgeId.source ?~ Map.singleton edgeId.target edge
        Just targets ->
          graphData.edges.sourceTarget # at edgeId.source <<< traversed <<< at edgeId.target ?~ edge
    targetSource =
      case Map.lookup edgeId.target graphData.edges.targetSource of
        Nothing ->
          graphData.edges.targetSource # at edgeId.target ?~ Map.singleton edgeId.source edge
        Just sources ->
          graphData.edges.targetSource # at edgeId.target <<< traversed <<< at edgeId.source ?~ edge
  in
    graphData { edges = { sourceTarget : sourceTarget, targetSource : targetSource } }

batchInsertEdges :: GraphData -> Array Edge -> GraphData
batchInsertEdges = foldr (\edge -> insertEdgeImpl edge.id >>> updateEdgeData (const edge) edge.id)

updateEdgeData :: (Edge -> Edge) -> EdgeId -> GraphData -> GraphData
updateEdgeData updater edgeId =
  (_sourceTarget <<< at edgeId.source <<< traversed <<< at edgeId.target <<< traversed %~ updater)
  >>>
  (_targetSource <<< at edgeId.target <<< traversed <<< at edgeId.source <<< traversed %~ updater)

deleteEdgeImpl :: EdgeId -> GraphData -> GraphData
deleteEdgeImpl edgeId =
  (_sourceTarget <<< at edgeId.source <<< traversed <<< at edgeId.target .~ Nothing)
  >>>
  (_targetSource <<< at edgeId.target <<< traversed <<< at edgeId.source .~ Nothing)

moveNodeImpl :: NodeId -> GraphSpacePoint2D -> GraphData -> GraphData
moveNodeImpl nodeId newPos = _position nodeId .~ newPos

updateNodeTextImpl :: NodeId -> String -> GraphData -> GraphData
updateNodeTextImpl nodeId newText = _nodeText nodeId .~ newText

updateEdgeTextImpl :: EdgeId -> String -> GraphData -> GraphData
updateEdgeTextImpl edgeId newText = updateEdgeData _{ text = newText } edgeId


------
-- Utilities

allEdgesTouchingNode :: NodeId -> GraphData -> { incoming :: Array Edge, outgoing :: Array Edge }
allEdgesTouchingNode nodeId graphData =
  let
    outgoingEdges = case Map.lookup nodeId graphData.edges.sourceTarget of
      Nothing -> []
      Just targetEdgeMap -> Array.fromFoldable $ Map.values targetEdgeMap
    incomingEdges = case Map.lookup nodeId graphData.edges.targetSource of
      Nothing -> []
      Just targetEdgeMap -> Array.fromFoldable $ Map.values targetEdgeMap
  in
    { outgoing : outgoingEdges
    , incoming : incomingEdges
    }

edgeArray :: GraphData -> Array Edge
edgeArray graphData = Array.fromFoldable do
  targetEdgeMap <- Map.values graphData.edges.sourceTarget
  edge <- Map.values targetEdgeMap
  pure edge

edgeIdMap :: GraphData -> Map EdgeId Edge
edgeIdMap graphData =
  Map.fromFoldable $ (\edge -> Tuple edge.id edge) <$> edgeArray graphData

allEdgesBetweenGraphs :: GraphData -> Array Edge
allEdgesBetweenGraphs =
  edgeArray >>> Array.filter \edge -> edge.id.sourceGraph /= edge.id.targetGraph

separateGraphs :: GraphData -> Map GraphId GraphData
separateGraphs graphSmoosh =
  graphIds
  # Array.mapMaybe (\graphId -> Tuple graphId <$> selectGraphData graphId graphSmoosh)
  # Map.fromFoldable
  where
    graphIds = Array.fromFoldable $ Map.keys graphSmoosh.panes

selectGraphData :: GraphId -> GraphData -> Maybe GraphData
selectGraphData graphId graphData =
  let
    singleGraphNodes = Map.filter (\node -> node.graphId == graphId) graphData.nodes

    singleGraphEdges = do
      nodeId <- Array.fromFoldable $ Map.keys singleGraphNodes
      let nodeEdges = allEdgesTouchingNode nodeId graphData
      nodeEdges.incoming <> nodeEdges.outgoing

    newGraphData = batchInsertEdges
                   (emptyGraphData { nodes = singleGraphNodes })
                   singleGraphEdges
  in do
    pane <- Map.lookup graphId graphData.panes
    pure $ newGraphData { panes = Map.singleton graphId pane }

mergeGraphData :: GraphData -> GraphData -> GraphData
mergeGraphData graphDataA graphDataB =
  let
    edgesA = edgeArray graphDataA
    edgesB = edgeArray graphDataB
    mergedWithoutEdges =
      emptyGraphData
        { nodes = Map.union graphDataA.nodes graphDataB.nodes
        , panes = Map.union graphDataA.panes graphDataB.panes
        }
  in
    batchInsertEdges mergedWithoutEdges $ edgesA <> edgesB

graphTitle :: GraphData -> Maybe String
graphTitle graphData =
  List.head titles >>= String.stripPrefix titlePattern
  where
    titlePattern = (Pattern "Title: ")
    titles = List.filter (String.contains titlePattern) nodesText
    nodesText =
      graphData.nodes
      # Map.values
      <#> _.text
      <#> String.trim
