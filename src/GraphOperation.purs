-- | GraphOp DSL for making UI interactions undoable and streamable
module GraphOperation where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.UUID as UUID
import Foreign.Class (class Encode, class Decode)
import Foreign.Generic (genericEncode, genericDecode, defaultOptions)
import Graph (GraphSpacePoint2D, GraphId, NodeId, EdgeMetadata, PathEquation)


-- | An operation on a graph is stored with the before/after values of the updated field
-- | to allow it to be inverted.
data GraphOperation
  -- GraphOp name   | target graph | target node/edge | pre-op state       | post-op state
  = InsertNode        GraphId        NodeId
  | DeleteNode        GraphId        NodeId
  | InsertEdge        GraphId        EdgeMetadata
  | DeleteEdge        GraphId        EdgeMetadata
  | MoveNode          GraphId        NodeId             GraphSpacePoint2D    GraphSpacePoint2D
  | UpdateNodeText    GraphId        NodeId             String               String
  | UpdateEdgeText    GraphId        EdgeMetadata       String               String
  | UpdateTitle       GraphId                           String               String
  | SetTitleValidity  GraphId                           Boolean              Boolean
  | ConnectSubgraph   GraphId        NodeId             (Maybe GraphId)      (Maybe GraphId)
  -- EquationOp name | target graph | equation
  | InsertPathEquation GraphId        PathEquation
  | DeletePathEquation GraphId        PathEquation
  -- MappingOp name       | mapping id | source graph | target graph | source element | target element
  | InsertNodeMappingEdge   MappingId    GraphId        GraphId        NodeId           NodeId
  | DeleteNodeMappingEdge   MappingId    GraphId        GraphId        NodeId           NodeId
  | InsertEdgeMappingEdge   MappingId    GraphId        GraphId        EdgeId           EdgeId
  | DeleteEdgeMappingEdge   MappingId    GraphId        GraphId        EdgeId           EdgeId

derive instance genericGraphOperation :: Generic GraphOperation _

instance decodeGraphOperation :: Decode GraphOperation where
  decode = genericDecode defaultOptions

instance encodeGraphOperation :: Encode GraphOperation where
  encode = genericEncode defaultOptions

type GraphUpdate = Array GraphOperation

invertGraphOperation :: GraphOperation -> GraphOperation
invertGraphOperation = case _ of
  -- Graph Operations
  InsertNode         graphId nodeId               -> DeleteNode         graphId nodeId
  DeleteNode         graphId nodeId               -> InsertNode         graphId nodeId
  InsertEdge         graphId edgeMetadata         -> DeleteEdge         graphId edgeMetadata
  DeleteEdge         graphId edgeMetadata         -> InsertEdge         graphId edgeMetadata
  MoveNode           graphId nodeId       from to -> MoveNode           graphId nodeId       to  from
  UpdateNodeText     graphId nodeId       from to -> UpdateNodeText     graphId nodeId       to  from
  UpdateEdgeText     graphId edgeMetadata from to -> UpdateEdgeText     graphId edgeMetadata to  from
  UpdateTitle        graphId              from to -> UpdateTitle        graphId              to  from
  SetTitleValidity   graphId              from to -> SetTitleValidity   graphId              to  from
  ConnectSubgraph    graphId nodeId       old new -> ConnectSubgraph    graphId nodeId       new old
  InsertPathEquation graphId pathEquation         -> DeletePathEquation graphId pathEquation
  DeletePathEquation graphId pathEquation         -> InsertPathEquation graphId pathEquation
  -- Mapping Operations
  InsertNodeMappingEdge mappingId sourceGraph targetGraph sourceNode targetNode ->
    DeleteNodeMappingEdge mappingId sourceGraph targetGraph sourceNode targetNode
  DeleteNodeMappingEdge mappingId sourceGraph targetGraph sourceNode targetNode ->
    InsertNodeMappingEdge mappingId sourceGraph targetGraph sourceNode targetNode
  InsertEdgeMappingEdge mappingId sourceGraph targetGraph sourceEdge targetEdge ->
    DeleteEdgeMappingEdge mappingId sourceGraph targetGraph sourceEdge targetEdge
  DeleteEdgeMappingEdge mappingId sourceGraph targetGraph sourceEdge targetEdge ->
    InsertEdgeMappingEdge mappingId sourceGraph targetGraph sourceEdge targetEdge

-- | Certain actions, if repeated, should be collapsed together for the sake of
-- | undoing them both at once. Examples:
-- | - dragging
-- | - updating a text field
collapse :: GraphOperation -> GraphOperation -> Maybe GraphOperation
collapse nextOp prevOp =
  case nextOp, prevOp of
    MoveNode nextNodeId  middlePos lastPos,
    MoveNode firstNodeId firstPos  middlePos' ->
      if nextNodeId == firstNodeId
      then Just $ MoveNode firstNodeId firstPos lastPos
      else Nothing

    UpdateNodeText nextNodeId  middleText lastText,
    UpdateNodeText firstNodeId firstText  middleText' ->
      if nextNodeId == firstNodeId
      then Just $ UpdateNodeText firstNodeId firstText lastText
      else Nothing

    UpdateEdgeText nextEdgeId  middleText lastText,
    UpdateEdgeText firstEdgeId firstText  middleText' ->
      if nextEdgeId == firstEdgeId
      then Just $ UpdateEdgeText firstEdgeId firstText lastText
      else Nothing

    UpdateTitle nextGraphId  middleTitle lastTitle,
    UpdateTitle firstGraphId firstTitle  middleTitle' ->
      if nextGraphId == firstGraphId
      then Just $ UpdateTitle firstGraphId firstTitle lastTitle
      else Nothing
    _, _ -> Nothing

showGraphOperation :: GraphOperation -> String
showGraphOperation = case _ of
  InsertNode graphId nodeId ->
    "InsertNode graph: " <> show graphId <> " node: " <> show nodeId
  DeleteNode graphId nodeId ->
    "DeleteNode graph: " <> show graphId <> " node: " <> show nodeId
  InsertEdge edge ->
    "InsertEdge edge: " <> show edge
  DeleteEdge edge ->
    "DeleteEdge edge: " <> show edge
  MoveNode nodeId from to ->
    "MoveNode node: " <> show nodeId <> " from: " <> show from <> " to: " <> show to
  UpdateNodeText nodeId from to ->
    "UpdateNodeText node: " <> show nodeId <> " from: " <> from <> " to: " <> to
  UpdateEdgeText edgeId from to ->
    "UpdateEdgeText node: " <> show edgeId <> " from: " <> from <> " to: " <> to
  UpdateTitle graphId from to ->
    "UpdateTitle graph: " <> show graphId <> " from " <> from <> " to: " <> to
  SetTitleValidity graphId from to ->
    "SetTitleValidity graph: " <> show graphId <> " from " <> show from <> " to: " <> show to
  ConnectSubgraph nodeId old new ->
    "ConnectSubgraph of node: " <> show nodeId <> " to graph: " <> show new
  InsertPathEquation graphId pathEquation ->
    "InsertPathEquation " <> show pathEquation <> " into graph: " <> show graphId
  DeletePathEquation graphId pathEquation -> InsertPathEquation graphId pathEquation
    "DeletePathEquation " <> show pathEquation <> " from graph: " <> show graphId
  InsertNodeMappingEdge mappingId sourceGraph targetGraph sourceNode targetNode ->
    "InsertNodeMappingEdge to mapping: " <> show mappingId
      <> "from graph: " <> show sourceGraph
      <> " to: " <> show targetGraph
      <> " from node: " <> show sourceNode
      <> " to: " <> targetNode
  DeleteNodeMappingEdge mappingId sourceGraph targetGraph sourceNode targetNode ->
    "DeleteNodeMappingEdge to mapping: " <> show mappingId
      <> "from graph: " <> show sourceGraph
      <> " to: " <> show targetGraph
      <> " from node: " <> show sourceNode
      <> " to: " <> targetNode
  InsertEdgeMappingEdge mappingId sourceGraph targetGraph sourceEdge targetEdge ->
    "InsertEdgeMappingEdge to mapping: " <> show mappingId
      <> "from graph: " <> show sourceGraph
      <> " to: " <> show targetGraph
      <> " from edge: " <> show sourceEdge
      <> " to: " <> targetEdge
  DeleteEdgeMappingEdge mappingId sourceGraph targetGraph sourceEdge targetEdge ->
    "DeleteEdgeMappingEdge to mapping: " <> show mappingId
      <> "from graph: " <> show sourceGraph
      <> " to: " <> show targetGraph
      <> " from edge: " <> show sourceEdge
      <> " to: " <> targetEdge

encodeNodeAsGraphUpdate :: Node -> GraphUpdate
encodeNodeAsGraphUpdate node =
  [ InsertNode node.graphId node.id
  , MoveNode node.graphId node.id (GraphSpacePoint2D 0.0 0.0) node.position
  , UpdateNodeText node.graphId "" node.nodeText
  , ConnectSubgraph node.graphId Nothing node.subgraph
  ]

encodeEdgeAsGraphUpdate :: Edge -> GraphUpdate
encodeEdgeAsGraphUpdate edge =
  let
    edgeMetadata = {id: edge.id, graphId: edge.graphId, source: edge.source, target: edge.target}
  in
    [ InsertEdge edge.graphId edgeMetadata
    , UpdateEdgeText edge.graphId edgeMetadata "" edge.text
    ]

encodePathEquationAsGraphUpdate :: GraphId -> PathEquation -> GraphUpdate
encodePathEquationAsGraphUpdate graphId pathEquation =
  [ InsertPathEquation graphId pathEquation ]

encodeGraphDataAsGraphUpdate :: Graph -> GraphUpdate
encodeGraphDataAsGraphUpdate graph =
  let
    nodeOps = encodeNodeAsGraphOp <$> (Array.fromFoldable $ Map.values graph.nodes)
    edgeOps = encodeEdgeAsGraphOp <$> edgeArray graph
    pathEquationOps = Array.concatMap encodePathEquationAsGraphOperation (Array.fromFoldable graph.pathEquations)
    titleOp = UpdateTitle graph.id "" graph.title.text
  in
    Array.concat $ nodeOps <> edgeOps <> pathEquationOps <> titleOp
