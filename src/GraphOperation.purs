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
  -- GraphOp name   | target node/edge | pre-op state       | post-op state
  = InsertNode        NodeId
  | DeleteNode        NodeId
  | InsertEdge        EdgeMetadata
  | DeleteEdge        EdgeMetadata
  | MoveNode          NodeId             GraphSpacePoint2D    GraphSpacePoint2D
  | UpdateNodeText    NodeId             String               String
  | UpdateEdgeText    EdgeMetadata       String               String
  | ConnectSubgraph   NodeId             (Maybe GraphId)      (Maybe GraphId)
  | UpdateTitle                          String               String
  | SetTitleValidity                     Boolean              Boolean

data EquationOperation
  -- EquationOp name   | target graph | equation
  = InsertPathEquation   PathEquation
  | DeletePathEquation   PathEquation

data MappingOperation
  -- MappingOp name       | source element | target element
  = InsertNodeMappingEdge   NodeId           NodeId
  | DeleteNodeMappingEdge   NodeId           NodeId
  | InsertEdgeMappingEdge   EdgeId           EdgeId
  | DeleteEdgeMappingEdge   EdgeId           EdgeId

data SubMegagraphOperation
  = SubGraphOperation GraphId GraphOperation
  | SubGraphEquationOperation GraphId EquationOperation
  | SubMappingOperation MappingId MappingOperation

type SubMegagraphUpdate = Array SubMegagraphOperation

derive instance genericGraphOperation :: Generic GraphOperation _
instance decodeGraphOperation :: Decode GraphOperation where
  decode = genericDecode defaultOptions
instance encodeGraphOperation :: Encode GraphOperation where
  encode = genericEncode defaultOptions

derive instance genericEquationOperation :: Generic EquationOperation _
instance decodeEquationOperation :: Decode EquationOperation where
  decode = genericDecode defaultOptions
instance encodeEquationOperation :: Encode EquationOperation where
  encode = genericEncode defaultOptions

derive instance genericMappingOperation :: Generic MappingOperation _
instance decodeMappingOperation :: Decode MappingOperation where
  decode = genericDecode defaultOptions
instance encodeMappingOperation :: Encode MappingOperation where
  encode = genericEncode defaultOptions

invertGraphOperation :: GraphOperation -> GraphOperation
invertGraphOperation = case _ of
  InsertNode         graphId nodeId               -> DeleteNode         graphId nodeId
  DeleteNode         graphId nodeId               -> InsertNode         graphId nodeId
  InsertEdge         graphId edgeMetadata         -> DeleteEdge         graphId edgeMetadata
  DeleteEdge         graphId edgeMetadata         -> InsertEdge         graphId edgeMetadata
  MoveNode           graphId nodeId       from to -> MoveNode           graphId nodeId       to  from
  UpdateNodeText     graphId nodeId       from to -> UpdateNodeText     graphId nodeId       to  from
  UpdateEdgeText     graphId edgeMetadata from to -> UpdateEdgeText     graphId edgeMetadata to  from
  ConnectSubgraph    graphId nodeId       old new -> ConnectSubgraph    graphId nodeId       new old
  UpdateTitle        graphId              from to -> UpdateTitle        graphId              to  from
  SetTitleValidity   graphId              from to -> SetTitleValidity   graphId              to  from

invertEquationOperation :: EquationOperation -> EquationOperation
  InsertPathEquation pathEquation -> DeletePathEquation pathEquation
  DeletePathEquation pathEquation -> InsertPathEquation pathEquation

invertMappingOperation :: MappingOperation -> MappingOperation
  InsertNodeMappingEdge sourceNode targetNode -> DeleteNodeMappingEdge sourceNode targetNode
  DeleteNodeMappingEdge sourceNode targetNode -> InsertNodeMappingEdge sourceNode targetNode
  InsertEdgeMappingEdge sourceEdge targetEdge -> DeleteEdgeMappingEdge sourceEdge targetEdge
  DeleteEdgeMappingEdge sourceEdge targetEdge -> InsertEdgeMappingEdge sourceEdge targetEdge

invertSubMegagraphOperation :: SubMegagraphOperation -> SubMegagraphOperation
invertSubMegagraphOperation = case _ of
  SubGraphOperation graphId graphOp ->
    SubGraphOperation graphId $ invertGraphOperation graphOp
  SubGraphEquationOperation graphId equationOp ->
    SubGraphEquationOperation graphId $ invertEquationOperation equationOp
  SubMappingOperation mappingId mappingOp ->
    SubMappingOperation mappingId $ invertMappingOperation mappingOp

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
  InsertNode nodeId ->
    "InsertNode " <> show nodeId
  DeleteNode nodeId ->
    "DeleteNode " <> show nodeId
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
  ConnectSubgraph old new ->
    "ConnectSubgraph of node: " <> show nodeId <> " to graph: " <> show new
  UpdateTitle from to ->
    "UpdateTitle from " <> from <> " to: " <> to
  SetTitleValidity from to ->
    "SetTitleValidity from " <> show from <> " to: " <> show to

showEquationOperation :: EquationOperation -> String
showEquationOperation = case _ of
  InsertPathEquation pathEquation ->
    "InsertPathEquation " <> show pathEquation
  DeletePathEquation pathEquation ->
    "DeletePathEquation " <> show pathEquation

showMappingOperation :: MappingOperation -> String
  InsertNodeMappingEdge sourceNode targetNode ->
    "InsertNodeMappingEdge from node: " <> show sourceNode <> " to: " <> targetNode
  DeleteNodeMappingEdge mappingId sourceGraph targetGraph sourceNode targetNode ->
    "DeleteNodeMappingEdge from node: " <> show sourceNode <> " to: " <> targetNode
  InsertEdgeMappingEdge mappingId sourceGraph targetGraph sourceEdge targetEdge ->
    "InsertEdgeMappingEdge from edge: " <> show sourceEdge <> " to: " <> targetEdge
  DeleteEdgeMappingEdge mappingId sourceGraph targetGraph sourceEdge targetEdge ->
    "DeleteEdgeMappingEdge from edge: " <> show sourceEdge <> " to: " <> targetEdge

showSubMegagraphOperation :: SubMegagraphOperation -> String
showSubMegagraphOperation = case _ of
  SubGraphOperation graphId graphOp ->
    "SubGraphOperation on graph " <> show graphId <> " " <> showGraphOperation graphOp
  SubGraphEquationOperation graphId equationOp ->
    "SubGraphEquationOperation on graph " <> show graphId <> " " <> showEquationOperation equationOp
  SubMappingOperation mappingId mappingOp ->
    "SubMappingOperation on mapping " <> show mappingId <> " " <> showMappingOperation mappingOp

encodeNodeAsSubMegagraphUpdate :: Node -> SubMegagraphUpdate
encodeNodeAsSubMegagraphUpdate node =
  [ SubGraphOperation node.graphid $ InsertNode node.id
  , SubGraphOperation node.graphId $ MoveNode node.id (GraphSpacePoint2D 0.0 0.0) node.position
  , SubGraphOperation node.graphId $ UpdateNodeText "" node.nodeText
  , SubGraphOperation node.graphId $ ConnectSubgraph Nothing node.subgraph
  ]

encodeEdgeAsSubMegagraphUpdate :: Edge -> SubMegagraphUpdate
encodeEdgeAsSubMegagraphUpdate edge =
  let
    edgeMetadata = {id: edge.id, graphId: edge.graphId, source: edge.source, target: edge.target}
  in
    [ SubGraphOperation edge.graphId $ InsertEdge edgeMetadata
    , SubGraphOperation edge.graphId $ UpdateEdgeText edgeMetadata "" edge.text
    ]

encodePathEquationAsSubMegagraphUpdate :: GraphId -> PathEquation -> SubMegagraphUpdate
encodePathEquationAsSubMegagraphUpdate graphId pathEquation =
  [ SubGraphEquationOperation graphId $ InsertPathEquation pathEquation ]

encodeGraphAsSubMegagraphUpdate :: Graph -> SubMegagraphUpdate
encodeGraphAsSubMegagraphUpdate graph =
  let
    nodeOps = encodeNodeAsSubMegagraphUpdate <$> (Array.fromFoldable $ Map.values graph.nodes)
    edgeOps = encodeEdgeAsSubMegagraphUpdate <$> edgeArray graph
    pathEquationOps = Array.concatMap encodePathEquationAsSubMegagraphUpdate (Array.fromFoldable graph.pathEquations)
    titleOp = [ SubGraphOperation graph.id $ UpdateTitle "" graph.title.text ]
  in
    Array.concat $ nodeOps <> edgeOps <> pathEquationOps <> titleOp

encodeNodeMappingEdgeAsSubMegagraphUpdate :: MappingId -> NodeMappingEdge -> SubMegagraphUpdate
encodeNodeMappingEdgeAsSubMegagraphUpdate mappingId nodeMappingEdge =
  [ SubMappingOperation mappingId $ InsertNodeMappingEdge nodeMappingEdge.sourceNode nodeMappingEdge.targetNode ]

encodeEdgeMappingEdgeAsSubMegagraphUpdate :: MappingId -> EdgeMappingEdge -> SubMegagraphUpdate
encodeEdgeMappingEdgeAsSubMegagraphUpdate mappingId edgeMappingEdge =
  [ SubMappingOperation mappingId $ InsertEdgeMappingEdge edgeMappingEdge.sourceEdge edgeMappingEdge.targetEdge ]

encodeMappingAsSubMegagraphUpdate :: Mapping -> SubMegagraphUpdate
encodeMappingAsSubMegagraphUpdate mapping =
  let
    nodeMappingEdgesOps = Set.map encodeNodeMappingEdgeAsSubMegagraphUpdate mapping.nodeMappingEdges
    edgeMappingEdgesOps = Set.map encodeEdgeMappingEdgeAsSubMegagraphUpdate mapping.nodeMappingEdges
  in
    Array.concat $ nodeMappingEdgesOps <> edgeMappingEdgesOps
