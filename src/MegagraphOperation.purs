module MegagraphOperation where

import Prelude

import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Foreign.Class (class Encode, class Decode)
import Foreign.Generic (genericEncode, genericDecode, defaultOptions)
import Megagraph (Edge, EdgeId, EdgeMappingEdge, EdgeMetadata, Graph, GraphEdgeSpacePoint2D(..), GraphId, GraphSpacePoint2D(..), Mapping, MappingId, Node, NodeId, NodeMappingEdge, PageEdgeSpacePoint2D(..), PathEquation, edgeArray, edgeMidpoint, nodePosition)


-- | An operation on a graph is stored with the before/after values of the updated field
-- | to allow it to be inverted.
data GraphOperation
  -- GraphOp name   | target node/edge | pre-op state         | post-op state
  = InsertNode        NodeId
  | UpdateNode                           Node                   Node
  | DeleteNode        NodeId
  | InsertEdge        EdgeMetadata
  | UpdateEdge                           Edge                   Edge
  | DeleteEdge        EdgeMetadata
  | MoveNode          NodeId             GraphSpacePoint2D      GraphSpacePoint2D
  | UpdateNodeText    NodeId             String                 String
  | UpdateEdgeText    EdgeId             String                 String
  | MoveEdgeMidpoint  EdgeId             GraphEdgeSpacePoint2D  GraphEdgeSpacePoint2D
  | ConnectSubgraph   NodeId             (Maybe GraphId)        (Maybe GraphId)
  | UpdateTitle                          String                 String
  | SetTitleValidity                     Boolean                Boolean

data EquationOperation
  -- EquationOp name   | target graph | equation
  = InsertPathEquation   PathEquation
  | DeletePathEquation   PathEquation

data MappingOperation
  -- MappingOp name            | element
  = InsertNodeMappingEdge        NodeMappingEdge
  | UpdateNodeMappingEdge                        NodeMappingEdge      NodeMappingEdge
  | DeleteNodeMappingEdge        NodeMappingEdge
  | InsertEdgeMappingEdge        EdgeMappingEdge
  | UpdateEdgeMappingEdge                        EdgeMappingEdge      EdgeMappingEdge
  | DeleteEdgeMappingEdge        EdgeMappingEdge
  | MoveNodeMappingEdgeMidpoint  EdgeId          PageEdgeSpacePoint2D PageEdgeSpacePoint2D
  | MoveEdgeMappingEdgeMidpoint  EdgeId          PageEdgeSpacePoint2D PageEdgeSpacePoint2D

data CreateOperation
  = CreateGraph GraphId String
  | DeleteGraph GraphId String
  | CreateMapping MappingId GraphId GraphId
  | DeleteMapping MappingId GraphId GraphId

data MegagraphOperation
  = GraphElementOperation GraphId GraphOperation
  | GraphElementEquationOperation GraphId EquationOperation
  | MappingElementOperation MappingId MappingOperation
  | CreateElementOperation CreateOperation

-- | Operations can be grouped together into a single update.
-- | However, the interpreter converts each operation into an update before they
-- | are actioned. If a single node or edge has multiple operations in a single
-- | update, they must be grouped into an update operation, or the earlier
-- | operations will be ignored.
type MegagraphUpdate = Array MegagraphOperation

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

derive instance genericCreateOperation :: Generic CreateOperation _
instance decodeCreateOperation :: Decode CreateOperation where
  decode = genericDecode defaultOptions
instance encodeCreateOperation :: Encode CreateOperation where
  encode = genericEncode defaultOptions

derive instance genericMegagraphOperation :: Generic MegagraphOperation _
instance decodeMegagraphOperation :: Decode MegagraphOperation where
  decode = genericDecode defaultOptions
instance encodeMegagraphOperation :: Encode MegagraphOperation where
  encode = genericEncode defaultOptions

invertGraphOperation :: GraphOperation -> GraphOperation
invertGraphOperation = case _ of
  InsertNode       nodeId               -> DeleteNode       nodeId
  DeleteNode       nodeId               -> InsertNode       nodeId
  UpdateNode                    from to -> UpdateNode                    to from
  InsertEdge       edgeMetadata         -> DeleteEdge       edgeMetadata
  DeleteEdge       edgeMetadata         -> InsertEdge       edgeMetadata
  UpdateEdge                    from to -> UpdateEdge                    to from
  MoveNode         nodeId       from to -> MoveNode         nodeId       to  from
  UpdateNodeText   nodeId       from to -> UpdateNodeText   nodeId       to  from
  UpdateEdgeText   edgeMetadata from to -> UpdateEdgeText   edgeMetadata to  from
  MoveEdgeMidpoint edgeId       from to -> MoveEdgeMidpoint edgeId       from to
  ConnectSubgraph  nodeId       old new -> ConnectSubgraph  nodeId       new old
  UpdateTitle                   from to -> UpdateTitle                   to  from
  SetTitleValidity              from to -> SetTitleValidity              to  from

invertEquationOperation :: EquationOperation -> EquationOperation
invertEquationOperation = case _ of
  InsertPathEquation pathEquation -> DeletePathEquation pathEquation
  DeletePathEquation pathEquation -> InsertPathEquation pathEquation

invertMappingOperation :: MappingOperation -> MappingOperation
invertMappingOperation = case _ of
  InsertNodeMappingEdge nodeMappingEdge  -> DeleteNodeMappingEdge nodeMappingEdge
  DeleteNodeMappingEdge nodeMappingEdge  -> InsertNodeMappingEdge nodeMappingEdge
  UpdateNodeMappingEdge          from to -> UpdateNodeMappingEdge          to from
  InsertEdgeMappingEdge edgeMappingEdge  -> DeleteEdgeMappingEdge edgeMappingEdge
  DeleteEdgeMappingEdge edgeMappingEdge  -> InsertEdgeMappingEdge edgeMappingEdge
  UpdateEdgeMappingEdge          from to -> UpdateEdgeMappingEdge          to from
  MoveNodeMappingEdgeMidpoint id from to -> MoveNodeMappingEdgeMidpoint id to from
  MoveEdgeMappingEdgeMidpoint id from to -> MoveEdgeMappingEdgeMidpoint id to from

invertCreateOperation :: CreateOperation -> CreateOperation
invertCreateOperation = case _ of
  CreateGraph graphId title -> DeleteGraph graphId title
  DeleteGraph graphId title -> CreateGraph graphId title
  CreateMapping mappingId from to -> DeleteMapping mappingId from to
  DeleteMapping mappingId from to -> CreateMapping mappingId from to

invertMegagraphOperation :: MegagraphOperation -> MegagraphOperation
invertMegagraphOperation = case _ of
  GraphElementOperation graphId graphOp ->
    GraphElementOperation graphId $ invertGraphOperation graphOp
  GraphElementEquationOperation graphId equationOp ->
    GraphElementEquationOperation graphId $ invertEquationOperation equationOp
  MappingElementOperation mappingId mappingOp ->
    MappingElementOperation mappingId $ invertMappingOperation mappingOp
  CreateElementOperation createOp ->
    CreateElementOperation $ invertCreateOperation createOp

invertMegagraphUpdate :: MegagraphUpdate -> MegagraphUpdate
invertMegagraphUpdate =
  Array.reverse <<< map invertMegagraphOperation

instance showGraphOperation :: Show GraphOperation where
  show = case _ of
    InsertNode nodeId ->
      "InsertNode " <> show nodeId
    DeleteNode nodeId ->
      "DeleteNode " <> show nodeId
    UpdateNode from to ->
      "UpdateNode from: " <> show from <> " to: " <> show to
    InsertEdge edge ->
      "InsertEdge edge: " <> show edge
    DeleteEdge edge ->
      "DeleteEdge edge: " <> show edge
    UpdateEdge from to ->
      "UpdateEdge from: " <> show from <> " to: " <> show to
    MoveNode nodeId from to ->
      "MoveNode node: " <> show nodeId <> " from: " <> show from <> " to: " <> show to
    UpdateNodeText nodeId from to ->
      "UpdateNodeText node: " <> show nodeId <> " from: " <> from <> " to: " <> to
    UpdateEdgeText edgeId from to ->
      "UpdateEdgeText edge: " <> show edgeId <> " from: " <> from <> " to: " <> to
    MoveEdgeMidpoint edgeId from to ->
      "MoveEdgeMidpoint edge: " <> show edgeId <> " from: " <> show from <> " to: " <> show to
    ConnectSubgraph nodeId old new ->
      "ConnectSubgraph of node: " <> show nodeId <> " to graph: " <> show new
    UpdateTitle from to ->
      "UpdateTitle from " <> from <> " to: " <> to
    SetTitleValidity from to ->
      "SetTitleValidity from " <> show from <> " to: " <> show to

instance showEquationOperation :: Show EquationOperation where
  show = case _ of
    InsertPathEquation pathEquation ->
      "InsertPathEquation " <> show pathEquation
    DeletePathEquation pathEquation ->
      "DeletePathEquation " <> show pathEquation

instance showMappingOperation :: Show MappingOperation where
  show = case _ of
    InsertNodeMappingEdge nodeMappingEdge ->
      "InsertNodeMappingEdge " <> show nodeMappingEdge
    DeleteNodeMappingEdge nodeMappingEdge ->
      "DeleteNodeMappingEdge " <> show nodeMappingEdge
    UpdateNodeMappingEdge from to ->
      "UpdateNodeMappingEdge from: " <> show from <> " to: " <> show to
    InsertEdgeMappingEdge edgeMappingEdge ->
      "InsertEdgeMappingEdge " <> show edgeMappingEdge
    DeleteEdgeMappingEdge edgeMappingEdge ->
      "DeleteEdgeMappingEdge " <> show edgeMappingEdge
    UpdateEdgeMappingEdge from to ->
      "UpdateEdgeMappingEdge from: " <> show from <> " to: " <> show to
    MoveNodeMappingEdgeMidpoint id from to ->
      "MoveNodeMappingEdgeMidpoint " <> show id <> " to: " <> show to <> " from: " <> show from
    MoveEdgeMappingEdgeMidpoint id from to ->
      "MoveEdgeMappingEdgeMidpoint " <> show id <> " to: " <> show to <> " from: " <> show from

instance showCreateOperation :: Show CreateOperation where
  show = case _ of
    CreateGraph graphId title ->
      "CreateGraph " <> show graphId <> " with title: " <> title
    DeleteGraph graphId title ->
      "DeleteGraph " <> show graphId <> " with title: " <> title
    CreateMapping mappingId from to ->
      "CreateMapping " <> show mappingId <> " from: " <> show from <> " to: " <> show to
    DeleteMapping mappingId from to ->
      "DeleteGraph " <> show mappingId <> " from: " <> show from <> " to: " <> show to

instance showMegagraphOperation :: Show MegagraphOperation where
  show = case _ of
    GraphElementOperation graphId graphOp ->
      "GraphElementOperation on graph " <> show graphId <> " " <> show graphOp
    GraphElementEquationOperation graphId equationOp ->
      "GraphElementEquationOperation on graph " <> show graphId <> " " <> show equationOp
    MappingElementOperation mappingId mappingOp ->
      "MappingElementOperation on mapping " <> show mappingId <> " " <> show mappingOp
    CreateElementOperation createOp ->
      "CreateELementOperation " <> show createOp

encodeNodeAsGraphOperations :: Node -> Array GraphOperation
encodeNodeAsGraphOperations node =
  [ InsertNode node.id
  , MoveNode node.id (GraphSpacePoint2D {x: 0.0, y: 0.0}) (nodePosition node)
  , UpdateNodeText node.id "" node.text
  , ConnectSubgraph node.id Nothing node.subgraph
  ]

encodeNodeAsMegagraphUpdate :: Node -> MegagraphUpdate
encodeNodeAsMegagraphUpdate node =
  GraphElementOperation node.graphId <$> encodeNodeAsGraphOperations node

encodeEdgeAsGraphOperations :: Edge -> Array GraphOperation
encodeEdgeAsGraphOperations edge =
  let
    edgeMetadata = {id: edge.id, graphId: edge.graphId, source: edge.source, target: edge.target}
  in
   [ InsertEdge edgeMetadata
   , UpdateEdgeText edge.id "" edge.text
   , MoveEdgeMidpoint edge.id (GraphEdgeSpacePoint2D {angle: 0.0, radius: 0.0}) (edgeMidpoint edge)
   ]

encodeEdgeAsMegagraphUpdate :: Edge -> MegagraphUpdate
encodeEdgeAsMegagraphUpdate edge =
  GraphElementOperation edge.graphId <$> encodeEdgeAsGraphOperations edge

encodePathEquationAsMegagraphUpdate :: GraphId -> PathEquation -> MegagraphUpdate
encodePathEquationAsMegagraphUpdate graphId pathEquation =
  [ GraphElementEquationOperation graphId $ InsertPathEquation pathEquation ]

encodeGraphAsMegagraphUpdate :: Graph -> MegagraphUpdate
encodeGraphAsMegagraphUpdate graph =
  let
    nodeOps = encodeNodeAsMegagraphUpdate <$> (Array.fromFoldable $ Map.values graph.nodes)
    edgeOps = encodeEdgeAsMegagraphUpdate <$> edgeArray graph
    pathEquationOps = encodePathEquationAsMegagraphUpdate graph.id <$> Array.fromFoldable graph.pathEquations
    titleOp = [ [ GraphElementOperation graph.id $ UpdateTitle "" graph.title.text ] ]
  in
    Array.concat $ nodeOps <> edgeOps <> pathEquationOps <> titleOp

encodeNodeMappingEdgeAsMegagraphUpdate :: NodeMappingEdge -> MegagraphUpdate
encodeNodeMappingEdgeAsMegagraphUpdate nodeMappingEdge =
  MappingElementOperation nodeMappingEdge.mappingId
  <$> [ InsertNodeMappingEdge nodeMappingEdge
      , MoveNodeMappingEdgeMidpoint
          nodeMappingEdge.id
          (PageEdgeSpacePoint2D {angle: 0.0, radius: 0.0})
          nodeMappingEdge.midpoint
      ]

encodeEdgeMappingEdgeAsMegagraphUpdate :: EdgeMappingEdge -> MegagraphUpdate
encodeEdgeMappingEdgeAsMegagraphUpdate edgeMappingEdge =
  MappingElementOperation edgeMappingEdge.mappingId
  <$> [ InsertEdgeMappingEdge edgeMappingEdge
      , MoveEdgeMappingEdgeMidpoint
          edgeMappingEdge.id
          (PageEdgeSpacePoint2D {angle: 0.0, radius: 0.0})
          edgeMappingEdge.midpoint
      ]

encodeMappingAsMegagraphUpdate :: Mapping -> MegagraphUpdate
encodeMappingAsMegagraphUpdate mapping =
  let
    nodeMappingEdgesOps = encodeNodeMappingEdgeAsMegagraphUpdate <$> Array.fromFoldable mapping.nodeMappingEdges
    edgeMappingEdgesOps = encodeEdgeMappingEdgeAsMegagraphUpdate <$> Array.fromFoldable mapping.edgeMappingEdges
  in
    Array.concat $ nodeMappingEdgesOps <> edgeMappingEdgesOps
