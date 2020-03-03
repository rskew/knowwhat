module MegagraphOperation where

import Prelude

import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Foreign.Class (class Encode, class Decode)
import Foreign.Generic (genericEncode, genericDecode, defaultOptions)
import Megagraph (Edge, EdgeMappingEdge, Graph, GraphId, Mapping, MappingId, Node, NodeMappingEdge, PathEquation, edgeArray)


-- | An operation on a graph is stored with the before/after values of the updated field
-- | to allow it to be inverted.
data GraphOperation
  -- GraphOp name   | target node/edge | pre-op state         | post-op state
  = InsertNodes       (Array Node)
  | UpdateNodes                          (Array Node)           (Array Node)
  | DeleteNodes       (Array Node)
  | InsertEdges       (Array Edge)
  | UpdateEdges                          (Array Edge)           (Array Edge)
  | DeleteEdges       (Array Edge)
  | UpdateTitle                          String                 String
  | SetTitleValidity                     Boolean                Boolean

data EquationOperation
  -- EquationOp name   | target graph | equation
  = InsertPathEquations  (Array PathEquation)
  | DeletePathEquations  (Array PathEquation)

data MappingOperation
  -- MappingOp name        | element                 | from                    | to
  = InsertNodeMappingEdges   (Array NodeMappingEdge)
  | UpdateNodeMappingEdges                             (Array NodeMappingEdge) (Array NodeMappingEdge)
  | DeleteNodeMappingEdges   (Array NodeMappingEdge)
  | InsertEdgeMappingEdges   (Array EdgeMappingEdge)
  | UpdateEdgeMappingEdges                             (Array EdgeMappingEdge) (Array EdgeMappingEdge)
  | DeleteEdgeMappingEdges   (Array EdgeMappingEdge)

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
  InsertNodes      node                 -> DeleteNodes      node
  DeleteNodes      node                 -> InsertNodes      node
  UpdateNodes                   from to -> UpdateNodes                   to from
  InsertEdges      edgeMetadata         -> DeleteEdges      edgeMetadata
  DeleteEdges      edgeMetadata         -> InsertEdges      edgeMetadata
  UpdateEdges                   from to -> UpdateEdges                   to from
  UpdateTitle                   from to -> UpdateTitle                   to  from
  SetTitleValidity              from to -> SetTitleValidity              to  from

invertEquationOperation :: EquationOperation -> EquationOperation
invertEquationOperation = case _ of
  InsertPathEquations pathEquations -> DeletePathEquations pathEquations
  DeletePathEquations pathEquations -> InsertPathEquations pathEquations

invertMappingOperation :: MappingOperation -> MappingOperation
invertMappingOperation = case _ of
  InsertNodeMappingEdges nodeMappingEdges -> DeleteNodeMappingEdges nodeMappingEdges
  DeleteNodeMappingEdges nodeMappingEdges -> InsertNodeMappingEdges nodeMappingEdges
  UpdateNodeMappingEdges from to          -> UpdateNodeMappingEdges to from
  InsertEdgeMappingEdges edgeMappingEdges -> DeleteEdgeMappingEdges edgeMappingEdges
  DeleteEdgeMappingEdges edgeMappingEdges -> InsertEdgeMappingEdges edgeMappingEdges
  UpdateEdgeMappingEdges from to          -> UpdateEdgeMappingEdges to from

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
    InsertNodes nodes ->
      "InsertNodes " <> show nodes
    DeleteNodes nodes ->
      "DeleteNodes " <> show nodes
    UpdateNodes from to ->
      "UpdateNodes from: " <> show from <> " to: " <> show to
    InsertEdges edges ->
      "InsertEdges edges: " <> show edges
    DeleteEdges edges ->
      "DeleteEdges edges: " <> show edges
    UpdateEdges from to ->
      "UpdateEdges from: " <> show from <> " to: " <> show to
    UpdateTitle from to ->
      "UpdateTitle from " <> from <> " to: " <> to
    SetTitleValidity from to ->
      "SetTitleValidity from " <> show from <> " to: " <> show to

instance showEquationOperation :: Show EquationOperation where
  show = case _ of
    InsertPathEquations pathEquations ->
      "InsertPathEquations " <> show pathEquations
    DeletePathEquations pathEquations ->
      "DeletePathEquations " <> show pathEquations

instance showMappingOperation :: Show MappingOperation where
  show = case _ of
    InsertNodeMappingEdges nodeMappingEdges ->
      "InsertNodeMappingEdges " <> show nodeMappingEdges
    DeleteNodeMappingEdges nodeMappingEdges ->
      "DeleteNodeMappingEdges " <> show nodeMappingEdges
    UpdateNodeMappingEdges from to ->
      "UpdateNodeMappingEdges from: " <> show from <> " to: " <> show to
    InsertEdgeMappingEdges edgeMappingEdges ->
      "InsertEdgeMappingEdges " <> show edgeMappingEdges
    DeleteEdgeMappingEdges edgeMappingEdges ->
      "DeleteEdgeMappingEdges " <> show edgeMappingEdges
    UpdateEdgeMappingEdges from to ->
      "UpdateEdgeMappingEdges from: " <> show from <> " to: " <> show to

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

encodeNodesAsMegagraphUpdate :: GraphId -> Array Node -> MegagraphUpdate
encodeNodesAsMegagraphUpdate graphId nodes =
  [GraphElementOperation graphId $ InsertNodes nodes]

encodeEdgesAsMegagraphUpdate :: GraphId -> Array Edge -> MegagraphUpdate
encodeEdgesAsMegagraphUpdate graphId edges =
  [GraphElementOperation graphId $ InsertEdges edges]

encodePathEquationsAsMegagraphUpdate :: GraphId -> Array PathEquation -> MegagraphUpdate
encodePathEquationsAsMegagraphUpdate graphId pathEquations =
  [ GraphElementEquationOperation graphId $ InsertPathEquations pathEquations ]

encodeGraphAsMegagraphUpdate :: Graph -> MegagraphUpdate
encodeGraphAsMegagraphUpdate graph =
  let
    nodeOps = encodeNodesAsMegagraphUpdate graph.id (Array.fromFoldable $ Map.values graph.nodes)
    edgeOps = encodeEdgesAsMegagraphUpdate graph.id (edgeArray graph)
    pathEquationOps = encodePathEquationsAsMegagraphUpdate graph.id $ Array.fromFoldable graph.pathEquations
    titleOp = [ GraphElementOperation graph.id $ UpdateTitle "" graph.title.text ]
  in
    nodeOps <> edgeOps <> pathEquationOps <> titleOp

encodeNodeMappingEdgesAsMegagraphUpdate :: MappingId -> Array NodeMappingEdge -> MegagraphUpdate
encodeNodeMappingEdgesAsMegagraphUpdate mappingId nodeMappingEdges =
  [MappingElementOperation mappingId $ InsertNodeMappingEdges nodeMappingEdges]

encodeEdgeMappingEdgesAsMegagraphUpdate :: MappingId -> Array EdgeMappingEdge -> MegagraphUpdate
encodeEdgeMappingEdgesAsMegagraphUpdate mappingId edgeMappingEdges =
  [MappingElementOperation mappingId $ InsertEdgeMappingEdges edgeMappingEdges]

encodeMappingAsMegagraphUpdate :: Mapping -> MegagraphUpdate
encodeMappingAsMegagraphUpdate mapping =
  let
    nodeMappingEdgesOps = encodeNodeMappingEdgesAsMegagraphUpdate mapping.id $ Array.fromFoldable mapping.nodeMappingEdges
    edgeMappingEdgesOps = encodeEdgeMappingEdgesAsMegagraphUpdate mapping.id $ Array.fromFoldable mapping.edgeMappingEdges
  in
    nodeMappingEdgesOps <> edgeMappingEdgesOps
