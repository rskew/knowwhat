module MegagraphStateUpdate where

import Prelude

import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Data.UUID (UUID)
import Foreign.Class (class Encode, class Decode)
import Foreign.Generic (genericEncode, genericDecode, defaultOptions)
import Megagraph (Megagraph, Edge, EdgeMappingEdge, Graph, GraphId, Mapping, MappingId, Node, NodeMappingEdge, PathEquation, edgeArray)

data MegagraphStateUpdate
  = UpdateNodes            (Array Node)            (Array Node)
  | UpdateEdges            (Array Edge)            (Array Edge)
  | UpdateTitle GraphId    String                  String
  | UpdateNodeMappingEdges (Array NodeMappingEdge) (Array NodeMappingEdge)
  | UpdateEdgeMappingEdges (Array EdgeMappingEdge) (Array EdgeMappingEdge)
  | CreateGraph GraphId String
  | CreateMapping MappingId GraphId GraphId String
  | InsertPathEquations GraphId (Array PathEquation)
  | DeletePathEquations GraphId (Array PathEquation)
  | MegagraphStateUpdateNoOp

data MegagraphComponent
  = GraphComponent GraphId
  | MappingComponent MappingId

derive instance eqMegagraphComponent :: Eq MegagraphComponent

derive instance ordMegagraphComponent :: Ord MegagraphComponent

derive instance genericMegagraphComponent :: Generic MegagraphComponent _
instance decodeMegagraphComponent :: Decode MegagraphComponent where
  decode = genericDecode defaultOptions
instance encodeMegagraphComponent :: Encode MegagraphComponent where
  encode = genericEncode defaultOptions

instance showMegagraphComponent :: Show MegagraphComponent where
  show = case _ of
    GraphComponent graphId -> "GraphComponent " <> show graphId
    MappingComponent mappingId ->
      "MappingComponent " <> show mappingId

megagraphStateUpdateElementIds :: MegagraphStateUpdate -> Array UUID
megagraphStateUpdateElementIds = case _ of
  UpdateNodes _ nodes -> _.id <$> nodes
  UpdateEdges _ edges -> _.id <$> edges
  UpdateTitle graphId _ _ -> [graphId]
  UpdateNodeMappingEdges _ nodeMappingEdges -> _.id <$> nodeMappingEdges
  UpdateEdgeMappingEdges _ edgeMappingEdges -> _.id <$> edgeMappingEdges
  CreateGraph graphId _ -> [graphId]
  CreateMapping mappingId _ _ _ -> [mappingId]
  InsertPathEquations graphId _ -> [graphId]
  DeletePathEquations graphId _ -> [graphId]
  MegagraphStateUpdateNoOp -> []

derive instance genericMegagraphStateUpdate :: Generic MegagraphStateUpdate _
instance decodeMegagraphStateUpdate :: Decode MegagraphStateUpdate where
  decode = genericDecode defaultOptions
instance encodeMegagraphStateUpdate :: Encode MegagraphStateUpdate where
  encode = genericEncode defaultOptions

invertMegagraphStateUpdate :: MegagraphStateUpdate -> MegagraphStateUpdate
invertMegagraphStateUpdate = case _ of
  UpdateNodes         from to -> UpdateNodes         to from
  UpdateEdges         from to -> UpdateEdges         to from
  UpdateTitle graphId from to -> UpdateTitle graphId to from
  UpdateNodeMappingEdges from to -> UpdateNodeMappingEdges to from
  UpdateEdgeMappingEdges from to -> UpdateEdgeMappingEdges to from
  InsertPathEquations graphId pathEquations -> DeletePathEquations graphId pathEquations
  DeletePathEquations graphId pathEquations -> InsertPathEquations graphId pathEquations
  CreateGraph _ _ -> MegagraphStateUpdateNoOp
  CreateMapping _ _ _ _ -> MegagraphStateUpdateNoOp
  MegagraphStateUpdateNoOp -> MegagraphStateUpdateNoOp

invertMegagraphStateUpdates :: Array MegagraphStateUpdate -> Array MegagraphStateUpdate
invertMegagraphStateUpdates = Array.reverse <<< map invertMegagraphStateUpdate

instance showMegagraphStateUpdate :: Show MegagraphStateUpdate where
  show = case _ of
    UpdateNodes from to ->
      "UpdateNodes from: " <> show from <> " to: " <> show to
    UpdateEdges from to ->
      "UpdateEdges from: " <> show from <> " to: " <> show to
    UpdateTitle graphId from to ->
      "UpdateTitle for graph " <> show graphId <> " from " <> from <> " to: " <> to
    InsertPathEquations graphId pathEquations ->
      "InsertPathEquations " <> show graphId <> show pathEquations
    DeletePathEquations graphId pathEquations ->
      "DeletePathEquations " <> show graphId <> show pathEquations
    UpdateNodeMappingEdges from to ->
      "UpdateNodeMappingEdges from: " <> show from <> " to: " <> show to
    UpdateEdgeMappingEdges from to ->
      "UpdateEdgeMappingEdges from: " <> show from <> " to: " <> show to
    CreateGraph graphId title ->
      "CreateGraph " <> show graphId <> " with title: " <> title
    CreateMapping mappingId from to title ->
      "CreateMapping " <> title <> " " <> show mappingId <> " from: " <> show from <> " to: " <> show to
    MegagraphStateUpdateNoOp -> "MegagraphStateUpdateNoOp"

encodeNodesAsMegagraphStateUpdates :: GraphId -> Array Node -> Array MegagraphStateUpdate
encodeNodesAsMegagraphStateUpdates graphId nodes =
  let
    deletedNodes = _{deleted = true} <$> nodes
  in
    [UpdateNodes deletedNodes nodes]

encodeEdgesAsMegagraphStateUpdates :: GraphId -> Array Edge -> Array MegagraphStateUpdate
encodeEdgesAsMegagraphStateUpdates graphId edges =
  let
    deletedEdges = _{deleted = true} <$> edges
  in
    [UpdateEdges deletedEdges edges]

encodePathEquationsAsMegagraphStateUpdates :: GraphId -> Array PathEquation -> Array MegagraphStateUpdate
encodePathEquationsAsMegagraphStateUpdates graphId pathEquations =
  [InsertPathEquations graphId pathEquations]

encodeGraphAsMegagraphStateUpdates :: Graph -> Array MegagraphStateUpdate
encodeGraphAsMegagraphStateUpdates graph =
  let
    createOp = [CreateGraph graph.id graph.title.text]
    nodeOps = encodeNodesAsMegagraphStateUpdates graph.id (Array.fromFoldable $ Map.values graph.nodes)
    edgeOps = encodeEdgesAsMegagraphStateUpdates graph.id (edgeArray graph)
    pathEquationOps = encodePathEquationsAsMegagraphStateUpdates graph.id $ Array.fromFoldable graph.pathEquations
  in
    createOp <> nodeOps <> edgeOps <> pathEquationOps

encodeNodeMappingEdgesAsMegagraphStateUpdates :: Mapping -> Array NodeMappingEdge -> Array MegagraphStateUpdate
encodeNodeMappingEdgesAsMegagraphStateUpdates mapping nodeMappingEdges =
  let
    deletedNodeMappingEdges = _{deleted = true} <$> nodeMappingEdges
  in
    [ CreateMapping mapping.id mapping.sourceGraph mapping.targetGraph mapping.title
    , UpdateNodeMappingEdges deletedNodeMappingEdges nodeMappingEdges
    ]

encodeEdgeMappingEdgesAsMegagraphStateUpdates :: Mapping -> Array EdgeMappingEdge -> Array MegagraphStateUpdate
encodeEdgeMappingEdgesAsMegagraphStateUpdates mapping edgeMappingEdges =
  let
    deletedEdgeMappingEdges = _{deleted = true} <$> edgeMappingEdges
  in
    [ CreateMapping mapping.id mapping.sourceGraph mapping.targetGraph mapping.title
    , UpdateEdgeMappingEdges deletedEdgeMappingEdges edgeMappingEdges
    ]

encodeMappingAsMegagraphStateUpdates :: Mapping -> Array MegagraphStateUpdate
encodeMappingAsMegagraphStateUpdates mapping =
  let
    nodeMappingEdgesOps = encodeNodeMappingEdgesAsMegagraphStateUpdates mapping
                          $ Array.fromFoldable mapping.nodeMappingEdges
    edgeMappingEdgesOps = encodeEdgeMappingEdgesAsMegagraphStateUpdates mapping
                          $ Array.fromFoldable mapping.edgeMappingEdges
  in
    nodeMappingEdgesOps <> edgeMappingEdgesOps

encodeMegagraphAsMegagraphStateUpdates :: Megagraph -> Array MegagraphStateUpdate
encodeMegagraphAsMegagraphStateUpdates megagraph =
  Array.concat
  $ (megagraph.graphs # Map.values >>> Array.fromFoldable >>> map encodeGraphAsMegagraphStateUpdates)
    <>
    (megagraph.mappings # Map.values >>> Array.fromFoldable >>> map encodeMappingAsMegagraphStateUpdates)
