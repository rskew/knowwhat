module MegagraphOperation where

import Prelude

import Data.Array as Array
import Data.Foldable (foldr)
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.UUID (UUID)
import Data.UUID as UUID
import Foreign.Class (class Encode, class Decode)
import Foreign.Generic (genericEncode, genericDecode, defaultOptions)
import Megagraph (Megagraph, Edge, EdgeMappingEdge, Graph, GraphId, Mapping, MappingId, Node, NodeMappingEdge, PathEquation, edgeArray)


-- | An operation on a graph is stored with the before/after values of the updated field
-- | to allow it to be inverted.
data GraphOperation
  -- GraphOp name   | pre-op state         | post-op state
  = UpdateNodes       (Array Node)           (Array Node)
  | UpdateEdges       (Array Edge)           (Array Edge)
  | UpdateTitle       String                 String

data EquationOperation
  -- EquationOp name   | target graph | equation
  = InsertPathEquations  (Array PathEquation)
  | DeletePathEquations  (Array PathEquation)

data MappingOperation
  -- MappingOp name        | from                    | to
  = UpdateNodeMappingEdges (Array NodeMappingEdge) (Array NodeMappingEdge)
  | UpdateEdgeMappingEdges (Array EdgeMappingEdge) (Array EdgeMappingEdge)

data CreateOperation
  = CreateGraph GraphId String
  | CreateMapping MappingId GraphId GraphId String

data MegagraphOperation
  = GraphComponentOperation GraphId GraphOperation
  | GraphComponentEquationOperation GraphId EquationOperation
  | MappingComponentOperation MappingId GraphId GraphId MappingOperation
  | CreateComponentOperation CreateOperation
  | MegagraphOperationNoOp

data MegagraphComponent
  = GraphComponent GraphId
  | MappingComponent MappingId GraphId GraphId

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
    MappingComponent mappingId source target ->
      "MappingComponent " <> show mappingId <> " from: " <> show source <> " to: " <> show target

megagraphOperationTargets :: MegagraphOperation -> Array UUID
megagraphOperationTargets = case _ of
  GraphComponentOperation graphId op -> case op of
    UpdateNodes _ nodes -> _.id <$> nodes
    UpdateEdges _ edges -> _.id <$> edges
    UpdateTitle _ _ -> [graphId]
  GraphComponentEquationOperation graphId _ -> [graphId]
  MappingComponentOperation _ _ _ op -> case op of
    UpdateNodeMappingEdges _ nodeMappingEdges -> _.id <$> nodeMappingEdges
    UpdateEdgeMappingEdges _ edgeMappingEdges -> _.id <$> edgeMappingEdges
  CreateComponentOperation op -> case op of
    CreateGraph graphId _ -> [graphId]
    CreateMapping mappingId _ _ _ -> [mappingId]
  MegagraphOperationNoOp -> []

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
  UpdateNodes      from to -> UpdateNodes      to from
  UpdateEdges      from to -> UpdateEdges      to from
  UpdateTitle      from to -> UpdateTitle      to from

invertEquationOperation :: EquationOperation -> EquationOperation
invertEquationOperation = case _ of
  InsertPathEquations pathEquations -> DeletePathEquations pathEquations
  DeletePathEquations pathEquations -> InsertPathEquations pathEquations

invertMappingOperation :: MappingOperation -> MappingOperation
invertMappingOperation = case _ of
  UpdateNodeMappingEdges from to -> UpdateNodeMappingEdges to from
  UpdateEdgeMappingEdges from to -> UpdateEdgeMappingEdges to from

invertMegagraphOperation :: MegagraphOperation -> MegagraphOperation
invertMegagraphOperation = case _ of
  GraphComponentOperation graphId graphOp ->
    GraphComponentOperation graphId $ invertGraphOperation graphOp
  GraphComponentEquationOperation graphId equationOp ->
    GraphComponentEquationOperation graphId $ invertEquationOperation equationOp
  MappingComponentOperation mappingId from to mappingOp ->
    MappingComponentOperation mappingId from to $ invertMappingOperation mappingOp
  CreateComponentOperation createOp -> MegagraphOperationNoOp
  MegagraphOperationNoOp -> MegagraphOperationNoOp

invertMegagraphUpdate :: MegagraphUpdate -> MegagraphUpdate
invertMegagraphUpdate =
  Array.reverse <<< map invertMegagraphOperation

instance showGraphOperation :: Show GraphOperation where
  show = case _ of
    UpdateNodes from to ->
      "UpdateNodes from: " <> show from <> " to: " <> show to
    UpdateEdges from to ->
      "UpdateEdges from: " <> show from <> " to: " <> show to
    UpdateTitle from to ->
      "UpdateTitle from " <> from <> " to: " <> to

instance showEquationOperation :: Show EquationOperation where
  show = case _ of
    InsertPathEquations pathEquations ->
      "InsertPathEquations " <> show pathEquations
    DeletePathEquations pathEquations ->
      "DeletePathEquations " <> show pathEquations

instance showMappingOperation :: Show MappingOperation where
  show = case _ of
    UpdateNodeMappingEdges from to ->
      "UpdateNodeMappingEdges from: " <> show from <> " to: " <> show to
    UpdateEdgeMappingEdges from to ->
      "UpdateEdgeMappingEdges from: " <> show from <> " to: " <> show to

instance showCreateOperation :: Show CreateOperation where
  show = case _ of
    CreateGraph graphId title ->
      "CreateGraph " <> show graphId <> " with title: " <> title
    CreateMapping mappingId from to title ->
      "CreateMapping " <> title <> " " <> show mappingId <> " from: " <> show from <> " to: " <> show to

instance showMegagraphOperation :: Show MegagraphOperation where
  show = case _ of
    GraphComponentOperation graphId graphOp ->
      "GraphComponentOperation on graph " <> show graphId <> " " <> show graphOp
    GraphComponentEquationOperation graphId equationOp ->
      "GraphComponentEquationOperation on graph " <> show graphId <> " " <> show equationOp
    MappingComponentOperation mappingId from to mappingOp ->
      "MappingComponentOperation on mapping " <> show mappingId <> " from: " <> show from <> " to: " <> show to <> " " <> show mappingOp
    CreateComponentOperation createOp ->
      "CreateComponentOperation " <> show createOp
    MegagraphOperationNoOp -> "MegagraphOperationNoOp"

encodeNodesAsMegagraphUpdate :: GraphId -> Array Node -> MegagraphUpdate
encodeNodesAsMegagraphUpdate graphId nodes =
  let
    deletedNodes = _{deleted = true} <$> nodes
  in
    [GraphComponentOperation graphId $ UpdateNodes deletedNodes nodes]

encodeEdgesAsMegagraphUpdate :: GraphId -> Array Edge -> MegagraphUpdate
encodeEdgesAsMegagraphUpdate graphId edges =
  let
    deletedEdges = _{deleted = true} <$> edges
  in
    [GraphComponentOperation graphId $ UpdateEdges deletedEdges edges]

encodePathEquationsAsMegagraphUpdate :: GraphId -> Array PathEquation -> MegagraphUpdate
encodePathEquationsAsMegagraphUpdate graphId pathEquations =
  [ GraphComponentEquationOperation graphId $ InsertPathEquations pathEquations ]

encodeGraphAsMegagraphUpdate :: Graph -> MegagraphUpdate
encodeGraphAsMegagraphUpdate graph =
  let
    nodeOps = encodeNodesAsMegagraphUpdate graph.id (Array.fromFoldable $ Map.values graph.nodes)
    edgeOps = encodeEdgesAsMegagraphUpdate graph.id (edgeArray graph)
    pathEquationOps = encodePathEquationsAsMegagraphUpdate graph.id $ Array.fromFoldable graph.pathEquations
    titleOp = [ GraphComponentOperation graph.id $ UpdateTitle "" graph.title.text ]
  in
    nodeOps <> edgeOps <> pathEquationOps <> titleOp

encodeNodeMappingEdgesAsMegagraphUpdate :: MappingId -> GraphId -> GraphId -> Array NodeMappingEdge -> MegagraphUpdate
encodeNodeMappingEdgesAsMegagraphUpdate mappingId from to nodeMappingEdges =
  let
    deletedNodeMappingEdges = _{deleted = true} <$> nodeMappingEdges
  in
    [MappingComponentOperation mappingId from to $ UpdateNodeMappingEdges deletedNodeMappingEdges nodeMappingEdges]

encodeEdgeMappingEdgesAsMegagraphUpdate :: MappingId -> GraphId -> GraphId -> Array EdgeMappingEdge -> MegagraphUpdate
encodeEdgeMappingEdgesAsMegagraphUpdate mappingId from to edgeMappingEdges =
  let
    deletedEdgeMappingEdges = _{deleted = true} <$> edgeMappingEdges
  in
    [MappingComponentOperation mappingId from to $ UpdateEdgeMappingEdges deletedEdgeMappingEdges edgeMappingEdges]

encodeMappingAsMegagraphUpdate :: Mapping -> MegagraphUpdate
encodeMappingAsMegagraphUpdate mapping =
  let
    nodeMappingEdgesOps = encodeNodeMappingEdgesAsMegagraphUpdate mapping.id mapping.sourceGraph mapping.targetGraph
                          $ Array.fromFoldable mapping.nodeMappingEdges
    edgeMappingEdgesOps = encodeEdgeMappingEdgesAsMegagraphUpdate mapping.id mapping.sourceGraph mapping.targetGraph
                          $ Array.fromFoldable mapping.edgeMappingEdges
  in
    nodeMappingEdgesOps <> edgeMappingEdgesOps

encodeMegagraphAsMegagraphUpdate :: Megagraph -> MegagraphUpdate
encodeMegagraphAsMegagraphUpdate megagraph =
  Array.concat
  $ (megagraph.graphs # Map.values >>> Array.fromFoldable >>> map encodeGraphAsMegagraphUpdate)
    <>
    (megagraph.mappings # Map.values >>> Array.fromFoldable >>> map encodeMappingAsMegagraphUpdate)

createTargetIfNotExists :: Megagraph -> MegagraphComponent -> MegagraphUpdate -> MegagraphUpdate
createTargetIfNotExists megagraph target =
  let
    createGraphIfNotExists graphId = case Map.lookup graphId megagraph.graphs of
      Just _ -> identity
      Nothing -> Array.cons (CreateComponentOperation $ CreateGraph graphId (UUID.toString graphId))
    createMappingIfNotExists mappingId from to = case Map.lookup mappingId megagraph.mappings of
      Just _ -> identity
      Nothing -> Array.cons (CreateComponentOperation $ CreateMapping mappingId from to (UUID.toString mappingId))
  in
    case target of
      GraphComponent graphId -> createGraphIfNotExists graphId
      MappingComponent mappingId from to -> createMappingIfNotExists mappingId from to

createTargetsIfNotExist :: Megagraph -> MegagraphUpdate -> MegagraphUpdate
createTargetsIfNotExist megagraph op =
  let
    targets =
      op
      <#> case _ of
        GraphComponentOperation graphId _ -> Just $ GraphComponent graphId
        GraphComponentEquationOperation graphId _ -> Just $ GraphComponent graphId
        MappingComponentOperation mappingId from to _ -> Just $ MappingComponent mappingId from to
        _ -> Nothing
      # Array.catMaybes
      # Array.nub
  in
    foldr (createTargetIfNotExists megagraph) op targets
