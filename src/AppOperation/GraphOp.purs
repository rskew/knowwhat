module AppOperation.GraphOp where

import Prelude

import Core (Edge, EdgeId, GraphData, GraphId, GraphSpacePoint2D, Node, NodeId, allEdgesTouchingNode, deleteEdgeImpl, deleteNodeImpl, insertEdgeImpl, insertNodeImpl, moveNodeImpl, updateEdgeTextImpl, updateNodeTextImpl, edgeArray)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Map as Map
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.UUID as UUID
import Foreign (Foreign)
import Foreign as Foreign
import Foreign.Class (class Encode, class Decode, decode)
import Foreign.Generic (genericEncode, genericDecode, defaultOptions)
import Foreign.Utils (edgeIdToString, parseUUIDEither, parseEdgeIdEither, toExceptT)
import Prim.Row (class Union)
import Run (Run, FProxy, Step(..))
import Run as Run


------
-- GraphOp DSL for making UI interactions undoable and streamable

data GraphOpF next
--  Op name     |  target graph | target node/edge | pre-op state       | post-op state      | next
  = InsertNode     GraphId        NodeId                                                       next
  | DeleteNode     GraphId        NodeId                                                       next
  | InsertEdge                    EdgeId                                                       next
  | DeleteEdge                    EdgeId                                                       next
  | MoveNode                      NodeId             GraphSpacePoint2D   GraphSpacePoint2D   next
  | UpdateNodeText                NodeId             String               String               next
  | UpdateEdgeText                EdgeId             String               String               next

derive instance functorGraphOpF :: Functor GraphOpF

type GRAPHOP = FProxy GraphOpF

_graphOp :: SProxy "graphOp"
_graphOp = SProxy


invertGraphOpF :: forall a. GraphOpF a -> Tuple (GraphOpF Unit) a
invertGraphOpF = case _ of
  InsertNode     graphId nodeId         next -> Tuple (DeleteNode     graphId nodeId         unit) next
  DeleteNode     graphId nodeId         next -> Tuple (InsertNode     graphId nodeId         unit) next
  InsertEdge             edgeId         next -> Tuple (DeleteEdge             edgeId         unit) next
  DeleteEdge             edgeId         next -> Tuple (InsertEdge             edgeId         unit) next
  MoveNode               nodeId from to next -> Tuple (MoveNode               nodeId to from unit) next
  UpdateNodeText         nodeId from to next -> Tuple (UpdateNodeText         nodeId to from unit) next
  UpdateEdgeText         ndgeId from to next -> Tuple (UpdateEdgeText         ndgeId to from unit) next

runInvert :: forall r a.
             Run (graphOp :: GRAPHOP | r) a
             -> Run r (Run (graphOp :: GRAPHOP) a)
runInvert =
  Run.runAccumPure
  (\accumulator ->
    Run.on
    _graphOp
    (invertGraphOpF
     >>> lmap (\reversedOp -> Run.lift _graphOp reversedOp >>= const accumulator)
     >>> Loop)
    Done)
  (\accumulator a -> accumulator >>= (a # pure >>> const))
  (pure unit)

invertGraphOp :: forall r a.
                 Union r (graphOp :: GRAPHOP) (graphOp :: GRAPHOP | r)
                 => Run (graphOp :: GRAPHOP | r) a
                 -> Run (graphOp :: GRAPHOP | r) a
invertGraphOp =
  -- type hackery to join the VariantF after splitting the row via interpreting
  runInvert >>> Run.expand >>> map Run.expand >>> join

-- | If the first action in both ops can be collapsed together, then collapse the
-- | two tops together , returning the rest of the each operation separately.
-- | This should be called recursively on the rest of each operation to decide if the
-- | whole of each operation can be collapsed together.
collapseGraphOpF :: forall a. GraphOpF a -> GraphOpF a -> Maybe { collapsedOp :: (GraphOpF Unit)
                                                                , nextNext :: a
                                                                , prevNext :: a
                                                                }
collapseGraphOpF nextOp prevOp =
  case Tuple nextOp prevOp of
    Tuple (MoveNode nextNodeId  middlePos lastPos    nextNext)
          (MoveNode firstNodeId firstPos  middlePos' prevNext) ->
      if nextNodeId   == firstNodeId
         && middlePos == middlePos'
      then Just $ { collapsedOp : MoveNode firstNodeId firstPos lastPos unit
                  , nextNext    : nextNext
                  , prevNext    : prevNext
                  }
      else Nothing
    Tuple (UpdateNodeText nextNodeId  middleText lastText    nextNext)
          (UpdateNodeText firstNodeId firstText  middleText' prevNext) ->
      if nextNodeId    == firstNodeId
         && middleText == middleText'
      then Just $ { collapsedOp : UpdateNodeText firstNodeId firstText lastText unit
                  , nextNext    : nextNext
                  , prevNext    : prevNext
                  }
      else Nothing
    Tuple (UpdateEdgeText nextEdgeId  middleText lastText    nextNext)
          (UpdateEdgeText firstEdgeId firstText  middleText' prevNext) ->
      if nextEdgeId    == firstEdgeId
         && middleText == middleText'
      then Just $ { collapsedOp : UpdateEdgeText firstEdgeId firstText lastText unit
                  , nextNext    : nextNext
                  , prevNext    : prevNext
                  }
      else Nothing
    _ -> Nothing

handleGraphOp :: forall a. GraphOpF a -> Tuple (GraphData -> GraphData) a
handleGraphOp = case _ of
  InsertNode graphId nodeId next     -> Tuple (insertNodeImpl graphId nodeId) next
  DeleteNode graphId nodeId next     -> Tuple (deleteNodeImpl nodeId)         next
  InsertEdge edgeId next             -> Tuple (insertEdgeImpl edgeId)         next
  DeleteEdge edgeId next             -> Tuple (deleteEdgeImpl edgeId)         next
  MoveNode nodeId from to next       -> Tuple (moveNodeImpl nodeId to)        next
  UpdateNodeText nodeId from to next -> Tuple (updateNodeTextImpl nodeId to)  next
  UpdateEdgeText edgeId from to next -> Tuple (updateEdgeTextImpl edgeId to)  next

interpretGraphOp :: forall r a.
                    Run (graphOp :: GRAPHOP | r) a -> Run r (Tuple (GraphData -> GraphData) a)
interpretGraphOp =
  Run.runAccumPure
  (\accumulator ->
    Run.on _graphOp (Loop <<< lmap ((>>>) accumulator) <<< handleGraphOp) Done)
  Tuple
  identity

showGraphOp :: forall a. GraphOpF a -> Tuple String a
showGraphOp = case _ of
  InsertNode graphId nodeId next ->
    Tuple ("InsertNode graph: " <> show graphId <> " node: " <> show nodeId) next
  DeleteNode graphId nodeId next ->
    Tuple ("DeleteNode graph: " <> show graphId <> " node: " <> show nodeId) next
  InsertEdge edge next ->
    Tuple ("InsertEdge edge: " <> show edge) next
  DeleteEdge edge next ->
    Tuple ("DeleteEdge edge: " <> show edge) next
  MoveNode nodeId from to next ->
    Tuple ("MoveNode node: " <> show nodeId <> " from: " <> show from <> " to: " <> show to) next
  UpdateNodeText nodeId from to next ->
    Tuple ("UpdateNodeText node: " <> show nodeId <> " from: " <> from <> " to: " <> to) next
  UpdateEdgeText edgeId from to next ->
    Tuple ("UpdateEdgeText node: " <> show edgeId <> " from: " <> from <> " to: " <> to) next

interpretShowGraphOp :: forall r a. Run (graphOp :: GRAPHOP | r) a -> Run r String
interpretShowGraphOp op =
  (op # Run.runAccumPure
   (\accumulator -> Run.on _graphOp (Loop <<< lmap (append accumulator) <<< showGraphOp) Done)
   (\accumulator a -> accumulator)
   "")

------
-- Interface

insertNode :: forall r. GraphId -> NodeId -> Run (graphOp :: GRAPHOP | r) Unit
insertNode graphId nodeId = Run.lift _graphOp $ InsertNode graphId nodeId unit

-- | For the delete operation to be undoable, we need to 'delete' the nodes
-- | data through GraphOps.
-- | First delete text, then position, then edges, then the node, so that
-- | the undo operation first creates the node, then adds the edges,
-- | then move the node into its pre-delete position and gives it its text back.
deleteNode :: forall r.
              Union r (graphOp :: GRAPHOP) (graphOp :: GRAPHOP | r)
              => GraphData -> Node -> Run (graphOp :: GRAPHOP | r) Unit
deleteNode graphData node =
  let
    deleteNodeOp = invertGraphOp $ encodeNodeAsGraphOp node
    allEdges = allEdgesTouchingNode node.id graphData
    deleteEdgeOps = deleteEdge <$> (allEdges.outgoing <> allEdges.incoming)
  in
    foldl bind (pure unit) $ const <$> (deleteEdgeOps <> [deleteNodeOp])

insertEdge :: forall r. EdgeId -> Run (graphOp :: GRAPHOP | r) Unit
insertEdge edgeId = Run.lift _graphOp $ InsertEdge edgeId unit

deleteEdge :: forall r.
              Union r (graphOp :: GRAPHOP) (graphOp :: GRAPHOP | r)
              => Edge -> Run (graphOp :: GRAPHOP | r) Unit
deleteEdge = encodeEdgeAsGraphOp >>> invertGraphOp

moveNode :: forall r. Node -> GraphSpacePoint2D -> Run (graphOp :: GRAPHOP | r) Unit
moveNode node newPos = Run.lift _graphOp $ MoveNode node.id node.position newPos unit

updateNodeText :: forall r. Node -> String -> Run (graphOp :: GRAPHOP | r) Unit
updateNodeText node newText = Run.lift _graphOp $ UpdateNodeText node.id node.text newText unit

updateEdgeText :: forall r. Edge -> String -> Run (graphOp :: GRAPHOP | r) Unit
updateEdgeText edge newText = Run.lift _graphOp $ UpdateEdgeText edge.id edge.text newText unit

encodeNodeAsGraphOp :: forall r. Node -> Run (graphOp :: GRAPHOP | r) Unit
encodeNodeAsGraphOp node = do
  insertNode node.graphId node.id
  moveNode node node.position
  updateNodeText node node.text

encodeEdgeAsGraphOp :: forall r. Edge -> Run (graphOp :: GRAPHOP | r) Unit
encodeEdgeAsGraphOp edge = do
  insertEdge edge.id
  updateEdgeText edge edge.text

encodeGraphDataAsGraphOp :: forall r. GraphData -> Run (graphOp :: GRAPHOP | r) Unit
encodeGraphDataAsGraphOp graphData =
  let
    nodeOps = (const <<< encodeNodeAsGraphOp) <$> (Array.fromFoldable $ Map.values graphData.nodes)
    edgeOps = (const <<< encodeEdgeAsGraphOp) <$> edgeArray graphData
  in
    foldl bind (pure unit) $ nodeOps <> edgeOps


--------
---- Serialisation/deserialisation

instance decodeGraphOpF :: Decode (GraphOpF Unit) where
  decode x = x # genericDecode defaultOptions >>= toExceptT <<< fromForeignGraphOpF

data ForeignGraphOpF
  = ForeignInsertNode      String String
  | ForeignDeleteNode      String String
  | ForeignInsertEdge             String
  | ForeignDeleteEdge             String
  | ForeignMoveNode               String GraphSpacePoint2D GraphSpacePoint2D
  | ForeignUpdateNodeText         String String            String
  | ForeignUpdateEdgeText         String String            String

derive instance genericForeignGraphOpF :: Generic ForeignGraphOpF _

instance encodeForeignGraphOpF :: Encode ForeignGraphOpF where
  encode x = x # genericEncode defaultOptions

instance decodeForeignGraphOpF :: Decode ForeignGraphOpF where
  decode x = x # genericDecode defaultOptions

toForeignGraphOpF :: forall a. GraphOpF a -> Tuple Foreign a
toForeignGraphOpF = lmap (genericEncode defaultOptions) <<< case _ of
  InsertNode graphId nodeId next ->
    Tuple (ForeignInsertNode (UUID.toString graphId) (UUID.toString  nodeId)) next
  DeleteNode graphId nodeId next ->
    Tuple (ForeignDeleteNode (UUID.toString graphId) (UUID.toString  nodeId)) next
  InsertEdge edgeId next ->
    Tuple (ForeignInsertEdge                         (edgeIdToString edgeId)) next
  DeleteEdge edgeId next ->
    Tuple (ForeignDeleteEdge                         (edgeIdToString edgeId)) next
  MoveNode nodeId from to next ->
    Tuple (ForeignMoveNode                           (UUID.toString  nodeId) from to) next
  UpdateNodeText nodeId from to next ->
    Tuple (ForeignUpdateNodeText                     (UUID.toString  nodeId) from to) next
  UpdateEdgeText edgeId from to next ->
    Tuple (ForeignUpdateEdgeText                     (edgeIdToString edgeId) from to) next

fromForeignGraphOpF :: ForeignGraphOpF -> Either String (GraphOpF Unit)
fromForeignGraphOpF = case _ of
  ForeignInsertNode graphIdStr nodeIdStr -> do
    graphId <- parseUUIDEither graphIdStr
    nodeId  <- parseUUIDEither nodeIdStr
    pure $ InsertNode graphId nodeId unit
  ForeignDeleteNode graphIdStr nodeIdStr -> do
    graphId <- parseUUIDEither graphIdStr
    nodeId  <- parseUUIDEither nodeIdStr
    pure $ DeleteNode graphId nodeId unit
  ForeignInsertEdge edgeIdStr -> do
    edgeId <- parseEdgeIdEither edgeIdStr
    pure $ InsertEdge edgeId unit
  ForeignDeleteEdge edgeIdStr -> do
    edgeId <- parseEdgeIdEither edgeIdStr
    pure $ DeleteEdge edgeId unit
  ForeignMoveNode nodeIdStr from to -> do
    nodeId <- parseUUIDEither nodeIdStr
    pure $ MoveNode nodeId from to unit
  ForeignUpdateNodeText nodeIdStr from to -> do
    nodeId <- parseUUIDEither nodeIdStr
    pure $ UpdateNodeText nodeId from to unit
  ForeignUpdateEdgeText edgeIdStr from to -> do
    edgeId <- parseEdgeIdEither edgeIdStr
    pure $ UpdateEdgeText edgeId from to unit

newtype ForeignGraphOp = ForeignGraphOp (Run (graphOp :: GRAPHOP) Unit)

instance encodeForeignGraphOp' :: Encode ForeignGraphOp where
  encode = Foreign.unsafeToForeign <<< encodeForeignGraphOp

instance decodeForeignGraphOp' :: Decode ForeignGraphOp where
  decode = decodeForeignGraphOp

encodeForeignGraphOp :: ForeignGraphOp -> Array Foreign
encodeForeignGraphOp (ForeignGraphOp op) =
  Run.extract $
  (op # Run.runAccumPure
    (\accumulator -> Run.on
      _graphOp (Loop <<< lmap (\encodedOp -> accumulator <> [encodedOp]) <<< toForeignGraphOpF) Done)
    (\accumulator a -> accumulator)
    [])

decodeForeignGraphOp :: Foreign -> Foreign.F ForeignGraphOp
decodeForeignGraphOp foreignOpArray =
  let
    decodeGraphOp :: Foreign -> Foreign.F (Run (graphOp :: GRAPHOP) Unit)
    decodeGraphOp = map (Run.lift _graphOp) <<< (decode :: Foreign -> Foreign.F (GraphOpF Unit))
  in do
    arrayForeign <- Foreign.readArray foreignOpArray
    decodedOperations <- traverse decodeGraphOp arrayForeign
    pure $ ForeignGraphOp $ foldl bind (pure unit) $ map const decodedOperations
