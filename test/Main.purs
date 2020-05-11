module Test.Main where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Lens ((^?))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Foreign (renderForeignError)
import Foreign.Generic (encodeJSON, decodeJSON)
import FunctorialDataMigration.Core.Examples as FDM
import Gen (TestMegagraph(..), TestMegagraphWithOp(..))
import Interpreter (interpretMegagraphStateUpdate)
import Megagraph (Graph, Mapping, _edge, _graph, _node, edgeArray, emptyGraph, emptyMapping, emptyMegagraph, freshEdge, freshEdgeMappingEdge, freshNode, freshNodeMappingEdge, freshPathEquation, insertEdge, insertNode, mappingToSignatureMapping, removeDeleted, updateEdgeMappingEdge, updateNodeMappingEdge, updatePathEquation)
import MegagraphStateUpdate (MegagraphStateUpdate(..), encodeMegagraphAsMegagraphStateUpdates, invertMegagraphStateUpdates)
import Test.Assert (assert)
import Test.QuickCheck (Result(..), quickCheck', (===), (<?>))
import Test.Spec (describe, it)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Unsafe.Coerce (unsafeCoerce)

prop_RemoveInsertNode :: TestMegagraph -> Result
prop_RemoveInsertNode (TestMegagraph megagraph) =
  case do
    graph <- List.head $ Map.values megagraph.graphs
    List.head $ Map.values graph.nodes
  of
    Nothing -> Success
    Just node ->
      megagraph
      ===
      (megagraph
       # interpretMegagraphStateUpdate (UpdateNodes [node] [node {deleted = true}])
       # interpretMegagraphStateUpdate (UpdateNodes [node {deleted = true}] [node]))

prop_MegagraphOperationsUndoable :: TestMegagraphWithOp -> Result
prop_MegagraphOperationsUndoable (TestMegagraphWithOp megagraph op) =
  let
    doneMegagraph = foldl (flip interpretMegagraphStateUpdate) megagraph op
    undoneMegagraph = foldl (flip interpretMegagraphStateUpdate) doneMegagraph (invertMegagraphStateUpdates op)
  in
    removeDeleted megagraph === removeDeleted undoneMegagraph

prop_MegagraphOperationsMaintainEdgeValidity :: TestMegagraphWithOp -> Result
prop_MegagraphOperationsMaintainEdgeValidity (TestMegagraphWithOp megagraph op) =
  let
    updatedMegagraph = foldl (flip interpretMegagraphStateUpdate) megagraph op
  in
    prop_MegagraphHasValidEdges (TestMegagraph updatedMegagraph)

prop_MegagraphHasValidEdges :: TestMegagraph -> Result
prop_MegagraphHasValidEdges (TestMegagraph megagraph) =
  (Array.all graphHasValidEdges (Array.fromFoldable $ Map.values megagraph.graphs))
  && (Array.all mappingHasValidEdges (Array.fromFoldable $ Map.values megagraph.mappings))
  <?> "Megagraph has invalid edges:\n" <> show megagraph
  where
    graphHasValidEdges graph =
      Array.all (\edge -> isJust (Map.lookup edge.source graph.nodes)
                          && isJust (Map.lookup edge.target graph.nodes))
                (edgeArray graph)
    mappingHasValidEdges mapping =
      Array.all (\nodeMappingEdge ->
                  isJust (megagraph ^? _graph mapping.sourceGraph <<< _node nodeMappingEdge.sourceNode)
                  && isJust (megagraph ^? _graph mapping.targetGraph <<< _node nodeMappingEdge.targetNode))
                (Array.fromFoldable $ Map.values mapping.nodeMappingEdges)
      &&
      Array.all (\edgeMappingEdge ->
                  isJust (megagraph ^? _graph mapping.sourceGraph <<< _edge edgeMappingEdge.sourceEdge)
                  && isJust (megagraph ^? _graph mapping.targetGraph <<< _edge edgeMappingEdge.targetEdge))
                (Array.fromFoldable $ Map.values mapping.edgeMappingEdges)

prop_EncodeDecode :: TestMegagraph -> Result
prop_EncodeDecode (TestMegagraph megagraph) =
  let
    op :: Array MegagraphStateUpdate
    op = encodeMegagraphAsMegagraphStateUpdates megagraph
    opJSONString = encodeJSON op
  in
    case
      lmap (show <<< map renderForeignError)
      $ runExcept
      $ decodeJSON opJSONString
    of
      Left errors -> false <?> "Failed to decode op:\n" <> errors <> "\n" <> opJSONString
      Right (op' :: Array MegagraphStateUpdate) ->
        let
          reconstructedMegagraph = foldl (flip interpretMegagraphStateUpdate) emptyMegagraph op'
        in
          megagraph == reconstructedMegagraph
          <?> "Encoded/decoded megagraph doesn't match original megagraph.\nEncoded/Decoded megagraphgraph:\n"
              <> show reconstructedMegagraph
              <> "\nOriginal megagraph:\n" <> show megagraph
              <> "\nEncoded megagraph:\n" <> show op'

n :: Int
n = 100

freeSquare :: Graph
freeSquare =
  let
    freeSquareId = unsafeCoerce "freeSquare"
    nodeA = freshNode freeSquareId $ unsafeCoerce "a"
    nodeB = freshNode freeSquareId $ unsafeCoerce "b"
    nodeC = freshNode freeSquareId $ unsafeCoerce "c"
    nodeD = freshNode freeSquareId $ unsafeCoerce "d"
    nodeDeleted = freshNode freeSquareId (unsafeCoerce "deleted")
                  # _{deleted = true}
    edgeAB = freshEdge {id: unsafeCoerce "ab", graphId: freeSquareId, source: unsafeCoerce "a", target: unsafeCoerce "b"}
    edgeAC = freshEdge {id: unsafeCoerce "ac", graphId: freeSquareId, source: unsafeCoerce "a", target: unsafeCoerce "c"}
    edgeBD = freshEdge {id: unsafeCoerce "bd", graphId: freeSquareId, source: unsafeCoerce "b", target: unsafeCoerce "d"}
    edgeCD = freshEdge {id: unsafeCoerce "cd", graphId: freeSquareId, source: unsafeCoerce "c", target: unsafeCoerce "d"}
  in
    emptyGraph freeSquareId
    # insertNode nodeA
    # insertNode nodeB
    # insertNode nodeC
    # insertNode nodeD
    # insertNode nodeDeleted
    # insertEdge edgeAB
    # insertEdge edgeAC
    # insertEdge edgeBD
    # insertEdge edgeCD

commutativeSquare :: Graph
commutativeSquare =
  let
    commSquareId = unsafeCoerce "commSquare"
    nodeA' = freshNode commSquareId $ unsafeCoerce "a'"
    nodeB' = freshNode commSquareId $ unsafeCoerce "b'"
    nodeC' = freshNode commSquareId $ unsafeCoerce "c'"
    nodeD' = freshNode commSquareId $ unsafeCoerce "d'"
    nodeDeleted' = freshNode commSquareId (unsafeCoerce "deleted'")
                   # _{deleted = true}
    edgeA'B' = freshEdge {id: unsafeCoerce "a'b'", graphId: commSquareId, source: unsafeCoerce "a'", target: unsafeCoerce "b'"}
    edgeA'C' = freshEdge {id: unsafeCoerce "a'c'", graphId: commSquareId, source: unsafeCoerce "a'", target: unsafeCoerce "c'"}
    edgeB'D' = freshEdge {id: unsafeCoerce "b'd'", graphId: commSquareId, source: unsafeCoerce "b'", target: unsafeCoerce "d'"}
    edgeC'D' = freshEdge {id: unsafeCoerce "c'd'", graphId: commSquareId, source: unsafeCoerce "c'", target: unsafeCoerce "d'"}
    equation = freshPathEquation (unsafeCoerce "it commutes!") commSquareId
               # _{ pathA = [unsafeCoerce "a'b'", unsafeCoerce "b'd'"]
                  , pathB = [unsafeCoerce "a'c'", unsafeCoerce "c'd'"]
                  }
  in
    emptyGraph commSquareId
    # insertNode nodeA'
    # insertNode nodeB'
    # insertNode nodeC'
    # insertNode nodeD'
    # insertNode nodeDeleted'
    # insertEdge edgeA'B'
    # insertEdge edgeA'C'
    # insertEdge edgeB'D'
    # insertEdge edgeC'D'
    # updatePathEquation equation

commToFreeSquareMapping :: Mapping
commToFreeSquareMapping =
  let
    mappingId = unsafeCoerce "mapping"
    mappingEdgeA'A = freshNodeMappingEdge (unsafeCoerce "a'a") mappingId (unsafeCoerce "a'") (unsafeCoerce "a")
    mappingEdgeB'B = freshNodeMappingEdge (unsafeCoerce "b'b") mappingId (unsafeCoerce "b'") (unsafeCoerce "b")
    mappingEdgeC'C = freshNodeMappingEdge (unsafeCoerce "c'c") mappingId (unsafeCoerce "c'") (unsafeCoerce "c")
    mappingEdgeD'D = freshNodeMappingEdge (unsafeCoerce "d'd") mappingId (unsafeCoerce "d'") (unsafeCoerce "d")
    mappingEdgeA'B'AB = freshEdgeMappingEdge (unsafeCoerce "a'b'ab") mappingId (unsafeCoerce "a'b'") (unsafeCoerce "ab")
    mappingEdgeA'C'AC = freshEdgeMappingEdge (unsafeCoerce "a'c'ac") mappingId (unsafeCoerce "a'c'") (unsafeCoerce "ac")
    mappingEdgeB'D'BD = freshEdgeMappingEdge (unsafeCoerce "b'd'bd") mappingId (unsafeCoerce "b'd'") (unsafeCoerce "bd")
    mappingEdgeC'D'CD = freshEdgeMappingEdge (unsafeCoerce "c'd'cd") mappingId (unsafeCoerce "c'd'") (unsafeCoerce "cd")
  in
    emptyMapping mappingId (unsafeCoerce "commSquare") (unsafeCoerce "freeSquare")
    # updateNodeMappingEdge mappingEdgeA'A
    # updateNodeMappingEdge mappingEdgeB'B
    # updateNodeMappingEdge mappingEdgeC'C
    # updateNodeMappingEdge mappingEdgeD'D
    # updateEdgeMappingEdge mappingEdgeA'B'AB
    # updateEdgeMappingEdge mappingEdgeA'C'AC
    # updateEdgeMappingEdge mappingEdgeB'D'BD
    # updateEdgeMappingEdge mappingEdgeC'D'CD

main :: Effect Unit
main =
  launchAff_ do
    runSpec [ consoleReporter ] do
      describe "Graph" do
        it "Invariant to removing then re-adding a node" do
          liftEffect $ quickCheck' n prop_RemoveInsertNode
        it "Megagraph operations are undoable" do
          liftEffect $ quickCheck' n prop_MegagraphOperationsUndoable
        it "Generated Megagraphs have valid edges" do
          liftEffect $ quickCheck' n prop_MegagraphHasValidEdges
        it "Megagraph operations maintain edge validity" do
          liftEffect $ quickCheck' n prop_MegagraphOperationsMaintainEdgeValidity
        it "Invariant to encoding then decoding megagraph as MegagraphUpdate JSON" do
          liftEffect $ quickCheck' n prop_EncodeDecode
      describe "Mapping validation" do
        it "Commutative Square to FreeSquare Mapping is correctly converted into a SignatureMapping" do
          liftEffect $ assert $ mappingToSignatureMapping commToFreeSquareMapping commutativeSquare freeSquare == FDM.commToFreeMapping
