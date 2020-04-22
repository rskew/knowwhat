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
import Gen (TestMegagraph(..), TestMegagraphWithOp(..))
import Interpreter (interpretMegagraphStateUpdate)
import Megagraph (edgeArray, emptyMegagraph, _graph, _node, _edge)
import MegagraphStateUpdate (MegagraphStateUpdate(..), encodeMegagraphAsMegagraphStateUpdates, invertMegagraphStateUpdates)
import Test.QuickCheck (Result(..), quickCheck', (===), (<?>))
import Test.Spec (it, describe)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

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
    megagraph === undoneMegagraph

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
