module Test.Main where

import Prelude

import AppState (emptyAppState)
import Control.Monad.Except (runExcept)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import Foreign (renderForeignError)
import Foreign.Generic (encodeJSON, decodeJSON)
import Gen (TestGraph(..), TestGraphWithOp(..))
import Interpreter (applyMegagraphUpdate, interpretGraphOperation)
import Megagraph (edgeArray)
import MegagraphOperation (GraphOperation(..), createTargetsIfNotExist, encodeGraphAsMegagraphUpdate, invertGraphOperation)
import Test.QuickCheck (Result(..), quickCheck', (===))
import Test.Spec (it, describe)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import WireData (decodeWireData, encodeWireData)

prop_RemoveInsertNode :: TestGraph -> Result
prop_RemoveInsertNode (TestGraph graph) =
  case List.uncons (Map.values graph.nodes) of
    Nothing -> Success
    Just {head, tail} ->
      graph
      ===
      (graph
       # interpretGraphOperation (UpdateNodes [head] [head {deleted = true}])
       # interpretGraphOperation (UpdateNodes [head {deleted = true}] [head]))

prop_GraphOperationsUndoable :: TestGraphWithOp -> Result
prop_GraphOperationsUndoable (TestGraphWithOp graph op) =
  let
    doneUndoneGraph = graph
                      # interpretGraphOperation op
                      # interpretGraphOperation (invertGraphOperation op)
  in
    graph === doneUndoneGraph

prop_GraphOperationsMaintainEdgeValidity :: TestGraphWithOp -> Result
prop_GraphOperationsMaintainEdgeValidity (TestGraphWithOp graph op) =
  let
    oppedGraph = graph # interpretGraphOperation op
  in
    true === Array.all (\edge -> isJust (Map.lookup edge.source oppedGraph.nodes)
                              && isJust (Map.lookup edge.target oppedGraph.nodes))
                       (edgeArray oppedGraph)

prop_EncodeDecode :: TestGraph -> Result
prop_EncodeDecode (TestGraph graph) =
  let
    op = encodeGraphAsMegagraphUpdate graph
    rawWireDatas = unsafePerformEffect $ encodeWireData $ op
    graphJSONString = encodeJSON rawWireDatas
    nullRect = {height: 0.0, width: 0.0, top: 0.0, bottom: 0.0, left: 0.0, right: 0.0}
    state = emptyAppState nullRect
  in
    case
      lmap (show <<< map renderForeignError)
      $ runExcept
      $ (decodeJSON graphJSONString >>= decodeWireData)
    of
      Left errors -> true === false
      Right wireData ->
        let
          reconstructedState =
            applyMegagraphUpdate (createTargetsIfNotExist state.megagraph wireData.op) state
        in
          case Map.lookup graph.id reconstructedState.megagraph.graphs of
            Nothing -> true === false
            Just reconstructedGraph ->
              graph === reconstructedGraph

n :: Int
n = 100

main :: Effect Unit
main =
  launchAff_ do
    runSpec [ consoleReporter ] do
      describe "Graph" do
        it "Invariant to removing then re-adding a node" do
          liftEffect $ quickCheck' n prop_RemoveInsertNode
        it "Graph operations are undoable" do
          liftEffect $ quickCheck' n prop_GraphOperationsUndoable
        it "Graph operations maintain edge validity" do
          liftEffect $ quickCheck' n prop_GraphOperationsMaintainEdgeValidity
        it "Invariant to encoding then decoding graph as MegagraphUpdate JSON" do
          liftEffect $ quickCheck' n prop_EncodeDecode
