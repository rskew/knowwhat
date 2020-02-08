module Test.Main where

import MegagraphOperation (MegagraphOperation(..), encodeNodeAsGraphOperations, invertMegagraphUpdate)
import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Gen (TestGraph(..))
import Interpreter (interpretGraphOperation)
import Test.QuickCheck (Result(..), quickCheck', (===))
import Test.Spec (it, describe)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

------
-- Properties to test


prop_RemoveInsertNode :: TestGraph -> Result
prop_RemoveInsertNode (TestGraph graph) =
  case List.uncons (Map.values graph.nodes) of
    Nothing -> Success
    Just {head, tail} ->
      let
        insertNodeOps = encodeNodeAsGraphOperations head
        -- Some fudgery as the mechanism to invert an operation is
        -- defined on MegagraphUpdates which operate on the MegagraphState,
        -- but we want to invert our operations which just operate on a Graph.
        removeNodeOps = insertNodeOps
                        <#> GraphElementOperation head.graphId
                        # invertMegagraphUpdate
                        <#> case _ of
                              GraphElementOperation _ op -> Just op
                              _ -> Nothing
                        # Array.catMaybes
      in
        graph
        ===
        (foldl (flip interpretGraphOperation) graph removeNodeOps
          # \graph' -> foldl (flip interpretGraphOperation) graph' insertNodeOps)

--prop_EncodeDecode :: TestGraph -> Result
--prop_EncodeDecode (TestGraph graphId graphData _ _) =
--  let
--    baseAppOp = map (const unit) $ AppOperation graphId $ encodeGraphDataAsGraphOp graphData
--  in
--    case runExceptT $ decode $ encode baseAppOp of
--      Identity (Left err) -> Failed $ show $ Foreign.renderForeignError <$> err
--      Identity (Right (decodedAppOperation :: AppOperation Unit)) ->
--        let
--          roundtripAppState = interpretAppOperation decodedAppOperation emptyAppState
--          baseAppState      = interpretAppOperation baseAppOp emptyAppState
--        in
--          roundtripAppState === baseAppState
--
--prop_fail :: TestGraph -> Result
--prop_fail (TestGraph graphId graphData extraNodes extraEdges) =
--  graphData === emptyGraphData

n :: Int
n = 100

main :: Effect Unit
main =
  launchAff_ do
    runSpec [ consoleReporter ] do
      describe "Graph" do
        it "Invariant to removing then re-adding a node" do
          liftEffect $ quickCheck' n prop_RemoveInsertNode
          -- quickCheck' n prop_EncodeDecode
