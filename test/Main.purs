module Test.Main where

import Prelude

import AppOperation (AppOperation(..))
import AppOperation.GraphOp (encodeGraphDataAsGraphOp, encodeNodeAsGraphOp, interpretGraphOp, invertGraphOp)
import AppOperation.Interpreter (interpretAppOperation)
import AppState (emptyAppState)
import Control.Monad.Except.Trans (runExceptT)
import Core (emptyGraphData, insertNodeImpl, moveNodeImpl, updateNodeTextImpl)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (fst)
import Effect (Effect)
import Foreign as Foreign
import Foreign.Class (encode, decode)
import GraphGen (TestGraph(..))
import Run as Run
import Test.QuickCheck (Result(..), (===), quickCheck')


------
-- Properties to test

prop_InsertNode :: TestGraph -> Result
prop_InsertNode (TestGraph graphId graphData extraNodes _) =
  case Array.head extraNodes of
    Nothing -> Success
    Just node ->
      let
        interpreted = graphData # (fst $ Run.extract $ interpretGraphOp $ encodeNodeAsGraphOp node)
        manual = graphData
                 # insertNodeImpl graphId node.id
                 # moveNodeImpl node.id node.position
                 # updateNodeTextImpl node.id node.text
      in
        interpreted === manual

prop_InsertRemoveNodeA :: TestGraph -> Result
prop_InsertRemoveNodeA (TestGraph graphId graphData extraNodes _) =
  case Array.head extraNodes of
    Nothing -> Success
    Just node ->
      (graphData # (fst $ Run.extract $ interpretGraphOp
                    $ encodeNodeAsGraphOp node >>= const (invertGraphOp $ encodeNodeAsGraphOp node)))
      ===
      graphData

prop_InsertRemoveNodeB :: TestGraph -> Result
prop_InsertRemoveNodeB (TestGraph graphId graphData extraNodes _) =
  case Array.head extraNodes of
    Nothing -> Success
    Just node ->
      (graphData
       # (fst $ Run.extract $ interpretGraphOp $ encodeNodeAsGraphOp node)
       # (fst $ Run.extract $ interpretGraphOp $ invertGraphOp $ encodeNodeAsGraphOp node))
      ===
      graphData

prop_EncodeDecode :: TestGraph -> Result
prop_EncodeDecode (TestGraph graphId graphData _ _) =
  let
    baseAppOp = map (const unit) $ AppOperation graphId $ encodeGraphDataAsGraphOp graphData
  in
    case runExceptT $ decode $ encode baseAppOp of
      Identity (Left err) -> Failed $ show $ Foreign.renderForeignError <$> err
      Identity (Right (decodedAppOperation :: AppOperation Unit)) ->
        let
          roundtripAppState = interpretAppOperation decodedAppOperation emptyAppState
          baseAppState      = interpretAppOperation baseAppOp emptyAppState
        in
          roundtripAppState === baseAppState

prop_fail :: TestGraph -> Result
prop_fail (TestGraph graphId graphData extraNodes extraEdges) =
  graphData === emptyGraphData

main :: Effect Unit
main = do
  let n = 100
  quickCheck' n prop_InsertNode
  quickCheck' n prop_InsertRemoveNodeA
  quickCheck' n prop_InsertRemoveNodeB

  quickCheck' n prop_EncodeDecode
