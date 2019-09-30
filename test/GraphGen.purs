module GraphGen where

import AppOperation.GraphOp (encodeNodeAsGraphOp, interpretGraphOp)
import Core (Edge, GraphData, GraphId, GraphSpacePoint2D(..), Node, batchInsertEdges, emptyGraphData, freshEdge, freshNode)
import Prelude (bind, comparing, pure, ($), (<$>), (<<<), (<>))
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen

import Data.Array as Array
import Data.Foldable (foldr, length)
import Data.List.Lazy (replicateM)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty(..))
import Data.String.CodePoints (fromCodePointArray, codePointFromChar)
import Data.Tuple (Tuple(..), fst)
import Run as Run
import Unsafe.Coerce (unsafeCoerce)

nodeGen :: GraphId -> Gen Node
nodeGen graphId = do
  --idStr <- genAsciiString
  --text  <- genAsciiString
  idChar <- arbitrary :: Gen Char
  let beginIdStr = fromCodePointArray [ codePointFromChar idChar ]
  idStr <- (arbitrary :: Gen String)
  text  <- (arbitrary :: Gen String)
  x     <- (arbitrary :: Gen Number)
  y     <- (arbitrary :: Gen Number)
  let id = unsafeCoerce $ beginIdStr <> idStr
  pure $ (freshNode graphId id) { text = text, position = GraphSpacePoint2D { x : x, y : y } }

-- | Generate a bunch of edges for a particular graph
edgeGen :: GraphId -> GraphData -> Array Node -> Gen (Array Edge)
edgeGen graphId graphData nodes = do
  let nNodes = Map.size graphData.nodes
  nEdges <- chooseInt 0 nNodes
  sourceNodes <- subset nodes
  targetNodes <- subset nodes
  pure do
    Tuple source target <- Array.zip sourceNodes targetNodes
    pure $ freshEdge { source      : source.id
                     , target      : target.id
                     , sourceGraph : graphId
                     , targetGraph : graphId
                     }

-- | A graph with some extra nodes and extra edges between the graph's nodes
data TestGraph = TestGraph GraphId GraphData (Array Node) (Array Edge)

graphGen :: Gen TestGraph
graphGen = do
  let graphId = unsafeCoerce "graphId"

  -- Generate a bunch of nodes, add them to graph
  -- Make sure ids are unique since we're just using random strings
  nodes <- Array.nubBy (comparing _.id) <$> (arrayOf $ nodeGen graphId)
  node <- nodeGen graphId

  let graphData = foldr (fst <<< Run.extract <<< interpretGraphOp <<< encodeNodeAsGraphOp) emptyGraphData nodes

  -- generate a bunch of edges between the nodes
  edges <- edgeGen graphId graphData nodes

  let graphData' = batchInsertEdges graphData edges

  extraNodes <- arrayOf $ nodeGen graphId
  extraEdges <- edgeGen graphId graphData' nodes

  pure $ TestGraph graphId graphData' extraNodes extraEdges

instance testGraphArbitrary :: Arbitrary TestGraph where
  arbitrary = graphGen


------
-- Utils

subset :: forall a. Array a -> Gen (Array a)
subset stuff = do
  case Array.uncons stuff of
    Nothing -> pure []
    Just consStuff -> do
      let nonEmptyStuff = NonEmpty consStuff.head consStuff.tail
      nSubStuffs <- chooseInt 0 $ length stuff
      subStuff <- replicateM nSubStuffs
        $ elements nonEmptyStuff
      pure $ Array.fromFoldable subStuff

