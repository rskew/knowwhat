module GraphComponent.Utils where

import Prelude

import AppOperation (AppOperation(..))
import AppState (AppState, DrawingEdge, edgeIdStr)
import Data.Array as Array
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Math as Math
import Megagraph (Edge, Focus(..), GraphId, GraphSpacePoint2D(..), GraphView, Node, NodeId, PageSpacePoint2D(..), Point2D, graphSpaceToPageSpace, pageSpaceToGraphSpace)
import MegagraphOperation (GraphOperation(..), MegagraphOperation(..))
import UI.Constants (haloRadius)
import Web.UIEvent.MouseEvent as ME


mouseEventPosition :: ME.MouseEvent -> PageSpacePoint2D
mouseEventPosition e =
  PageSpacePoint2D { x : toNumber $ ME.pageX e
                   , y : toNumber $ ME.pageY e
                   }

euclideanDistance :: GraphSpacePoint2D -> GraphSpacePoint2D -> Number
euclideanDistance (GraphSpacePoint2D pos1) (GraphSpacePoint2D pos2) =
  Math.sqrt
  $ (Math.pow (pos1.x - pos2.x) 2.0)
    + (Math.pow (pos1.y - pos2.y) 2.0)

drawingEdgeWithinNodeHalo :: DrawingEdge -> GraphView -> Node -> Boolean
drawingEdgeWithinNodeHalo drawingEdgeState pane node =
  let
    pointPositionGraphSpace = drawingEdgeState.pointPosition # pageSpaceToGraphSpace pane
  in
    haloRadius > euclideanDistance node.position pointPositionGraphSpace

edgeTextFieldIdStr :: Edge -> String
edgeTextFieldIdStr edge = edgeIdStr edge <> "textField"

edgeSourcePosition :: AppState -> GraphId -> NodeId -> GraphView -> Maybe GraphSpacePoint2D
edgeSourcePosition state graphId sourceId renderPane = do
  sourceGraphState <- Map.lookup graphId state.megagraph.graphs
  sourceNode <- Map.lookup sourceId sourceGraphState.graph.nodes
  pure $ sourceNode.position # graphSpaceToPageSpace sourceGraphState.view # pageSpaceToGraphSpace renderPane

edgeTargetPosition :: AppState -> GraphId -> NodeId -> GraphView -> Maybe GraphSpacePoint2D
edgeTargetPosition state graphId targetId renderPane = do
  targetGraphState <- Map.lookup graphId state.megagraph.graphs
  targetNode <- Map.lookup targetId targetGraphState.graph.nodes
  pure $ targetNode.position # graphSpaceToPageSpace targetGraphState.view # pageSpaceToGraphSpace renderPane

edgeMidPosition :: AppState -> GraphId -> NodeId -> NodeId -> GraphView -> Maybe GraphSpacePoint2D
edgeMidPosition state graphId sourceId targetId renderPane = do
  GraphSpacePoint2D sourcePos <- edgeSourcePosition state graphId sourceId renderPane
  GraphSpacePoint2D targetPos <- edgeTargetPosition state graphId targetId renderPane
  pure $ GraphSpacePoint2D
           { x : (sourcePos.x + targetPos.x) / 2.0
           , y : (sourcePos.y + targetPos.y) / 2.0
           }

-- | Utility to indicate if an operation received from the server updates a node's
-- | text, in which case the contenteditable field needs to be refreshed.
nodesWithTextUpdate :: AppOperation -> Array (Tuple GraphId NodeId)
nodesWithTextUpdate (AppOperation {target, op, historyUpdate, undoneUpdate}) =
  op
  <#> graphAndNodeId
  # Array.catMaybes
    where
      graphAndNodeId (GraphElementOperation graphId (UpdateNodeText nodeId _ _)) =
        Just $ Tuple graphId nodeId
      graphAndNodeId _ = Nothing

focusNode :: AppState -> Maybe Node
focusNode state = do
  graphId    <- state.focusedPane
  graphState <- Map.lookup graphId state.megagraph.graphs
  focus      <- graphState.view.focus
  nodeId     <- case focus of
    FocusNode nodeId -> Just nodeId
    _ -> Nothing
  Map.lookup nodeId graphState.graph.nodes

focusNodeSubgraph :: AppState -> Maybe GraphId
focusNodeSubgraph state = do
  node <- focusNode state
  subgraphId <- node.subgraph
  pure subgraphId

-- | To draw an SVG porabola that goes through points
-- | $\mathbf{P}_0, \mathbf{P}_1, \mathbf{P}_2$, we can use a
-- | quadratic Bézier curve with a suitable control point $\mathbf{CP}$.
-- |
-- | A quadradic Bézier curve defined by points
-- | $\mathbf{P}_0, \mathbf{CP}, \mathbf{P}_2$ can be parameterised by the
-- | double interpolation[1]:
-- |         $B(t) = (1 - t)[(1 - t)\mathbf{P}_0 + t\mathbf{CP}] + t[(1 - t)\mathbf{CP} + t\mathbf{P}_2]$
-- |
-- | Setting $\mathbf{B}(\frac{1}{2}) = \mathbf{P}_1$ and rearranging gives:
-- |         $\mathbf{CP} = 2\mathbf{P}_1 - \frac{\mathbf{P}_0 + \mathbf{P}_2}{2}
-- |
-- | [1] https://en.wikipedia.org/wiki/B%C3%A9zier_curve
bezierControlPointFromParabolaPoints :: Point2D -> Point2D -> Point2D -> Point2D
bezierControlPointFromParabolaPoints p0 p1 p2 =
  { x : 2.0 * p1.x - (p0.x + p2.x) / 2.0
  , y : 2.0 * p1.y - (p0.y + p2.y) / 2.0
  }
