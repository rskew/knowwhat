module GraphComponent.Utils where

import Prelude

import AppOperation (AppOperation(..))
import AppState (AppState, DrawingEdge)
import Data.Array as Array
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Math as Math
import Megagraph (GraphId, GraphSpacePoint2D(..), GraphView, Node, NodeId, PageSpacePoint2D(..), Point2D, Point2DPolar, graphSpaceToPageSpace, nodePosition, pageSpaceToGraphSpace)
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
    haloRadius > euclideanDistance (nodePosition node) pointPositionGraphSpace

lookupNodePositionInPane :: AppState -> GraphId -> NodeId -> GraphView -> Maybe GraphSpacePoint2D
lookupNodePositionInPane state graphId nodeId renderPane = do
  graphState <- Map.lookup graphId state.megagraph.graphs
  node <- Map.lookup nodeId graphState.graph.nodes
  pure $ (nodePosition node) # graphSpaceToPageSpace graphState.view # pageSpaceToGraphSpace renderPane

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

type Parabola
  = { p0 :: Point2D
    , p1 :: Point2D
    , p2 :: Point2D
    }

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
bezierControlPointFromParabolaPoints :: Parabola -> Point2D
bezierControlPointFromParabolaPoints parabola =
  { x : 2.0 * parabola.p1.x - (parabola.p0.x + parabola.p2.x) / 2.0
  , y : 2.0 * parabola.p1.y - (parabola.p0.y + parabola.p2.y) / 2.0
  }

parallelParabola :: Number -> Parabola -> Parabola
parallelParabola offset parabola =
  let
    bezierControl = bezierControlPointFromParabolaPoints parabola
    controlP0OrthogonalUnit =
      bezierControl - parabola.p0
      # orthogonalVector2D
      # unitVector2D
    controlP2OrthogonalUnit =
      bezierControl - parabola.p2
      # orthogonalVector2D >>> scalarMult2D (-1.0)
      # unitVector2D
    averageOffsetVecUnit =
      { x : (controlP0OrthogonalUnit.x + controlP2OrthogonalUnit.x) / 2.0
      , y : (controlP0OrthogonalUnit.y + controlP2OrthogonalUnit.y) / 2.0
      }
      # unitVector2D
  in
    { p0 : parabola.p0 + (scalarMult2D offset controlP0OrthogonalUnit)
    , p1 : parabola.p1 + (scalarMult2D offset averageOffsetVecUnit)
    , p2 : parabola.p2 + (scalarMult2D offset controlP2OrthogonalUnit)
    }

orthogonalVector2D :: Point2D -> Point2D
orthogonalVector2D vec =
  cartesianToPolar vec
  # \polar -> polar { angle = polar.angle + Math.pi / 2.0 }
  # polarToCartesian

cartesianToPolar :: Point2D -> Point2DPolar
cartesianToPolar cartVec =
  { angle : Math.atan2 cartVec.y cartVec.x
  , radius : norm2D cartVec
  }

norm2D :: Point2D -> Number
norm2D vec = Math.sqrt (vec.x * vec.x + vec.y * vec.y)

polarToCartesian :: Point2DPolar -> Point2D
polarToCartesian polarVec =
  { x : polarVec.radius * (Math.cos polarVec.angle)
  , y : polarVec.radius * (Math.sin polarVec.angle)
  }

unitVector2D :: Point2D -> Point2D
unitVector2D vec =
  let
    magnitude = norm2D vec
  in
    { x : vec.x / magnitude
    , y : vec.y / magnitude
    }

scalarMult2D :: Number -> Point2D -> Point2D
scalarMult2D scale vec = { x : vec.x * scale, y : vec.y * scale }
