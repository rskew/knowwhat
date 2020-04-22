module GraphComponent.Utils where

import Prelude

import AppState (AppState, DrawingEdge)
import Data.Array as Array
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Halogen.HTML.Properties as HP
import Math as Math
import Megagraph (GraphId, GraphSpacePoint2D(..), Node, NodeId, PageSpacePoint2D(..), Point2D, Point2DPolar, GraphView, graphSpaceToPageSpace, nodePosition, pageSpaceToGraphSpace)
import MegagraphStateUpdate (MegagraphStateUpdate(..))
import Svg.Attributes as SA
import Svg.Types as SVGT
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
  graph <- Map.lookup graphId state.megagraph.graphs
  view <- Map.lookup graphId state.panes
  node <- Map.lookup nodeId graph.nodes
  pure $ (nodePosition node) # graphSpaceToPageSpace view # pageSpaceToGraphSpace renderPane

-- | Indicated which nodes are updated in a MegagraphUpdate. Used to update the
-- | text fields when a node is updated from a server message.
updatedNodes :: Array MegagraphStateUpdate -> Array (Tuple GraphId NodeId)
updatedNodes op =
  Array.concatMap graphAndNodeId op
    where
      graphAndNodeId (UpdateNodes from to) =
        (\node -> Tuple node.graphId node.id) <$> to
      graphAndNodeId _ = []

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

quadraticSvgBezierPath :: forall r i. Parabola -> HP.IProp (d :: String | r) i
quadraticSvgBezierPath parabola =
  let
    quadraticBezierControlPoint = bezierControlPointFromParabolaPoints parabola
  in
    SA.d [ SVGT.Abs (SVGT.M parabola.p0.x parabola.p0.y)
         , SVGT.Abs (SVGT.Q quadraticBezierControlPoint.x quadraticBezierControlPoint.y parabola.p2.x parabola.p2.y)
         ]

-- | Add some extra curviness to an otherwise parabolic edge by splitting the
-- | single control point of a quadratic bezier into two (making a cubic bezier),
-- | and moving these apart along the direction of the start and end-points (or
-- | the direction orthogonal to the vector between the midpoint of the start and
-- | end-points and the quadratic bezier control point).
-- |
-- | This is done by taking the vector between the midpoint of the two endpoints
-- | and the quadratic control point $v_1$, and moving the cubic control points perpendicular
-- | to $v_1$ in either direction.
cubicBezierControlPoints :: Parabola -> Number -> Tuple Point2D Point2D
cubicBezierControlPoints parabola shiftAmount =
  let
    quadraticBezierControlPoint = bezierControlPointFromParabolaPoints parabola
    endpointMidpoint = { x: 0.5 * (parabola.p0.x + parabola.p2.x)
                       , y: 0.5 * (parabola.p0.y + parabola.p2.y)
                       }
    v1 = { x: quadraticBezierControlPoint.x - endpointMidpoint.x
         , y: quadraticBezierControlPoint.y - endpointMidpoint.y
         }
    dotProd v w = v.x*w.x + v.y*w.y
    v1Norm = Math.sqrt $ dotProd v1 v1
    v1OrthogonalUnit = {x: -v1.y / v1Norm, y: v1.x / v1Norm}
    v1OrthogonalShift = { x: v1OrthogonalUnit.x * shiftAmount
                        , y: v1OrthogonalUnit.y * shiftAmount
                        }
    controlPointA = quadraticBezierControlPoint - v1OrthogonalShift
    controlPointB = quadraticBezierControlPoint + v1OrthogonalShift
  in
    if dotProd (controlPointB - controlPointA) (parabola.p2 - parabola.p0) > 0.0
    -- control points are aligned with curve direction
    then Tuple controlPointA controlPointB
    -- control points are anti-aligned with curve direction
    else Tuple controlPointB controlPointA

cubicSvgBezierPath :: forall r i. Number -> Parabola -> HP.IProp (d :: String | r) i
cubicSvgBezierPath shift parabola =
  let
    Tuple controlPointA controlPointB = cubicBezierControlPoints parabola shift
  in
    SA.d [ SVGT.Abs (SVGT.M parabola.p0.x parabola.p0.y)
         , SVGT.Abs (SVGT.C controlPointA.x controlPointA.y controlPointB.x controlPointB.y parabola.p2.x parabola.p2.y)
         ]

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
