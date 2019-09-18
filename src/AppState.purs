module AppState where

import Prelude
import Workflow.UIGraph.UIGraphOp (UIGraphOpF, UIGRAPHOP, _uiGraphOp, interpretUIGraphOp, invertUIGraphOp, collapseUIGraphOp, encodeUIGraphOpF, showUIGraphOp)

import Data.Bifunctor (lmap)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Collapsable (class Collapsable)
import Data.Array as Array
import Data.Group (class Group)
import Data.Lens (Lens', Traversal', lens, traversed, (^.), (.~), (%~), over)
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Monoid.Action (class ActionM)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse, foldl)
import Data.Tuple (Tuple(..))
import Data.UInt (UInt)
import Data.UUID (UUID)
import Data.Undoable (Undoable, _current)
import Effect (Effect)
import Foreign (Foreign, unsafeToForeign)
import Foreign as Foreign
import Foreign.Class (class Encode, class Decode, decode)
import Foreign.Unit (ForeignUnit)
import Point2D (Point2D)
import Run (Run(..), FProxy, Step(..))
import Run as Run
import Web.HTML.HTMLElement as WHE
import Workflow.Core (NodeId, EdgeId, _nodes, _source, _target)
import Workflow.Synth (SynthState, SynthNodeState, interpretToSynthUpdate)
import Workflow.UIGraph.Types (UIEdge, UIGraph, _pos)


appStateVersion :: String
appStateVersion = "0.0.0.0.0.0.0.1"

type Shape = { width :: Number, height :: Number }

type GraphId = UUID

-- | A position in graph space, as distinct from a point on the page/window
newtype GraphSpacePos = GraphSpacePos Point2D

-- | PageSpacePos represents a position on the browser window such as
-- | a mouse position, as distinct from a position in graph space.
newtype PageSpacePos = PageSpacePos Point2D

toGraphSpace :: WHE.DOMRect -> PageSpacePos -> Number -> PageSpacePos -> GraphSpacePos
toGraphSpace boundingRect (PageSpacePos graphOrigin) zoom (PageSpacePos pagePos) =
  GraphSpacePos $ { x : (pagePos.x - boundingRect.left - graphOrigin.x) * zoom
                  , y : (pagePos.y - boundingRect.top - graphOrigin.y) * zoom
                  }

toPageSpace :: WHE.DOMRect -> Number -> GraphSpacePos -> PageSpacePos
toPageSpace boundingRect zoom (GraphSpacePos graphPos) =
  let
    graphOrigin = { x : boundingRect.left, y : boundingRect.top }
  in
    PageSpacePos $ { x : (graphPos.x + boundingRect.left + graphOrigin.x) / zoom
                   , y : (graphPos.y + boundingRect.top + graphOrigin.y) / zoom
                   }

type Edge = { source :: NodeId
            , target :: NodeId
            }

edgeIdStr :: UIEdge -> String
edgeIdStr edge = show (edge ^. _source) <> "_" <> show (edge ^. _target)

type DrawingEdge = { source :: NodeId
                   , pos :: GraphSpacePos
                   }

type DrawingEdgeId = NodeId

drawingEdgeKey :: DrawingEdgeId -> String
drawingEdgeKey id = "DrawingEdge_" <> show id

data DragSource
  = NodeDrag
  | HaloDrag
  | BackgroundDrag
derive instance eqDragSource :: Eq DragSource
derive instance ordDragSource :: Ord DragSource
instance showDragSource :: Show DragSource where
  show NodeDrag = "NodeDrag"
  show HaloDrag = "HaloDrag"
  show BackgroundDrag = "BackgroundDrag"

data HoveredElementId
  = NodeHaloId NodeId
  | NodeBorderId NodeId
  | EdgeBorderId EdgeId
derive instance eqGraphElementId :: Eq HoveredElementId

type AppStateInner =
  { graph :: UIGraph
  , drawingEdges :: Map DrawingEdgeId DrawingEdge
  , hoveredElementId :: Maybe HoveredElementId
  , boundingRect :: WHE.DOMRect
  , graphOrigin :: PageSpacePos
  , zoom :: Number
  , synthState :: SynthState
  , analyserBuffer :: Uint8Array
  , analyserArray :: Array UInt
  }
newtype AppStateCurrent = AppStateCurrent AppStateInner
derive instance newtypeAppStateCurrent :: Newtype AppStateCurrent _

type AppState = Undoable AppStateCurrent (AppOperation Unit)

-- | Action instance required by Undoable
instance actUIGraphOpAppState :: Monoid a => ActionM Effect (AppOperation a) AppStateCurrent where
  actM op = interpretAppOperation op

type UninitializedAppStateInner =
  { graph :: UIGraph
  , drawingEdges :: Map DrawingEdgeId DrawingEdge
  , hoveredElementId :: Maybe HoveredElementId
  , boundingRect :: WHE.DOMRect
  , graphOrigin :: PageSpacePos
  , zoom :: Number
  }

type UninitializedAppState =
  Undoable UninitializedAppStateInner (AppOperation Unit)

_AppStateCurrent :: Lens' AppStateCurrent AppStateInner
_AppStateCurrent = (lens (\(AppStateCurrent appStateCurrent) -> appStateCurrent) (\_ -> AppStateCurrent))

_graph' :: Lens' AppStateCurrent UIGraph
_graph' = _AppStateCurrent <<< prop (SProxy :: SProxy "graph")

_graph :: Lens' AppState UIGraph
_graph = _current <<< _graph'

_drawingEdges :: Lens' AppState (Map DrawingEdgeId DrawingEdge)
_drawingEdges = _current <<< _AppStateCurrent <<< prop (SProxy :: SProxy "drawingEdges")

_drawingEdgePos :: DrawingEdgeId -> Traversal' AppState GraphSpacePos
_drawingEdgePos drawingEdgePos =
  _drawingEdges <<< at drawingEdgePos <<< traversed <<< prop (SProxy :: SProxy "pos")

_graphNodePos :: NodeId -> Traversal' AppState GraphSpacePos
_graphNodePos nodeId =
  _graph <<<_nodes <<< at nodeId <<< traversed <<< _pos <<< _coerceToGraphSpace

_synthState' :: Lens' AppStateCurrent SynthState
_synthState' = _AppStateCurrent <<< prop (SProxy :: SProxy "synthState")

_synthState :: Lens' AppState SynthState
_synthState = _current <<< _synthState'

_synthNodeState :: NodeId -> Lens' AppState (Maybe SynthNodeState)
_synthNodeState nodeId = _synthState <<< prop (SProxy :: SProxy "synthNodes") <<< at nodeId

_hoveredElementId :: Lens' AppState (Maybe HoveredElementId)
_hoveredElementId = _current <<< _AppStateCurrent <<< prop (SProxy :: SProxy "hoveredElementId")

_graphOrigin :: Lens' AppState PageSpacePos
_graphOrigin = _current <<< _AppStateCurrent <<< prop (SProxy :: SProxy "graphOrigin")

_boundingRect :: Lens' AppState WHE.DOMRect
_boundingRect = _current <<< _AppStateCurrent <<< prop (SProxy :: SProxy "boundingRect")

_zoom :: Lens' AppState Number
_zoom = _current <<< _AppStateCurrent <<< prop (SProxy :: SProxy "zoom")

_coerceToGraphSpace :: Lens' Point2D GraphSpacePos
_coerceToGraphSpace = lens GraphSpacePos (\_ (GraphSpacePos pos) -> pos)

data GraphOpF next
  = Asdf next
  | Fdsa next
derive instance functorGraphOpF :: Functor GraphOpF

type GRAPHOP = FProxy GraphOpF

_asdf :: SProxy "asdf"
_asdf = SProxy

interpretGraphOp :: forall r a. a -> Run (asdf :: GRAPHOP | r) a -> Run r a
interpretGraphOp initial =
  Run.runAccumPure
  (\accumulator -> Run.on
                   _asdf
                   (case _ of
                       Asdf next -> Loop $ Tuple accumulator next
                       Fdsa next -> Loop $ Tuple accumulator next)
                   Done)
  (\accumulator a -> accumulator)
  initial

type AppOperationRow = (uiGraphOp :: UIGRAPHOP, asdf :: GRAPHOP)

newtype AppOperation a = AppOperation (Run AppOperationRow a)

derive newtype instance functorAppOperation :: Functor AppOperation

interpretAppOperation :: forall a. AppOperation a -> (AppStateCurrent -> Effect AppStateCurrent)
interpretAppOperation (AppOperation op) =
  let
    uiGraphUpdaterRun = interpretUIGraphOp op <#> over _graph'
    uiGraphUpdater = Run.extract $ (interpretGraphOp identity) uiGraphUpdaterRun
  in
    pure <<< uiGraphUpdater
  --Run.extract (op # Run.runAccumPure
  --  (\accumulator -> Run.match
  --    { uiGraphOp : \uiGraphOp ->
  --        let
  --          Tuple synthStateUpdater _ = interpretToSynthUpdate uiGraphOp
  --          Tuple uiGraphUpdater next = interpretUIGraphOp uiGraphOp
  --        in
  --          Loop $ Tuple (\appState -> do
  --                         newAppState <- accumulator appState
  --                         newSynthState <- synthStateUpdater (newAppState ^. _synthState')
  --                         pure $ newAppState
  --                                # _synthState' .~ newSynthState
  --                                # _graph' %~ uiGraphUpdater)
  --                       next
  --    , asdf : case _ of
  --       Asdf next -> Loop $ Tuple accumulator next
  --       Fdsa next -> Loop $ Tuple accumulator next
  --    })
  --  (\accumulator a -> accumulator)
  --  pure)

instance showAppOperation :: Show (AppOperation Unit) where
  show (AppOperation op) =
    Run.extract (op # Run.runAccumPure
      (\accumulator -> Run.match
        { uiGraphOp : Loop <<< lmap (append accumulator) <<< showUIGraphOp
        , asdf : case _ of
          Asdf next -> Loop $ Tuple accumulator next
          Fdsa next -> Loop $ Tuple accumulator next
        })
      (\accumulator a -> accumulator)
      "")

instance semigroupAppOperation :: Semigroup (AppOperation a) where
  append (AppOperation a) (AppOperation b) = AppOperation $ a >>= \_ -> b

instance monoidAppOperation :: Monoid a => Monoid (AppOperation a) where
  mempty = AppOperation (Run mempty)

instance groupAppOperation :: Monoid a => Group (AppOperation a) where
  ginverse (AppOperation op) = AppOperation inverseOp where
    inverseOp = op # (Run.interpret (Run.match
      { uiGraphOp : invertUIGraphOp >>> Run.lift _uiGraphOp
      , asdf : \asdf -> Run.lift (SProxy :: SProxy "asdf") asdf
      }))

  -- | Collapsable instance required by Undoable
  -- This is a bit of a shemozzle :/
instance collapsableAppOperation :: Collapsable (AppOperation Unit) where
  collapse (AppOperation opA) (AppOperation opB) = AppOperation <$> collapsedOp
    where
      collapsedOp :: Maybe (Run AppOperationRow Unit)
      collapsedOp =
        opA
        # (Run.resume
           (Run.on _uiGraphOp
            (\uiGraphOpA -> collapsedRightUIGraphOp uiGraphOpA opB)
            (Run.default Nothing))
           (const Nothing))
      collapsedRightUIGraphOp :: forall a. UIGraphOpF (Run AppOperationRow a) -> Run AppOperationRow a -> Maybe (Run AppOperationRow Unit)
      collapsedRightUIGraphOp uiGraphOpA opB' =
        opB'
        # (Run.resume
           (Run.on _uiGraphOp
            (\uiGraphOpB -> collapseUIGraphOp uiGraphOpA uiGraphOpB
                            <#> Run.lift _uiGraphOp)
            (Run.default Nothing))
           (const Nothing))


------
-- Serialisation/deserialisation

instance encodeAppOperation' :: Encode a => Encode (AppOperation a) where
  encode = unsafeToForeign <<< encodeAppOperation
instance decodeAppOperation' :: Decode (AppOperation ForeignUnit) where
  decode = decodeAppOperation >>> map AppOperation

encodeAppOperation :: forall a. AppOperation a -> Array Foreign
encodeAppOperation (AppOperation op) = Run.extract $ (op # Run.runAccumPure
  (\accumulator -> Run.match
    { uiGraphOp : \uiGraphOp ->
       let
         Tuple encodedUIGraphOp next = encodeUIGraphOpF uiGraphOp
       in
         Loop $ Tuple (accumulator <> [encodedUIGraphOp]) next
    , asdf : case _ of
      Asdf next -> Loop $ Tuple accumulator next
      Fdsa next -> Loop $ Tuple accumulator next
    })
  (\accumulator a -> accumulator)
  [])

decodeAppOperation :: Foreign -> Foreign.F (Run AppOperationRow ForeignUnit)
decodeAppOperation foreignOpArray =
  let
    tryDecode :: Foreign -> Foreign.F (Run AppOperationRow ForeignUnit)
    tryDecode = map (Run.lift _uiGraphOp) <<< (decode :: Foreign -> Foreign.F (UIGraphOpF ForeignUnit))
  in do
    arrayForeign <- Foreign.readArray foreignOpArray
    decodedOperations <- traverse tryDecode arrayForeign
    case Array.uncons decodedOperations of
      Nothing -> Foreign.fail $ Foreign.ForeignError "No operations in decoded array"
      Just unconsOps -> pure $ foldl (\ops op -> (ops >>= \_ -> op)) unconsOps.head unconsOps.tail
