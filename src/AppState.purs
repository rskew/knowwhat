module AppState where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Collapsable (class Collapsable)
import Data.Group (class Group)
import Data.Lens (Lens', Traversal', lens, traversed, over, (^.), (.~))
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.List (List)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Monoid.Action (class ActionM)
import Data.Newtype (class Newtype)
import Data.NonEmpty (NonEmpty(..))
import Data.NonEmpty as NonEmpty
import Data.Symbol (SProxy(..), class IsSymbol)
import Data.Traversable (traverse, foldl)
import Data.Tuple (Tuple(..), fst, snd)
import Data.UUID (UUID)
import Data.Undoable (Undoable, _current)
import Data.Undoable.UndoOp (_undoOp, UNDOOP, handleUndoOpM)
import Effect (Effect)
import Foreign (Foreign, unsafeToForeign)
import Foreign as Foreign
import Foreign.Class (class Encode, class Decode, decode)
import Foreign.Unit (ForeignUnit)
import Point2D (Point2D)
import Prim.Row (class Cons)
import Run (Run(..), FProxy, Step(..))
import Run as Run
import Web.HTML.HTMLElement as WHE
import Workflow.Core (Graph, Edge, NodeId, EdgeId, _nodes, _source, _target, _pos)
import Workflow.Graph.GraphOp (GraphOpF, _graphOp, GRAPHOP, handleGraphOp, showGraphOp, invertGraphOp, encodeGraphOpF)
import Workflow.Synth (Synth, SynthParams)
import Workflow.Synth.SynthOp (_synthOp, SYNTHOP, SynthOpF, interpretSynthOp, handleGraphOpAsSynthUpdate, handleUIGraphOpAsSynthUpdate, invertSynthOp, showSynthOp, encodeSynthOpF, collapseSynthOp)
import Workflow.UIGraph.UIGraphOp (UIGraphOpF, UIGRAPHOP, _uiGraphOp, handleUIGraphOp, invertUIGraphOp, collapseUIGraphOp, encodeUIGraphOpF, showUIGraphOp)


appStateVersion :: String
appStateVersion = "0.0.0.0.0.0.0.1"

------
-- Types

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

edgeIdStr :: Edge -> String
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
  { graph :: Graph
  , drawingEdges :: Map DrawingEdgeId DrawingEdge
  , hoveredElementId :: Maybe HoveredElementId
  , boundingRect :: WHE.DOMRect
  , graphOrigin :: PageSpacePos
  , zoom :: Number
  , synth :: Synth
  }
newtype AppStateCurrent = AppStateCurrent AppStateInner
derive instance newtypeAppStateCurrent :: Newtype AppStateCurrent _

type AppState = Undoable AppStateCurrent (AppOperation Unit) List

-- | Action instance required by Undoable
instance actGraphOpAppState :: ActionM Effect (AppOperation Unit) AppStateCurrent where
  actM op = interpretAppOperation op

type UninitializedAppStateInner =
  { graph :: Graph
  , drawingEdges :: Map DrawingEdgeId DrawingEdge
  , hoveredElementId :: Maybe HoveredElementId
  , boundingRect :: WHE.DOMRect
  , graphOrigin :: PageSpacePos
  , zoom :: Number
  , synthParams :: SynthParams
  }

type UninitializedAppState = Undoable UninitializedAppStateInner (AppOperation Unit) List


------
-- Lenses

_AppStateCurrent :: Lens' AppStateCurrent AppStateInner
_AppStateCurrent = (lens (\(AppStateCurrent appStateCurrent) -> appStateCurrent) (\_ -> AppStateCurrent))

_graph' :: Lens' AppStateCurrent Graph
_graph' = _AppStateCurrent <<< prop (SProxy :: SProxy "graph")

_graph :: Lens' AppState Graph
_graph = _current <<< _graph'

_drawingEdges :: Lens' AppState (Map DrawingEdgeId DrawingEdge)
_drawingEdges = _current <<< _AppStateCurrent <<< prop (SProxy :: SProxy "drawingEdges")

_drawingEdgePos :: DrawingEdgeId -> Traversal' AppState GraphSpacePos
_drawingEdgePos drawingEdgePos =
  _drawingEdges <<< at drawingEdgePos <<< traversed <<< prop (SProxy :: SProxy "pos")

_graphNodePos :: NodeId -> Traversal' AppState GraphSpacePos
_graphNodePos nodeId =
  _graph <<<_nodes <<< at nodeId <<< traversed <<< _pos <<< _coerceToGraphSpace

_synth' :: Lens' AppStateCurrent Synth
_synth' = _AppStateCurrent <<< prop (SProxy :: SProxy "synth")

_synth :: Lens' AppState Synth
_synth = _current <<< _synth'

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



------
-- AppOperation DSL for actions that change the graph state and
-- associated state (e.g. synth state) directly

type AppOperationRow =
  ( uiGraphOp :: UIGRAPHOP
  , synthOp :: SYNTHOP
  , graphOp :: GRAPHOP
  )

newtype AppOperation a = AppOperation (Run AppOperationRow a)

derive newtype instance functorAppOperation :: Functor AppOperation


interpretAppOperation :: forall a. AppOperation a
                         -> (AppStateCurrent -> Effect AppStateCurrent)
interpretAppOperation (AppOperation op) =
  let
    smooshTupleM (Tuple f g) = f >=> g
  in do
    op
    # interpretUIGraphOpAsAppStateUpdate               # map fst
    # interpretGraphOpAsAppStateUpdate                 # map smooshTupleM
    # interpretSynthOp # (map (lmap (overM _synth')) >>> map smooshTupleM)
    # Run.extract

interpretUIGraphOpAsAppStateUpdate :: forall r a.
                                      Run (uiGraphOp :: UIGRAPHOP | r) a
                                      -> Run r (Tuple (AppStateCurrent -> Effect AppStateCurrent) a)
interpretUIGraphOpAsAppStateUpdate =
  interpretOpMulti
  _uiGraphOp
  (NonEmpty
    (handleUIGraphOpAsSynthUpdate >>> lmap (overM _synth'))
    [ handleUIGraphOp >>> lmap (over _graph' >>> compose pure) ])
  (>=>)
  pure

interpretGraphOpAsAppStateUpdate :: forall r a.
                                    Run (graphOp :: GRAPHOP | r) a
                                    -> Run r (Tuple (AppStateCurrent -> Effect AppStateCurrent) a)
interpretGraphOpAsAppStateUpdate =
  interpretOpMulti
  _graphOp
  (NonEmpty
    (handleGraphOpAsSynthUpdate >>> lmap (overM _synth'))
    [ handleGraphOp >>> lmap (over _graph' >>> compose pure) ])
  (>=>)
  pure

instance showAppOperation :: Show (AppOperation a) where
  show (AppOperation op) =
    Run.extract (op # Run.runAccumPure
      (\accumulator -> Run.match
        { uiGraphOp : Loop <<< lmap (append accumulator) <<< showUIGraphOp
        , synthOp   : Loop <<< lmap (append accumulator) <<< showSynthOp
        , graphOp   : Loop <<< lmap (append accumulator) <<< showGraphOp
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
      , synthOp : invertSynthOp >>> Run.lift _synthOp
      , graphOp : invertGraphOp >>> Run.lift _graphOp
      }))

-- | Collapsable instance required by Undoable
instance collapsableAppOperation :: Collapsable (AppOperation Unit) where
  collapse (AppOperation opA) (AppOperation opB) = AppOperation <$> collapsedOp
    where
      callWithOp :: forall sym f a b r r'. Cons sym (FProxy f) r' r => IsSymbol sym =>
                    SProxy sym -> (f (Run r a) -> Maybe b) -> Run r a -> Maybe b
      callWithOp sym f =
        Run.resume
          (Run.on sym f (Run.default Nothing))
          (const Nothing)
      collapseWith :: forall sym f a b r r'. Cons sym (FProxy f) r' r => IsSymbol sym => Functor f =>
                      SProxy sym -> (f (Run r a) -> f (Run r a) -> Maybe (f b)) -> Run r a -> Run r a -> Maybe (Run r b)
      collapseWith sym collapser opC opD = opC # callWithOp sym (\opCF ->
        opD # callWithOp sym (\opDF ->
          collapser opCF opDF <#> Run.lift sym))
      collapsedOp :: Maybe (Run AppOperationRow Unit)
      collapsedOp =
        collapseWith _uiGraphOp collapseUIGraphOp opA opB
        <|> collapseWith _synthOp collapseSynthOp opA opB


------
-- UndoOp DSL for performing actions or undo/redoing performed actions

interpretUndoOp :: ActionM Effect (AppOperation Unit) AppStateCurrent
                   => Run (undoOp :: UNDOOP (AppOperation Unit)) Unit
                   -> (AppState -> (Effect AppState))
interpretUndoOp op =
  Run.extract
  $ op # Run.runAccumPure
         (\accumulator ->
           Run.on
           _undoOp
           (Loop <<< lmap ((>=>) accumulator) <<< handleUndoOpM)
           Done)
         (\accumulator a -> accumulator)
         pure


------
-- Serialisation/deserialisation

instance encodeAppOperation' :: Encode a => Encode (AppOperation a) where
  encode = unsafeToForeign <<< encodeAppOperation
instance decodeAppOperation' :: Decode (AppOperation ForeignUnit) where
  decode = decodeAppOperation >>> map AppOperation

encodeAppOperation :: forall a. AppOperation a -> Array Foreign
encodeAppOperation (AppOperation op) =
  Run.extract $
  (op # Run.runAccumPure
    (\accumulator -> Run.match
      { uiGraphOp : \uiGraphOp ->
         let
           Tuple encodedGraphOp next = encodeUIGraphOpF uiGraphOp
         in
           Loop $ Tuple (accumulator <> [encodedGraphOp]) next
      , synthOp : \synthOp ->
         let
           Tuple encodedSynthOp next = encodeSynthOpF synthOp
         in
          Loop $ Tuple (accumulator <> [encodedSynthOp]) next
      , graphOp : Loop <<< lmap (\encodedOp -> accumulator <> [encodedOp]) <<< encodeGraphOpF
      })
    (\accumulator a -> accumulator)
    [])

decodeAppOperation :: Foreign -> Foreign.F (Run AppOperationRow ForeignUnit)
decodeAppOperation foreignOpArray =
  let
    decodeUIGraphOp :: Foreign -> Foreign.F (Run AppOperationRow ForeignUnit)
    decodeUIGraphOp = map (Run.lift _uiGraphOp) <<< (decode :: Foreign -> Foreign.F (UIGraphOpF ForeignUnit))
    decodeSynthOp :: Foreign -> Foreign.F (Run AppOperationRow ForeignUnit)
    decodeSynthOp = map (Run.lift _synthOp) <<< (decode :: Foreign -> Foreign.F (SynthOpF ForeignUnit))
    decodeGraphOp :: Foreign -> Foreign.F (Run AppOperationRow ForeignUnit)
    decodeGraphOp = map (Run.lift _graphOp) <<< (decode :: Foreign -> Foreign.F (GraphOpF ForeignUnit))
    tryDecode op = decodeUIGraphOp op <|> decodeSynthOp op <|> decodeGraphOp op
  in do
    arrayForeign <- Foreign.readArray foreignOpArray
    decodedOperations <- traverse tryDecode arrayForeign
    case Array.uncons decodedOperations of
      Nothing -> Foreign.fail $ Foreign.ForeignError "No operations in decoded array"
      Just unconsOps -> pure $ foldl (\ops op -> (ops >>= const op)) unconsOps.head unconsOps.tail


------
-- Utilities

overM :: forall s a' m. Monad m => Lens' s a' -> (a' -> m a') -> s -> m s
overM lens_ f val = do
  newSubVal <- f $ val ^. lens_
  pure $ (val # (lens_ .~ newSubVal))

interpretOpMulti :: forall sym f a b r r'.
                    Cons sym (FProxy f) r' r
                    => IsSymbol sym
                    => SProxy sym
                    -> NonEmpty Array (f (Run r a) -> Tuple b (Run r a))
                    -> (b -> b -> b)
                    -> b
                    -> Run r a
                    -> Run r' (Tuple b a)
interpretOpMulti sym handlers joiner =
  Run.runAccumPure
  (\accumulator ->
    Run.on
    sym
    (\symOp ->
      let
        appliedHandlers = handlers <#> \handler -> handler symOp
        handlerFuncs = fst <$> appliedHandlers
        next = snd $ NonEmpty.head appliedHandlers
        handlerAccumulator = foldl joiner (NonEmpty.head handlerFuncs) (NonEmpty.tail handlerFuncs)
        combinedHandler = joiner accumulator handlerAccumulator
      in
       Loop $ Tuple combinedHandler next)
    Done)
  (\accumulator a -> Tuple accumulator a)
