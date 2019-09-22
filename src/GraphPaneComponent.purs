module GraphPaneComponent where

import Prelude

import Audio.WebAudio.Types (AudioContext) as WebAudio
import AppState (Shape)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Effect.Console (log)
import GraphComponent as GraphComponent
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Elements.Keyed as HK
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import Web.Event.Event as WE
import Web.HTML as WH
import Web.HTML.Window as WHW
import Workflow.UIGraph.Types (UIGraph)

data PanePos = LeftPane | RightPane
derive instance ordPanePos :: Ord PanePos
derive instance eqPanePos :: Eq PanePos

data PaneConfiguration = SinglePane | DoublePane

type State = { configuration :: PaneConfiguration
             , windowShape :: Shape
             , demoGraph :: UIGraph
             , audioContext :: WebAudio.AudioContext
             }

data Action
  = PreventDefault WE.Event Action
  | StopPropagation WE.Event Action
  | EvalQuery (Query Unit)
  | Init
  | PaneMessage PanePos GraphComponent.Message

data Query a = ResizeWindow WE.Event a

type Input = { windowShape :: Shape
             , demoGraph :: UIGraph
             , audioContext :: WebAudio.AudioContext
             }

type Slots = ( panes :: GraphComponent.Slot PanePos )

_panes :: SProxy "panes"
_panes = SProxy

paneComponent :: H.Component HH.HTML Query Input Unit Aff
paneComponent =
  H.mkComponent
    { initialState : initialState
    , render : render
    , eval: H.mkEval (H.defaultEval { handleAction = handleAction
                                    , handleQuery = handleQuery
                                    , initialize   = Just Init
                                    })
    }
  where

  initialState :: Input -> State
  initialState inputs =
    { configuration : SinglePane
    , windowShape : inputs.windowShape
    , demoGraph : inputs.demoGraph
    , audioContext : inputs.audioContext
    }

  renderPane :: PanePos -> State -> H.ComponentHTML Action Slots Aff
  renderPane pos state =
    HH.slot
    _panes
    pos
    GraphComponent.graph
    { boundingRect : { left : 0.0
                     , width : state.windowShape.width
                     , right : state.windowShape.width
                     , top : 0.0
                     , height : state.windowShape.height
                     , bottom : state.windowShape.height
                     }
    , graph : state.demoGraph
    , audioContext : state.audioContext
    }
    (Just <<< PaneMessage pos)

  renderDivider :: H.ComponentHTML Action Slots Aff
  renderDivider =
    HH.div
    [ HP.classes [ HH.ClassName "paneDivider" ] ]
    []

  render :: State -> H.ComponentHTML Action Slots Aff
  render state =
    HK.div
    [ HP.classes [ HH.ClassName "pane" ] ]
    case state.configuration of
      SinglePane ->
        [ Tuple "LeftPane" $ renderPane LeftPane state ]
      DoublePane ->
        [ Tuple "LeftPane" $ renderPane LeftPane state
        , Tuple "divider" renderDivider
        , Tuple "RightPane" $ renderPane RightPane state
        ]

  handleAction :: Action -> H.HalogenM State Action Slots Unit Aff Unit
  handleAction = case _ of
    PreventDefault e q -> do
      H.liftEffect $ WE.preventDefault e
      handleAction q

    StopPropagation e q -> do
      H.liftEffect $ WE.stopPropagation e
      handleAction q

    Init -> do
      window <- H.liftEffect $ WH.window
      _ <- H.subscribe $ ES.eventListenerEventSource
                         (WE.EventType "resize")
                         (WHW.toEventTarget window)
                         \event -> Just $ EvalQuery $ ResizeWindow event unit
      pure unit

    PaneMessage panePos (GraphComponent.Focused focus) -> do
      H.liftEffect $ log $ "focus message received"

    PaneMessage panePos (GraphComponent.DrawEdge _) -> do
      H.liftEffect $ log $ "draw edge message received"

    PaneMessage panePos (GraphComponent.MappingMode) -> do
      state <- H.get
      case state.configuration of
        SinglePane -> do
          H.modify_ _{ configuration = DoublePane }
          -- only update LeftPane bounding rect, as the right pane will
          -- update its rect on Init
          _ <- H.query _panes LeftPane $ GraphComponent.UpdateBoundingRect unit
          H.liftEffect $ log "mapping mode!"
        DoublePane -> do
          H.modify_ _{ configuration = SinglePane }
          _ <- H.query _panes LeftPane $ GraphComponent.UpdateBoundingRect unit
          pure unit

    EvalQuery query -> do
      _ <- handleQuery query
      pure unit

  handleQuery :: forall a. Query a -> H.HalogenM State Action Slots Unit Aff (Maybe a)
  handleQuery = case _ of
    ResizeWindow resizeEvent a -> do
      H.liftEffect $ log "resizing"
      window <- H.liftEffect $ WH.window
      width <- H.liftEffect $ Int.toNumber <$> (WHW.innerWidth window)
      height <- H.liftEffect $ Int.toNumber <$> (WHW.innerHeight window)
      let newWindowShape = { width : width, height : height }
      H.liftEffect $ log $ "resizing " <> show newWindowShape
      state <- H.get
      H.put $ state { windowShape = newWindowShape }
      _ <- H.query _panes LeftPane $ GraphComponent.UpdateBoundingRect unit
      _ <- H.query _panes RightPane $ GraphComponent.UpdateBoundingRect unit
      pure $ Just a
