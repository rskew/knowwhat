module GraphPaneComponent where

import Prelude

import AppState (Shape)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Console (log)
import GraphComponent as GraphComponent
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Elements.Keyed as HK
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import Web.Event.Event as WE
import Web.Event.EventTarget as WET
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
             }

data Query a
  = PreventDefault WE.Event (Query a)
  | StopPropagation WE.Event (Query a)
  | Init a
  | ResizeWindow WE.Event (H.SubscribeStatus -> a)
  | PaneMessage PanePos GraphComponent.Message a

type Input = { windowShape :: Shape, demoGraph :: UIGraph }

type Slot = PanePos

paneComponent :: H.Component HH.HTML Query Input Unit Aff
paneComponent =
  H.lifecycleParentComponent
    { initialState : initialState
    , render
    , eval
    , receiver : const Nothing
    , initializer : Just $ H.action Init
    , finalizer : Nothing
    }
  where

  initialState :: Input -> State
  initialState inputs =
    { configuration : SinglePane
    , windowShape : inputs.windowShape
    , demoGraph : inputs.demoGraph
    }

  renderPane :: PanePos -> Shape -> UIGraph -> H.ParentHTML Query GraphComponent.Query Slot Aff
  renderPane pos windowShape demoGraph =
    HH.slot
    pos
    GraphComponent.graph
    { boundingRect : { left : 0.0
                     , width : windowShape.width
                     , right : windowShape.width
                     , top : 0.0
                     , height : windowShape.height
                     , bottom : windowShape.height
                     }
    , graph : demoGraph
    }
    (HE.input (PaneMessage pos))

  renderDivider :: H.ParentHTML Query GraphComponent.Query Slot Aff
  renderDivider =
    HH.div
    [ HP.classes [ HH.ClassName "paneDivider" ] ]
    []

  render :: State -> H.ParentHTML Query GraphComponent.Query Slot Aff
  render state =
    HK.div
    [ HP.classes [ HH.ClassName "pane" ] ]
    case state.configuration of
      SinglePane ->
        [ Tuple "LeftPane" $ renderPane LeftPane state.windowShape state.demoGraph ]
      DoublePane ->
        [ Tuple "LeftPane" $ renderPane LeftPane state.windowShape state.demoGraph
        , Tuple "divider" renderDivider
        , Tuple "RightPane" $ renderPane RightPane state.windowShape state.demoGraph
        ]

  eval :: Query ~> H.ParentDSL State Query GraphComponent.Query Slot Unit Aff
  eval = case _ of
    PreventDefault e q -> do
      H.liftEffect $ WE.preventDefault e
      eval q

    StopPropagation e q -> do
      H.liftEffect $ WE.stopPropagation e
      eval q

    Init next -> next <$ do
      window <- H.liftEffect $ WH.window
      let
        target = WHW.toEventTarget window
        attachResizeListener = \fn -> do
          listener <- WET.eventListener fn
          WET.addEventListener (WE.EventType "resize") listener false target
      H.subscribe $ ES.eventSource attachResizeListener (Just <<< H.request <<< ResizeWindow)

    ResizeWindow resizeEvent reply -> do
      H.liftEffect $ log "resizing"
      window <- H.liftEffect $ WH.window
      width <- H.liftEffect $ Int.toNumber <$> (WHW.innerWidth window)
      height <- H.liftEffect $ Int.toNumber <$> (WHW.innerHeight window)
      let newWindowShape = { width : width, height : height }
      H.liftEffect $ log $ "resizing " <> show newWindowShape
      state <- H.get
      H.put $ state { windowShape = newWindowShape }
      _ <- H.query LeftPane $ H.action GraphComponent.UpdateBoundingRect
      _ <- H.query RightPane $ H.action GraphComponent.UpdateBoundingRect
      pure $ reply H.Listening

    PaneMessage panePos (GraphComponent.Focused focus) next -> next <$ do
      H.liftEffect $ log $ "focus message received"

    PaneMessage panePos (GraphComponent.DrawEdge _) next -> next <$ do
      H.liftEffect $ log $ "draw edge message received"

    PaneMessage panePos (GraphComponent.MappingMode) next -> next <$ do
      state <- H.get
      case state.configuration of
        SinglePane -> do
          H.modify_ _{ configuration = DoublePane }
          -- only update LeftPane bounding rect, as the right pane will
          -- update its rect on Init
          _ <- H.query LeftPane $ H.action GraphComponent.UpdateBoundingRect
          H.liftEffect $ log "mapping mode!"
        DoublePane -> do
          H.modify_ _{ configuration = SinglePane }
          _ <- H.query LeftPane $ H.action GraphComponent.UpdateBoundingRect
          pure unit
