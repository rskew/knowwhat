module AnalyserComponent where

import Prelude

import AppState (Shape)
import Audio.WebAudio.AnalyserNode (getByteFrequencyData) as WebAudio
import Audio.WebAudio.Types (AnalyserNode) as WebAudio
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.ArrayBuffer.Typed (traverseWithIndex_)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Int (toNumber, round)
import Data.Maybe (Maybe(..))
import Data.UInt as UInt
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Ref (Ref)
import Graphics.Canvas (Context2D)
import Graphics.Canvas as Canvas
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Svg.Attributes as SA
import Svg.Elements as SE
import Unsafe.Coerce (unsafeCoerce)
import Utils (animationFrameLoop)
import Web.HTML.HTMLCanvasElement as HTMLCanvasElement
import Workflow.Synth (defaultFrequencyBinCount)


canvasRefLabel :: H.RefLabel
canvasRefLabel = H.RefLabel "canvas"

type State = { analyserNode :: WebAudio.AnalyserNode
             , spectrumBuffer :: Uint8Array
             , drawLoopStopSignal :: Ref Boolean
             , shape :: Shape
             }

type Input = State

initialState :: Input -> State
initialState = identity

data Action = Init

data Query a = Resize Shape a

type Message = Unit

-- Slot type for parent components using a child GraphComponent
type Slot = H.Slot Query Message

type Slots = ()

analyser :: H.Component HH.HTML Query Input Message Aff
analyser =
  H.mkComponent
    { initialState : initialState
    , render : render
    , eval: H.mkEval (H.defaultEval { handleAction = handleAction
                                    , handleQuery = handleQuery
                                    , initialize   = Just Init
                                    })
    }
  where

  render :: State -> H.ComponentHTML Action Slots Aff
  render state =
    SE.foreignObject
    [ SA.class_ "analyser"
    , SA.height $ SA.Length $ SA.Px state.shape.height
    , SA.width $ SA.Length $ SA.Px state.shape.width
    ]
    [ HH.canvas
      [ HP.ref canvasRefLabel
      , HP.classes [ H.ClassName "analyser-canvas" ]
      , HP.height $ round state.shape.height
      , HP.width $ round state.shape.width
      ]
    ]

  handleAction :: Action -> H.HalogenM State Action Slots Message Aff Unit
  handleAction = case _ of
    Init -> do
      state <- H.get
      let
        widthMultiplier = state.shape.width / (toNumber defaultFrequencyBinCount)
        heightMultiplier = state.shape.height / 256.0
        drawSpectrumOnCanvas :: Context2D -> WebAudio.AnalyserNode -> Uint8Array -> Effect Unit
        drawSpectrumOnCanvas context2D analyserNode' spectrumBuffer = do
          WebAudio.getByteFrequencyData analyserNode' spectrumBuffer
          Canvas.clearRect context2D { x : 0.0
                                     , y : 0.0
                                     , width : state.shape.width
                                     , height : state.shape.height
                                     }
          Canvas.beginPath context2D
          spectrumBuffer # traverseWithIndex_ \index freqUInt ->
              Canvas.lineTo context2D ((toNumber index) * widthMultiplier) ((UInt.toNumber freqUInt) * heightMultiplier)
          Canvas.stroke context2D
      void $ runMaybeT do
        htmlElement <- MaybeT $ H.getHTMLElementRef canvasRefLabel
        canvasElement <- MaybeT $ pure $ HTMLCanvasElement.fromHTMLElement htmlElement
        MaybeT $ Just <$> do
          context2D <- H.liftEffect $ Canvas.getContext2D $ unsafeCoerce canvasElement
          stopLoop <- H.liftEffect $ animationFrameLoop state.drawLoopStopSignal
                                   $ drawSpectrumOnCanvas context2D state.analyserNode state.spectrumBuffer
          pure unit

  handleQuery :: forall a. Query a -> H.HalogenM State Action Slots Message Aff (Maybe a)
  handleQuery = case _ of
    Resize newShape a -> Just a <$ do
      H.modify_ _{ shape = newShape }
