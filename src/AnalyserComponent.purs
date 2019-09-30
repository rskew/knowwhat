module AnalyserComponent where

import Prelude

import AnalyserComponent.Utils (animationFrameLoop)
import AppState (Shape)
import Audio.WebAudio.AnalyserNode (getByteFrequencyData) as WebAudio
import Audio.WebAudio.Types (AnalyserNode) as WebAudio
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Array as Array
import Data.ArrayBuffer.Typed (toArray)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Foldable (foldl)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Data.UInt as UInt
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Console (log)
import Effect.Ref (Ref)
import Math as Math
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Svg.Attributes as SA
import Svg.Elements as SE
import Web.DOM.Element as Element
import Web.DOM.Node as DN
import Web.DOM.NodeList as NL
import Web.DOM.DOMTokenList as DTL
import Web.HTML.HTMLElement as HTMLElement
import Synth (defaultFrequencyBinCount)


analyserRefLabel :: H.RefLabel
analyserRefLabel = H.RefLabel "analyser"

spectrumClass :: String
spectrumClass = "spectrum-polylines"

type State = { analyserNode :: WebAudio.AnalyserNode
             , spectrumBuffer :: Uint8Array
             , drawLoopStopSignal :: Ref Boolean
             , shape :: Shape
             }

type Input = State

data Action
  = Init

data Query a
  = Resize Shape a

type Message = Unit

-- Slot type for parent components using a child GraphComponent
type Slot = H.Slot Query Message

type Slots = ()

analyser :: H.Component HH.HTML Query Input Message Aff
analyser =
  H.mkComponent
    { initialState : identity
    , render : render
    , eval: H.mkEval (H.defaultEval { handleAction = handleAction
                                    , handleQuery = handleQuery
                                    , initialize   = Just Init
                                    })
    }
  where

  render :: State -> H.ComponentHTML Action Slots Aff
  render state =
    SE.g
    []
    [ SE.polyline
      [ SA.class_ spectrumClass
      ]
    -- User a div inside a foreignObject as a hack to access the polyline
    -- element via HP.ref
    , SE.foreignObject
      []
      [ HH.div
        [ HP.ref analyserRefLabel
        ]
        []
      ]
    ]

  handleAction :: Action -> H.HalogenM State Action Slots Message Aff Unit
  handleAction = case _ of
    Init -> do
      state <- H.get
      let
        widthMultiplier = state.shape.width / (Math.log ((toNumber defaultFrequencyBinCount) + 1.0))
        heightMultiplier = state.shape.height / 256.0
        drawSpectrumSVG :: DN.Node -> Effect Unit
        drawSpectrumSVG analyserDOMNode = do
          WebAudio.getByteFrequencyData state.analyserNode state.spectrumBuffer
          spectrumArray <- toArray state.spectrumBuffer
          let
            spectrumPositions = spectrumArray # Array.mapWithIndex \index binAmplitude ->
              Tuple ((Math.log ((toNumber index) + 1.0)) * widthMultiplier)
                    (state.shape.height - (UInt.toNumber binAmplitude) * heightMultiplier)
            spectrumPath = foldl (<>) ""
              (spectrumPositions <#> \(Tuple x y) -> " " <> show x <> "," <> show y)
          -- Update spectrum display
          children <- DN.childNodes analyserDOMNode >>= NL.toArray
          for_ children \childNode -> do
            let maybeChildElement = Element.fromNode childNode
            case Element.fromNode childNode of
              Nothing -> pure unit
              Just childElement -> do
                classList <- Element.classList childElement
                isSpectrumPolyline <- DTL.contains classList spectrumClass
                if isSpectrumPolyline
                then do
                  Element.setAttribute "points" spectrumPath childElement
                else
                  pure unit
      maybeAnalyserDOMNode <- getAnalyserElement
      case maybeAnalyserDOMNode of
        Nothing -> pure unit
        Just analyserDOMNode -> do
          H.liftEffect $ log "starting analyser update loop"
          stopLoop <- H.liftEffect $ animationFrameLoop state.drawLoopStopSignal
                                   $ drawSpectrumSVG analyserDOMNode
          pure unit

  handleQuery :: forall a. Query a -> H.HalogenM State Action Slots Message Aff (Maybe a)
  handleQuery = case _ of
    Resize newShape a -> Just a <$ do
      H.modify_ _{ shape = newShape }

-- Get the div inside the foreignObject as HP.ref doesn't seem to work for
-- SVG elements
getAnalyserElement :: H.HalogenM State Action Slots Message Aff (Maybe DN.Node)
getAnalyserElement = H.getHTMLElementRef analyserRefLabel >>= case _ of
  Nothing -> do
    H.liftEffect $ log "couldn't get analyser element"
    pure Nothing
  Just htmlElement -> H.liftEffect $ runMaybeT do
    foreignObject <- MaybeT $ DN.parentNode $ Element.toNode $ HTMLElement.toElement htmlElement
    MaybeT $ DN.parentNode foreignObject
