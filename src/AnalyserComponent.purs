module AnalyserComponent where

import Prelude

import AppState (Shape)
import Audio.WebAudio.AnalyserNode (getByteFrequencyData) as WebAudio
import Audio.WebAudio.Types (AnalyserNode) as WebAudio
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
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Svg.Attributes as SA
import Svg.Elements as SE
import Utils (animationFrameLoop)
import Web.DOM.Document as Document
import Web.DOM.Element as Element
import Web.DOM.Node as DN
import Web.DOM.NodeList as NL
import Web.HTML as WH
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window as WHW
import Workflow.Synth (defaultFrequencyBinCount)


analyserRefLabel :: H.RefLabel
analyserRefLabel = H.RefLabel "analyser"

initialZoom :: Number
initialZoom = 1.0

type State = { analyserNode :: WebAudio.AnalyserNode
             , spectrumBuffer :: Uint8Array
             , drawLoopStopSignal :: Ref Boolean
             , shape :: Shape
             , zoom :: Number
             , zoomRef :: Maybe (Ref Number)
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
    [ SA.class_ "analyser"
    , HP.ref analyserRefLabel
    ]
    [ SE.rect
      [ HE.onClick \_ -> Just Init
      , SA.height $ SA.Length $ SA.Px 100.0
      , SA.width $ SA.Length $ SA.Px 100.0
      ]
    ]

  handleAction :: Action -> H.HalogenM State Action Slots Message Aff Unit
  handleAction = case _ of
    Init -> do
      H.liftEffect $  log "initialising spectrum vis"
      state <- H.get
      let
        widthMultiplier = state.shape.width / (toNumber defaultFrequencyBinCount)
        heightMultiplier = state.shape.height / 256.0
        drawSpectrumSVG :: Element.Element -> Effect Unit
        drawSpectrumSVG analyserElement = do
          log "entering drawSpectrumSVG"
          WebAudio.getByteFrequencyData state.analyserNode state.spectrumBuffer
          spectrumArray <- toArray state.spectrumBuffer
          let
            spectrumPositions = spectrumArray # Array.mapWithIndex \index binAmplitude ->
              Tuple ((toNumber index) * widthMultiplier) ((UInt.toNumber binAmplitude) * heightMultiplier)
            spectrumPath = foldl (<>) "M0.0,0.0 "
              (spectrumPositions <#> \(Tuple x y) -> "L" <> show x <> "," <> show y <> " ")
          -- Create the new spectrum SVG element and append it to the analyser g
          let analyserNode = Element.toNode analyserElement
          -- Remove existing spectrum
          children <- DN.childNodes analyserNode >>= NL.toArray
          for_ children \child -> DN.removeChild child analyserNode
          -- Add new spectrum
          window <- WH.window
          documentHTML <- WHW.document window
          let document = HTMLDocument.toDocument documentHTML
          newPathElement <- Document.createElement "path" document
          Element.setAttribute "d" spectrumPath newPathElement
          Element.setAttribute "class" "spectrum-path" newPathElement
          let newPathNode = Element.toNode newPathElement
          _ <- DN.appendChild newPathNode analyserNode
          log "drew spectrum SVG"
          pure unit
      maybeAnalyserElement <- getAnalyserElement
      case maybeAnalyserElement of
        Nothing -> pure unit
        Just analyserElement -> do
          H.liftEffect $ log "starting loop"
          stopLoop <- H.liftEffect $ animationFrameLoop state.drawLoopStopSignal
                                   $ drawSpectrumSVG analyserElement
          pure unit

  handleQuery :: forall a. Query a -> H.HalogenM State Action Slots Message Aff (Maybe a)
  handleQuery = case _ of
    Resize newShape a -> Just a <$ do
      H.modify_ _{ shape = newShape }

getAnalyserElement :: H.HalogenM State Action Slots Message Aff (Maybe Element.Element)
getAnalyserElement = H.getHTMLElementRef analyserRefLabel >>= case _ of
  Nothing -> do
    H.liftEffect $ log "couldn't get analyser element"
    pure Nothing
  Just htmlElement -> pure $ Just $ HTMLElement.toElement htmlElement
