module Main where

import Prelude

import Audio.WebAudio.BaseAudioContext (newAudioContext)
import Audio.WebAudio.Utils (createUint8Buffer)
import Data.Int (toNumber)
import Effect (Effect)
import Effect.Class.Console (log)
import GraphComponent as G
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Synth (defaultFrequencyBinCount)
import Web.HTML (window)
import Web.HTML.Window (innerWidth, innerHeight)

main :: Effect Unit
main =
  HA.runHalogenAff do
  body <- HA.awaitBody
  w <- H.liftEffect window
  windowWidth <- H.liftEffect $ innerWidth w
  windowHeight <- H.liftEffect $ innerHeight w
  H.liftEffect $ log $ "Window size: " <> show windowWidth <> " " <> show windowHeight
  let
    initialWindowBoundingRect =
      { left : 0.0
      , width : toNumber windowWidth
      , right : toNumber windowWidth
      , top : 0.0
      , height : toNumber windowHeight
      , bottom : toNumber windowHeight
      }
  audioContext <- H.liftEffect newAudioContext
  analyserBuffer <- H.liftEffect $ createUint8Buffer defaultFrequencyBinCount
  runUI G.graphComponent { windowBoundingRect : initialWindowBoundingRect
                         , audioContext : audioContext
                         }
                         body
