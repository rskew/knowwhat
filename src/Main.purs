module Main where

import Prelude

import Audio.WebAudio.BaseAudioContext (newAudioContext)
import Audio.WebAudio.Utils (createUint8Buffer)
import Data.Int (toNumber)
import DemoGraph (demo)
import Effect (Effect)
import Effect.Class.Console (log)
import GraphPaneComponent as GP
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Web.HTML (window)
import Web.HTML.Window (innerWidth, innerHeight)
import Workflow.Synth (defaultFrequencyBinCount)

main :: Effect Unit
main =
  HA.runHalogenAff do
  body <- HA.awaitBody
  w <- H.liftEffect window
  windowWidth <- H.liftEffect $ innerWidth w
  windowHeight <- H.liftEffect $ innerHeight w
  H.liftEffect $ log $ "Window size: " <> show windowWidth <> " " <> show windowHeight
  let windowShape = { width : toNumber windowWidth
                    , height : toNumber windowHeight
                    }
  demoGraph <- H.liftEffect demo
  audioContext <- H.liftEffect newAudioContext
  analyserBuffer <- H.liftEffect $ createUint8Buffer defaultFrequencyBinCount
  runUI GP.paneComponent { windowShape : windowShape
                         , demoGraph : demoGraph
                         , audioContext : audioContext
                         }
                         body
