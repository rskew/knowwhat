module Main where

import Prelude

import Config as Config
import Data.Int (toNumber)
import Effect (Effect)
import GraphComponent (graphComponent)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Web.HTML (window)
import Web.HTML.Window (innerWidth, innerHeight)
import Web.Socket.WebSocket as WS

main :: Effect Unit
main = do
  connection <- WS.create Config.webSocketURL []
  HA.runHalogenAff do
    body <- HA.awaitBody
    w <- H.liftEffect window
    windowWidth <- H.liftEffect $ innerWidth w
    windowHeight <- H.liftEffect $ innerHeight w
    void $ runUI
           graphComponent
           { windowShape: { width  : toNumber windowWidth
                          , height : toNumber windowHeight
                          }
           , webSocketConnection: connection
           }
           body
