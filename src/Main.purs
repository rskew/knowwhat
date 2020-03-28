module Main where

import Prelude

import Config as Config
import Data.Int (toNumber)
import Effect (Effect)
import Effect.Console as Console
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
  H.liftEffect $ Console.log
    """
     |                                            |
  _` |  __| _` |\ \  \   /   _` |  __| _` | __ \  __ \   __|
 (   | |   (   | \ \  \ /   (   | |   (   | |   | | | |\__ \
\__,_|_|  \__,_|  \_/\_/   \__, |_|  \__,_| .__/ _| |_|____/
                           |___/           _|
                                            |     |
  __|  _` |\ \   / _ \ \ \  \   / _ \   __| |  _` |
\__ \ (   | \ \ /  __/  \ \  \ / (   | |    | (   |
____/\__,_|  \_/ \___|   \_/\_/ \___/ _|   _|\__,_|
    """
