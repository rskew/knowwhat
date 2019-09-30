module UI.Constants where

import AppState (Shape)
import Core (Point2D)
import Math as Math
import Prelude

nodeRadius :: Number
nodeRadius = 7.0

groupNodeRadius :: Number
groupNodeRadius = 12.0

nodeBorderRadius :: Number
nodeBorderRadius = 28.0

haloRadius :: Number
haloRadius = 40.0

nodeTextBoxOffset :: Point2D
nodeTextBoxOffset = { x : 20.0, y : - 10.0 }

amplifierBoxSize :: Number
amplifierBoxSize = 15.0

amplifierTextBoxOffset :: Point2D
amplifierTextBoxOffset = { x : - 0.0 - amplifierBoxSize
                         , y : - 23.5 - amplifierBoxSize
                         }

amplifierHaloOffset :: Number
amplifierHaloOffset = 17.5

amplifierGainToControl :: Number -> Number
amplifierGainToControl gain = Math.sqrt gain

controlToAmplifierGain :: Number -> Number
controlToAmplifierGain controlPos = Math.pow controlPos 2.0

delayPeriodToGraphSpace :: Number -> Number
delayPeriodToGraphSpace = (*) 1000.0

pageSpaceToDelayPeriod :: Number -> Number
pageSpaceToDelayPeriod pageSpacePoint2D = pageSpacePoint2D / 1000.0

delayRectHeight :: Number
delayRectHeight = 20.0

delayRectHaloOffset :: Number
delayRectHaloOffset = 19.0

delayTextBoxOffset :: Point2D
delayTextBoxOffset = { x : 0.0
                     , y : -23.5  - delayRectHeight / 2.0
                     }

edgeTextBoxOffset :: Point2D
edgeTextBoxOffset = { x : 10.0, y : - 20.0 }

zoomScaling :: Number
zoomScaling = 0.01

defaultTextFieldShape :: Shape
defaultTextFieldShape = { width : 100.0, height : 50.0 }

maxTextFieldShape :: Shape
maxTextFieldShape = { width : 700.0, height : 500.0 }

filterShape :: Shape
filterShape = { height : 100.0
              , width : 600.0
              }

filterTextBoxOffset :: Point2D
filterTextBoxOffset = { x : 0.0
                      , y : -23.5
                      }

paneDividerWidth :: Number -- px
paneDividerWidth = 20.0
