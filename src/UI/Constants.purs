module UI.Constants where

import AppState (Shape)
import Core (Point2D)
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

edgeTextBoxOffset :: Point2D
edgeTextBoxOffset = { x : 10.0, y : - 20.0 }

zoomScaling :: Number
zoomScaling = 0.01

defaultTextFieldShape :: Shape
defaultTextFieldShape = { width : 100.0, height : 50.0 }

maxTextFieldShape :: Shape
maxTextFieldShape = { width : 700.0, height : 500.0 }

paneDividerWidth :: Number -- px
paneDividerWidth = 20.0

defaultTitleShape :: Shape
defaultTitleShape = { width : 200.0, height : 100.0 }

maxTitleShape :: Shape
maxTitleShape = { width : 1500.0, height: 500.0 }

invalidIndicatorOffset :: Point2D
invalidIndicatorOffset = { x : 2.0, y : 2.0 }

invalidIndicatorSize :: Number
invalidIndicatorSize = 10.0
