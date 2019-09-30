module UI.SvgDefs where

import Prelude

import Halogen as H
import Svg.Attributes as SA
import Svg.Elements as SE
import Svg.Types as SVGT
import UI.Constants (amplifierBoxSize, delayRectHeight)


svgDefs :: forall action slots m. H.ComponentHTML action slots m
svgDefs = SE.defs []
          [ SE.marker
            [ SA.id "drawing-arrow"
            , SA.markerWidth $ show 10.0
            , SA.markerHeight $ show 10.0
            , SA.refX $ show 3.0
            , SA.refY $ show 5.0
            , SA.orient SVGT.AutoOrient
            , SA.markerUnits SVGT.UserSpaceOnUse
            ]
            [ SE.path [ SA.d [ SVGT.Abs (SVGT.M 0.0 0.0)
                             , SVGT.Abs (SVGT.L 10.0 5.0)
                             , SVGT.Abs (SVGT.L 0.0 10.0)
                             , SVGT.Abs SVGT.Z
                             ]
                      ]
            ]
          , SE.marker
            [ SA.id "arrow"
            , SA.markerWidth $ show 10.0
            , SA.markerHeight $ show 10.0
            , SA.refX $ show 15.0
            , SA.refY $ show 5.0
            , SA.orient SVGT.AutoOrient
            , SA.markerUnits SVGT.UserSpaceOnUse
            ]
            [ SE.path [ SA.d [ SVGT.Abs (SVGT.M 0.0 0.0)
                             , SVGT.Abs (SVGT.L 0.0 10.0)
                             , SVGT.Abs (SVGT.L 8.0 5.0)
                             , SVGT.Abs SVGT.Z
                             ]
                      ]
            ]
          , SE.marker
            [ SA.id "arrow-to-synth-node"
            , SA.markerWidth $ show 10.0
            , SA.markerHeight $ show 10.0
            , SA.refX $ show 57.0
            , SA.refY $ show 5.0
            , SA.orient SVGT.AutoOrient
            , SA.markerUnits SVGT.UserSpaceOnUse
            ]
            [ SE.path [ SA.d [ SVGT.Abs (SVGT.M 0.0 0.0)
                             , SVGT.Abs (SVGT.L 0.0 10.0)
                             , SVGT.Abs (SVGT.L 8.0 5.0)
                             , SVGT.Abs SVGT.Z
                             ]
                      ]
            ]
          , SE.marker
            [ SA.id "arrow-to-group"
            , SA.markerWidth $ show 10.0
            , SA.markerHeight $ show 10.0
            , SA.refX $ show 19.0
            , SA.refY $ show 5.0
            , SA.orient SVGT.AutoOrient
            , SA.markerUnits SVGT.UserSpaceOnUse
            ]
            [ SE.path [ SA.d [ SVGT.Abs (SVGT.M 0.0 0.0)
                             , SVGT.Abs (SVGT.L 0.0 10.0)
                             , SVGT.Abs (SVGT.L 8.0 5.0)
                             , SVGT.Abs SVGT.Z
                             ]
                      ]
            ]
          , SE.pattern
            [ SA.id "pattern-delay-fill"
            , SA.height $ show 20.0
            , SA.width $ show 20.0
            , SA.x $ show $ - delayRectHeight / 2.0
            , SA.y $ show $ - delayRectHeight / 2.0
            , SA.patternUnits SVGT.PatternUnitsUserSpaceOnUse
            ]
            let
              patternRect x y cssClass =
                SE.rect
                [ SA.class_ cssClass
                , SA.height $ show 10.0
                , SA.width $ show 10.0
                , SA.x $ show x
                , SA.y $ show y
                ]
            in
              [ patternRect 0.0 0.0   "pattern-delay-fill-rect1"
              , patternRect 10.0 0.0  "pattern-delay-fill-rect2"
              , patternRect 10.0 10.0 "pattern-delay-fill-rect1"
              , patternRect 0.0 10.0  "pattern-delay-fill-rect2"
              ]
          , SE.pattern
            [ SA.id "pattern-amplifier-below-unity"
            , SA.height $ show 10.0
            , SA.width $ show 10.0
            , SA.x $ show $ - amplifierBoxSize / 3.0
            , SA.y $ show $ - amplifierBoxSize / 3.0
            , SA.patternUnits SVGT.PatternUnitsUserSpaceOnUse
            ]
            [ SE.rect
              [ SA.height $ show 10.0
              , SA.width $ show 10.0
              , SA.class_ "pattern-amplifier-unity-background"
              ]
            , SE.path [ SA.class_ "amplifier-below-unity-triangle"
                      , SA.d [ SVGT.Abs (SVGT.M 0.0 0.0)
                             , SVGT.Abs (SVGT.L 5.0 10.0)
                             , SVGT.Abs (SVGT.L 10.0 0.0)
                             , SVGT.Abs SVGT.Z
                             ]
                      ]
            ]
          , SE.pattern
            [ SA.id "pattern-amplifier-above-unity"
            , SA.height $ show 10.0
            , SA.width $ show 10.0
            , SA.x $ show $ - amplifierBoxSize / 3.0
            , SA.y $ show $ - amplifierBoxSize / 3.0
            , SA.patternUnits SVGT.PatternUnitsUserSpaceOnUse
            ]
            [ SE.rect
              [ SA.height $ show 10.0
              , SA.width $ show 10.0
              , SA.class_ "pattern-amplifier-unity-background"
              ]
            , SE.path [ SA.class_ "amplifier-above-unity-triangle"
                      , SA.d [ SVGT.Abs (SVGT.M 0.0 10.0)
                             , SVGT.Abs (SVGT.L 5.0 0.0)
                             , SVGT.Abs (SVGT.L 10.0 10.0)
                             , SVGT.Abs SVGT.Z
                             ]
                      ]
            ]
          ]
