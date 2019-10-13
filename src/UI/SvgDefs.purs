module UI.SvgDefs where

import Prelude

import Halogen as H
import Svg.Attributes as SA
import Svg.Elements as SE
import Svg.Types as SVGT


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
          ]
