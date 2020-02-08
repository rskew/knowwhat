module UI.SvgDefs where

import Prelude

import Halogen as H
import Svg.Attributes as SA
import Svg.Elements as SE
import Svg.Types as SVGT
import UI.Constants (edgeMappingEdgeBeginMarkerRadius)


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
          , SE.marker
            [ SA.id "arrow-to-edge"
            , SA.markerWidth $ show 10.0
            , SA.markerHeight $ show 10.0
            , SA.refX $ show 8.0
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
            [ SA.id "edge-mapping-edge-begin"
            , SA.markerWidth $ show $ 2.0 * edgeMappingEdgeBeginMarkerRadius
            , SA.markerHeight $ show $ 2.0 * edgeMappingEdgeBeginMarkerRadius
            , SA.refX $ show edgeMappingEdgeBeginMarkerRadius
            , SA.refY $ show edgeMappingEdgeBeginMarkerRadius
            , SA.orient SVGT.AutoOrient
            , SA.markerUnits SVGT.UserSpaceOnUse
            ]
            [ SE.circle
              [ SA.r $ show edgeMappingEdgeBeginMarkerRadius
              , SA.cx $ show edgeMappingEdgeBeginMarkerRadius
              , SA.cy $ show edgeMappingEdgeBeginMarkerRadius
              ]
            ]
          ]
