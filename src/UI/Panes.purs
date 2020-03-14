module UI.Panes where

import Prelude

import AppState (AppState, _focusedPane, _megagraph, _windowBoundingRect)
import Data.Array as Array
import Data.Int (toNumber)
import Data.Lens ((.~), (%~), (?~))
import Data.Lens.At (at)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Math as Math
import Megagraph (GraphId, GraphView, PageSpacePoint2D(..), _boundingRect, _graphs, _origin, _panes, _zoom, emptyGraph, freshPane)
import MegagraphOperation (MegagraphComponent(..))
import UI.Constants (paneDividerWidth)
import Web.HTML.HTMLElement as WHE


insertBlankPane :: GraphId -> AppState -> AppState
insertBlankPane graphId state =
  let
    newPaneWidth = (state.windowBoundingRect.width / (toNumber (1 + (Map.size state.megagraph.graphs))))
                   - paneDividerWidth
    newPaneRect =
      state.windowBoundingRect
        { width = newPaneWidth
        , left  = state.windowBoundingRect.right - newPaneWidth
        }
    newPane = freshPane graphId newPaneRect
    squishedOldPanesWidth = state.windowBoundingRect.width - newPaneWidth - paneDividerWidth
  in
    state
    -- Squish existing panes by shrinking window
    # rescaleWindow (state.windowBoundingRect { width = squishedOldPanesWidth
                                              , right = squishedOldPanesWidth
                                              })
    -- Reset window to full size leaving a fresh gap
    # _windowBoundingRect .~ state.windowBoundingRect
    -- Insert new pane in empty space
    # _megagraph <<< _graphs <<< at graphId ?~ emptyGraph graphId
    # _megagraph <<< _panes <<< at graphId ?~ newPane
    # _focusedPane ?~ GraphComponent graphId
    # arrangePanes

arrangePanes :: AppState -> AppState
arrangePanes state =
  state # _megagraph <<< _panes %~ mapMapWithIndex updatePane
    where
      nPanes = toNumber $ Map.size state.megagraph.graphs
      paneWidth = (state.windowBoundingRect.width - paneDividerWidth * (nPanes - 1.0)) / nPanes
      updatePane :: Int -> GraphView -> GraphView
      updatePane index =
        _boundingRect %~ _{ width = paneWidth
                          , left  = (paneWidth + paneDividerWidth) * (toNumber index)
                          , right = (paneWidth + paneDividerWidth) * (toNumber index) + paneWidth
                          }
      mapMapWithIndex f =
        Map.toUnfoldable
        >>> Array.mapWithIndex (\index (Tuple key val) -> Tuple key (f index val))
        >>> Map.fromFoldable

rescaleWindow :: WHE.DOMRect -> AppState -> AppState
rescaleWindow newWindowBoundingRect appState =
  let
    horizontalScale = newWindowBoundingRect.width  / appState.windowBoundingRect.width
    verticalScale   = newWindowBoundingRect.height / appState.windowBoundingRect.height
    scaleRect rect =
      { width  : rect.width  * horizontalScale
      , left   : rect.left   * horizontalScale
      , right  : rect.right  * horizontalScale
      , height : rect.height * verticalScale
      , top    : rect.top    * verticalScale
      , bottom : rect.bottom * verticalScale
      }
    maxScale = Math.max horizontalScale verticalScale
  in
    appState
    # _windowBoundingRect .~ newWindowBoundingRect
    # _megagraph <<< _panes %~ map (_boundingRect %~ scaleRect)

-- | Zoom in/out holding the focus point invariant in page space and graph space
-- |
-- | The mapping from page space to graph space is given by:
-- |     graphSpacePoint2D = (pageSpacePoint2D - paneTopLeft - origin) * zoom
-- | where a larger value for zoom means that more of graph space is visible.
-- |
-- | To find the new graph origin that holds the mouse position
-- | in graph space constant, let:
-- |     p = focusPointPageSpace
-- |     prevFocusPointGraphSpace = (p - paneTopLeft - oldGraphOrigin) * oldZoom     (1)
-- |      newFocusPointGraphSpace = (p - paneTopLeft - newGraphOrigin) * newZoom     (2)
-- | and let:
-- |      newFocusPointGraphSpace = prevFocusPointGraphSpace           (3)
-- | by substituting (1) and (2) into (3) and rearranging:
-- |     newGraphOrigin = p - paneTopLeft - ((p - paneTopLeft - oldGraphOrigin) * (oldZoom / newZoom))
-- |
-- | If newZoom --> inf, newGraphOrigin --> focusPoint, as expected.
zoomAtPoint :: Number -> PageSpacePoint2D -> GraphView -> GraphView
zoomAtPoint newZoom (PageSpacePoint2D zoomFocus) pane =
  let
    PageSpacePoint2D origin = pane.origin
    newGraphOrigin =
      PageSpacePoint2D
      { x : zoomFocus.x - pane.boundingRect.left - ((zoomFocus.x - pane.boundingRect.left - origin.x) * (pane.zoom / newZoom))
      , y : zoomFocus.y - pane.boundingRect.top  - ((zoomFocus.y - pane.boundingRect.top  - origin.y) * (pane.zoom / newZoom))
      }
  in
    pane
    # _origin .~ newGraphOrigin
    # _zoom .~ newZoom

paneContainingPoint :: Array GraphView -> PageSpacePoint2D -> Maybe GraphView
paneContainingPoint panes pagePosition =
  let
    rectsContainingPoint =
      Array.filter
        (\pane -> withinRect pagePosition pane.boundingRect)
        panes
  in
    Array.head rectsContainingPoint

withinRect :: PageSpacePoint2D -> WHE.DOMRect -> Boolean
withinRect (PageSpacePoint2D point) rect =
  point.x < rect.right
  &&
  point.x > rect.left
  &&
  point.y > rect.top
  &&
  point.y < rect.bottom
