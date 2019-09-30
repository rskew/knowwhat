module UI.Panes where

import Prelude

import AppOperation (AppOperation(..))
import AppOperation.UIOp (moveGraphOrigin, updateZoom)
import AppState (AppState, _graphData, _windowBoundingRect)
import Core (GraphId, PageSpacePoint2D(..), GraphView, emptyPane, _panes, _boundingRect)
import Data.Array as Array
import Data.Int (toNumber)
import Data.Lens ((?~), (.~), traversed, (%~))
import Data.Lens.At (at)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Math as Math
import UI.Constants (paneDividerWidth)
import Web.HTML.HTMLElement as WHE

insertPaneImpl :: GraphId -> AppState -> AppState
insertPaneImpl graphId appState =
  let
    newPaneWidth = (appState.windowBoundingRect.width / (toNumber $ 1 + (Map.size appState.graphData.panes)))
                   - paneDividerWidth
    newPaneRect  = appState.windowBoundingRect { width = newPaneWidth
                                               , left  = appState.windowBoundingRect.right - newPaneWidth
                                               }
    newPane = (emptyPane graphId) { boundingRect = newPaneRect }
    squishedOldPanesWidth = appState.windowBoundingRect.width - newPaneWidth - paneDividerWidth
  in
    appState
    # rescaleWindowImpl (appState.windowBoundingRect { width = squishedOldPanesWidth
                                                     , right = squishedOldPanesWidth
                                                     })
    # _windowBoundingRect .~ appState.windowBoundingRect
    # _graphData <<< _panes <<< at graphId ?~ newPane
    # arrangePanes

removePaneImpl :: GraphId -> AppState -> AppState
removePaneImpl graphId appState =
  -- TODO: implement remove graph data when monadic ops are removed
  arrangePanes appState

arrangePanes :: AppState -> AppState
arrangePanes appState =
  let
    orderedPanes = appState.graphData.panes
                   # Map.values >>> Array.fromFoldable
                   # Array.sortBy (comparing _.boundingRect.left)
    nPanes = toNumber $ Array.length orderedPanes
    paneWidth = (appState.windowBoundingRect.width - paneDividerWidth * (nPanes - 1.0)) / nPanes
    newPanes = orderedPanes # Array.mapWithIndex \index pane ->
      Tuple pane.graphId $
        pane { boundingRect =
                  pane.boundingRect { width =  paneWidth
                                    , left  = (paneWidth + paneDividerWidth) * (toNumber index)
                                    , right = (paneWidth + paneDividerWidth) * (toNumber index) + paneWidth
                                    }
             }
  in
    appState # _graphData <<< _panes .~ Map.fromFoldable newPanes

rescalePaneImpl :: GraphId -> WHE.DOMRect -> AppState -> AppState
rescalePaneImpl graphId rect =
  _graphData <<< _panes <<< at graphId <<< traversed <<< _boundingRect .~ rect

rescaleWindowImpl :: WHE.DOMRect -> AppState -> AppState
rescaleWindowImpl newWindowBoundingRect appState =
  let
    horizontalScale = newWindowBoundingRect.width  / appState.windowBoundingRect.width
    verticalScale   = newWindowBoundingRect.height / appState.windowBoundingRect.height
    shiftRect rect =
      { width  : rect.width  * horizontalScale
      , left   : rect.left   * horizontalScale
      , right  : rect.right  * horizontalScale
      , height : rect.height * verticalScale
      , top    : rect.top    * verticalScale
      , bottom : rect.bottom * verticalScale
      }
    maxScale = Math.max horizontalScale verticalScale
    newPanes = appState.graphData.panes <#> (_boundingRect %~ shiftRect)
  in
    appState
    # _windowBoundingRect .~ newWindowBoundingRect
    # _graphData <<< _panes .~ newPanes

-- | Zoom in/out holding the focus point invariant in page space and graph space
-- |
-- | The mapping from page space to graph space is given by:
-- |     graphSpacePoint2D = (pageSpacePoint2D - origin) * zoom
-- | where a larger value for zoom means that more of graph space is visible.
-- |
-- | To find the new graph origin that holds the mouse position
-- | in graph space constant, let:
-- |     p = focusPointPageSpace
-- |     prevFocusPointGraphSpace = (p - oldGraphOrigin) * oldZoom     (1)
-- |      newFocusPointGraphSpace = (p - newGraphOrigin) * newZoom     (2)
-- | and let:
-- |      newFocusPointGraphSpace = prevFocusPointGraphSpace           (3)
-- | by substituting (1) and (2) into (3) and rearranging:
-- |     newGraphOrigin = p - ((p - oldGraphOrigin) * (oldZoom / newZoom))
-- |
-- | If newZoom --> inf, newGraphOrigin --> focusPoint, as expected.
zoomAtPoint :: Number -> PageSpacePoint2D -> GraphView -> AppOperation Unit
zoomAtPoint newZoom pageSpacePoint pane =
  let
    PageSpacePoint2D point = pageSpacePoint
    PageSpacePoint2D origin = pane.origin
    newGraphOrigin = PageSpacePoint2D
                       { x : point.x - ((point.x - origin.x) * (pane.zoom / newZoom))
                       , y : point.y - ((point.y - origin.y) * (pane.zoom / newZoom))
                       }
  in AppOperation do
    moveGraphOrigin pane.graphId newGraphOrigin
    updateZoom pane.graphId newZoom

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
