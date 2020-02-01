module GraphComponent.Render where

import Prelude

import AppState (AppState, DrawingEdge, HoveredElementId(..), _drawingEdges, _graphAtId, _paneAtId)
import CSS as CSS
import ContentEditable.SVGComponent as SVGContentEditable
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Array as Array
import Data.Lens ((^.), (^?), traversed)
import Data.Lens.At (at)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import GraphComponent.Types (Action(..), Slots, _edgeTextField, _nodeTextField, _titleTextField)
import GraphComponent.Utils (bezierControlPointFromParabolaPoints, drawingEdgeWithinNodeHalo, edgeMidPosition, edgeSourcePosition, edgeTargetPosition)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Elements.Keyed as HK
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Megagraph (Edge, EdgeMappingEdge, EdgeSpacePoint2D(..), Focus(..), Graph, GraphId, GraphSpacePoint2D(..), GraphView, Mapping, Node, NodeMappingEdge, PageSpacePoint2D(..), _nodes, edgeArray, edgeSpaceToGraphSpace, graphSpaceToPageSpace, lookupEdgeById, pageSpaceToGraphSpace)
import Svg.Attributes as SA
import Svg.Elements as SE
import Svg.Elements.Keyed as SK
import Svg.Types as SVGT
import UI.Constants (defaultTextFieldShape, defaultTitleShape, edgeTextBoxOffset, groupNodeRadius, haloRadius, invalidIndicatorOffset, invalidIndicatorSize, maxTextFieldShape, maxTitleShape, nodeBorderRadius, nodeRadius, nodeTextBoxOffset)
import UI.SvgDefs (svgDefs)
import Web.UIEvent.MouseEvent as ME


renderGraphNode :: AppState -> GraphView -> Node -> Tuple String (H.ComponentHTML Action Slots Aff)
renderGraphNode state pane node =
  let
    nodeFocused = pane.focus == Just (FocusNode node.id)
    hoveredOverBorder = state.hoveredElementId == Just (NodeBorderId node.id)
    hoveredOverHalo   = state.hoveredElementId == Just (NodeHaloId node.id)
    noDrawingEdgeFromNode =
      Map.isEmpty (Map.filter (\drawingEdge -> drawingEdge.source == node.id)
                              (state ^. _drawingEdges))
    existsDrawingEdgeHoveredOverNode =
      (0 < Map.size (Map.filter (\drawingEdge -> drawingEdgeWithinNodeHalo drawingEdge pane node) state.drawingEdges))
    drawingEdgeOverNode =
      (hoveredOverBorder || hoveredOverHalo)
      &&
      noDrawingEdgeFromNode
      &&
      existsDrawingEdgeHoveredOverNode
    nodeClasses =
      joinWith " " $ Array.catMaybes
      [ Just "node"
      , if nodeFocused
        then Just "focused"
        else Nothing
      ]
    nodeBorderClasses =
        joinWith " " $ Array.catMaybes
        [ Just "nodeBorder"
        -- TODO: use highlighting
        --, if (Set.member node.id state.graphData.highlighted)
        --  then Just "highlighted"
        --  else Nothing
        , if hoveredOverBorder
             &&
             (not drawingEdgeOverNode)
             &&
             (not nodeFocused)
          then Just "hovered"
          else Nothing
        ]
    haloClasses =
      joinWith " " $ Array.catMaybes
      [ Just "nodeHalo"
      , if nodeFocused
        then Just "focused"
        else Nothing
      , if drawingEdgeOverNode
        then Just "ready"
        else Nothing
      ]
    textBoxHTML textBoxOffset =
      [ SE.g
        [ SA.transform [ SVGT.Translate textBoxOffset.x textBoxOffset.y ]
        , HE.onMouseDown \e -> Just
                               $ StopPropagation (ME.toEvent e)
                               $ DoNothing
        ]
        [ HH.slot
          _nodeTextField
          node.id
          SVGContentEditable.svgContenteditable
          { shape : defaultTextFieldShape
          , initialText : node.text
          , maxShape : maxTextFieldShape
          , fitContentDynamic : true
          }
          (Just <<< NodeTextInput node.graphId node.id)
        ]
      ]
    graphNodeHTML =
      -- Node Halo, for creating edges from
      [ SE.circle
        [ SA.class_ haloClasses
        , SA.r $ show haloRadius
        , HE.onMouseDown \e -> Just $ StopPropagation (ME.toEvent e)
                               $ EdgeDrawStart pane node.id e
        , HE.onMouseEnter \_ -> Just $ Hover $ Just $ NodeHaloId node.id
        , HE.onMouseLeave \_ -> Just $ Hover Nothing
        ]

      , SE.circle
        [ SA.class_ nodeClasses
        , SA.r $ show $ if isJust node.subgraph
                        then groupNodeRadius
                        else nodeRadius
        ]
      -- Node border, for grabbing
      , SE.circle
        [ SA.class_ nodeBorderClasses
        , SA.r $ show nodeBorderRadius
        , HE.onMouseDown \e -> Just
                               $ StopPropagation (ME.toEvent e)
                               $ NodeDragStart node.graphId node.id node.position e
        , HE.onDoubleClick \e -> Just $ StopPropagation (ME.toEvent e)
                                 $ AppDeleteNode node
        , HE.onMouseEnter \_ -> Just $ Hover $ Just $ NodeBorderId node.id
        , HE.onMouseLeave \_ -> Just $ Hover Nothing
        ]
      ] <> textBoxHTML nodeTextBoxOffset
    GraphSpacePoint2D nodePos = node.position
    shiftedNodeHTML = SE.g
               [ SA.transform [ SVGT.Translate nodePos.x nodePos.y ] ]
               graphNodeHTML
  in
    Tuple (show node.id) shiftedNodeHTML

renderGhostNode :: AppState -> GraphView -> Node -> Maybe (Tuple String (H.ComponentHTML Action Slots Aff))
renderGhostNode state renderPane node =
  case state ^? _paneAtId node.graphId of
    Nothing -> Nothing
    Just nativePane -> Just
      let
        graphNodeHTML =
          SE.circle
          [ SA.class_ "node ghost"
          , SA.r $ show $ if isJust node.subgraph
                          then groupNodeRadius
                          else nodeRadius
          ]
        GraphSpacePoint2D nodePos = node.position # graphSpaceToPageSpace nativePane # pageSpaceToGraphSpace renderPane
        nodeHTML = SE.g
                   [ SA.transform [ SVGT.Translate nodePos.x nodePos.y ] ]
                   [ graphNodeHTML ]
      in
        Tuple (show node.id <> "_ghost") nodeHTML

renderNodeMappingEdge :: AppState -> GraphView -> Mapping -> NodeMappingEdge -> Maybe (Tuple String (H.ComponentHTML Action Slots Aff))
renderNodeMappingEdge state renderPane mapping nodeMappingEdge = do
  sourcePos <- edgeSourcePosition state mapping.sourceGraph nodeMappingEdge.sourceNode renderPane
  targetPos <- edgeTargetPosition state mapping.targetGraph nodeMappingEdge.targetNode renderPane
  let
    focused = renderPane.focus == (Just $ FocusEdge nodeMappingEdge.id [])
    edgeClasses = ["nodeMappingEdge"] <> if focused then ["focused"] else []
    edgeKey = show nodeMappingEdge.id <> "_edge_" <> show renderPane.graphId
    targetHasSubgraph = isJust do
      graphState <- Map.lookup mapping.targetGraph state.megagraph.graphs
      targetNode <- Map.lookup nodeMappingEdge.targetNode graphState.graph.nodes
      targetNode.subgraph
    zeroMidpoint = EdgeSpacePoint2D {angle: 0.0, radius: 0.0}
    svg = edgeSvg sourcePos targetPos zeroMidpoint edgeClasses targetHasSubgraph
  pure $ Tuple edgeKey svg

renderEdgeMappingEdge :: AppState -> GraphView -> Mapping -> EdgeMappingEdge -> Maybe (Tuple String (H.ComponentHTML Action Slots Aff))
renderEdgeMappingEdge state renderPane mapping edgeMappingEdge = do
  sourceGraph <- Map.lookup mapping.sourceGraph state.megagraph.graphs <#> _.graph
  sourceEdge <- lookupEdgeById edgeMappingEdge.sourceEdge sourceGraph
  targetGraph <- Map.lookup mapping.targetGraph state.megagraph.graphs <#> _.graph
  targetEdge <- lookupEdgeById edgeMappingEdge.targetEdge targetGraph
  sourcePos <- edgeMidPosition state mapping.sourceGraph sourceEdge.source sourceEdge.target renderPane
  targetPos <- edgeMidPosition state mapping.sourceGraph targetEdge.source targetEdge.target renderPane
  let
    focused = renderPane.focus == (Just $ FocusEdge edgeMappingEdge.id [])
    edgeClasses = ["edgeMappingEdge"] <> if focused then ["focused"] else []
    edgeKey = show edgeMappingEdge.id <> "_edge_" <> show renderPane.graphId
    targetHasSubgraph = false
    zeroMidpoint = EdgeSpacePoint2D {angle: 0.0, radius: 0.0}
    svg = edgeSvg sourcePos targetPos zeroMidpoint edgeClasses targetHasSubgraph
  pure $ Tuple edgeKey svg

renderEdge :: AppState -> GraphView -> Edge -> Maybe (Tuple String (H.ComponentHTML Action Slots Aff))
renderEdge state renderPane edge = do
  sourcePos <- edgeSourcePosition state edge.graphId edge.source renderPane
  targetPos <- edgeTargetPosition state edge.graphId edge.target renderPane
  let
    focused = renderPane.focus == (Just $ FocusEdge edge.id [])
    edgeClasses = if focused then ["focused"] else []
    edgeKey = show edge.id <> "_edge_" <> show renderPane.graphId
    targetHasSubgraph = isJust do
      graphState <- Map.lookup edge.graphId state.megagraph.graphs
      targetNode <- Map.lookup edge.target graphState.graph.nodes
      targetNode.subgraph
    svg = edgeSvg sourcePos targetPos edge.midpoint edgeClasses targetHasSubgraph
  pure $ Tuple edgeKey svg

edgeSvg :: GraphSpacePoint2D -> GraphSpacePoint2D -> EdgeSpacePoint2D -> Array String -> Boolean -> H.ComponentHTML Action Slots Aff
edgeSvg (GraphSpacePoint2D sourcePos) (GraphSpacePoint2D targetPos) midpoint classes targetHasSubgraph =
  let
    edgeClasses =
      joinWith " " $ ["edge"] <> classes
    markerRef = if targetHasSubgraph
                then "url(#arrow-to-group)"
                else "url(#arrow)"
    GraphSpacePoint2D midpointGraphSpace =
      edgeSpaceToGraphSpace
        (GraphSpacePoint2D sourcePos)
        (GraphSpacePoint2D targetPos)
        midpoint
    bezierControlPoint = bezierControlPointFromParabolaPoints sourcePos midpointGraphSpace targetPos
  in
    SE.path
    [ SA.class_ edgeClasses
    , SA.d [ SVGT.Abs (SVGT.M sourcePos.x sourcePos.y)
           , SVGT.Abs (SVGT.Q bezierControlPoint.x bezierControlPoint.y targetPos.x targetPos.y)
           ]
    , SA.markerEnd markerRef
    ]

renderEdgeTextField :: AppState -> GraphView -> Edge -> Maybe (Tuple String (H.ComponentHTML Action Slots Aff))
renderEdgeTextField state renderPane edge = do
  targetNode <- state ^? _graphAtId edge.graphId <<< _nodes <<< at edge.target <<< traversed
  sourceNode <- state ^? _graphAtId edge.graphId <<< _nodes <<< at edge.source <<< traversed
  let
    GraphSpacePoint2D midpoint =
      edgeSpaceToGraphSpace
        sourceNode.position
        targetNode.position
        edge.midpoint
    focused = renderPane.focus == (Just $ FocusEdge edge.id [])
  if not focused && (edge.text == "")
  then Nothing
  else Just $ Tuple (show edge.id <> "_textField_" <> show renderPane.graphId) $
    SE.g
    [ SA.transform [ SVGT.Translate
                     (midpoint.x + edgeTextBoxOffset.x)
                     (midpoint.y + edgeTextBoxOffset.y)
                   ]
    , HE.onMouseDown \e -> Just
                           $ StopPropagation (ME.toEvent e)
                           $ DoNothing
    ]
    [ HH.slot
      _edgeTextField
      edge.id
      SVGContentEditable.svgContenteditable
      { shape : defaultTextFieldShape
      , initialText : edge.text
      , maxShape : maxTextFieldShape
      , fitContentDynamic : true
      }
      (Just <<< EdgeTextInput sourceNode.graphId edge.id)
    ]

renderEdgeBorder :: AppState -> GraphView -> Edge -> Maybe (Tuple String (H.ComponentHTML Action Slots Aff))
renderEdgeBorder state renderPane edge = do
  targetNode <- state ^? _graphAtId edge.graphId <<< _nodes <<< at edge.target
  GraphSpacePoint2D sourcePos <- edgeSourcePosition state edge.graphId edge.source renderPane
  GraphSpacePoint2D targetPos <- edgeTargetPosition state edge.graphId edge.target renderPane
  let
    edgeBorderClasses =
      joinWith " " $ Array.catMaybes
      [ Just "edgeBorder"
      , if state.hoveredElementId == (Just $ EdgeBorderId edge.id)
        then Just "hover"
        else Nothing
      ]
    GraphSpacePoint2D midpointGraphSpace =
      edgeSpaceToGraphSpace
        (GraphSpacePoint2D sourcePos)
        (GraphSpacePoint2D targetPos)
        edge.midpoint
    bezierControlPoint = bezierControlPointFromParabolaPoints sourcePos midpointGraphSpace targetPos
  pure $ Tuple (show edge.id <> "_border_" <> show renderPane.graphId) $
    SE.path
    [ SA.class_ edgeBorderClasses
    , SA.d [ SVGT.Abs (SVGT.M sourcePos.x sourcePos.y)
           , SVGT.Abs (SVGT.Q bezierControlPoint.x bezierControlPoint.y targetPos.x targetPos.y)
           ]
    , HE.onMouseDown \e -> Just
                           $ StopPropagation (ME.toEvent e)
                           $ EdgeDragStart edge.graphId edge.id edge.midpoint e
    , HE.onMouseEnter \_ -> Just $ Hover $ Just $ EdgeBorderId edge.id
    , HE.onMouseLeave \_ -> Just $ Hover Nothing
    , HE.onDoubleClick \e -> Just $ StopPropagation (ME.toEvent e)
                                  $ AppDeleteEdge renderPane.graphId edge
    ]

renderDrawingEdge :: AppState -> GraphView -> DrawingEdge -> Maybe (Tuple String (H.ComponentHTML Action Slots Aff))
renderDrawingEdge state renderPane drawingEdgeState = do
  let
    GraphSpacePoint2D sourceGraphPos =
      drawingEdgeState.sourcePosition
      # pageSpaceToGraphSpace renderPane
    GraphSpacePoint2D pointGraphPos =
      drawingEdgeState.pointPosition
      # pageSpaceToGraphSpace renderPane
    isAcrossGraphs = drawingEdgeState.sourceGraph /= drawingEdgeState.targetGraph
  pure $ Tuple (show drawingEdgeState.source <> "_drawingEdge_" <> show renderPane.graphId) $
    SE.line
    [ SA.class_ $ "edge" <> if isAcrossGraphs then " mappingEdge" else ""
    , SA.x1 $ show sourceGraphPos.x
    , SA.y1 $ show sourceGraphPos.y
    , SA.x2 $ show pointGraphPos.x
    , SA.y2 $ show pointGraphPos.y
    , SA.markerEnd "url(#drawing-arrow)"
    ]

renderTitle :: GraphId -> String -> Tuple String (H.ComponentHTML Action Slots Aff)
renderTitle graphId titleText =
  Tuple (show graphId <> "_title") $
    SE.g
    [ SA.class_ "title"
    , HE.onMouseDown \e -> Just
                           $ StopPropagation (ME.toEvent e)
                           $ DoNothing
    ]
    [ HH.slot
      _titleTextField
      graphId
      SVGContentEditable.svgContenteditable
      { shape : defaultTitleShape
      , initialText : titleText
      , maxShape : maxTitleShape
      , fitContentDynamic : true
      }
      (Just <<< TitleTextInput graphId)
    ]

renderTitleInvalidIndicator :: GraphId -> Tuple String (H.ComponentHTML Action Slots Aff)
renderTitleInvalidIndicator graphId =
  Tuple (show graphId <> "_titleInvalidIndicator") $
    SE.rect
    [ SA.class_ "invalidIndicator"
    , SA.width  $ show invalidIndicatorSize
    , SA.height $ show invalidIndicatorSize
    , SA.x $ show invalidIndicatorOffset.x
    , SA.y $ show invalidIndicatorOffset.y
    ]

renderSingleGraph :: AppState -> GraphView -> Graph -> H.ComponentHTML Action Slots Aff
renderSingleGraph state renderPane graph =
  let
    nodes                 = graph.nodes # Map.values # Array.fromFoldable

    keyedNodes            = nodes <#> renderGraphNode state renderPane

    allNodesInOtherGraphs = state.megagraph.graphs
                            # Map.values >>> Array.fromFoldable
                            # Array.filter (\graphState -> graphState.graph.id /= graph.id)
                            # Array.concatMap (Array.fromFoldable <<< Map.values <<< _.graph.nodes)

    ghostNodes            = Array.catMaybes $ (allNodesInOtherGraphs <#> renderGhostNode state renderPane)

    allMappings           = state.megagraph.mappings
                            # Map.values >>> Array.fromFoldable
                            <#> _.mapping
                            # Array.filter (\mapping -> mapping.sourceGraph == graph.id
                                                     || mapping.targetGraph == graph.id)

    keyedNodeMappingEdges :: Array (Tuple String (H.ComponentHTML Action Slots Aff))
    keyedNodeMappingEdges = allMappings # Array.concatMap \mapping ->
                              mapping.nodeMappingEdges
                              # Array.fromFoldable
                              <#> renderNodeMappingEdge state renderPane mapping
                              # Array.catMaybes

    keyedEdgeMappingEdges = allMappings # Array.concatMap \mapping ->
                              mapping.edgeMappingEdges
                              # Array.fromFoldable
                              <#> renderEdgeMappingEdge state renderPane mapping
                              # Array.catMaybes

    edges                 = edgeArray graph

    keyedEdges            = Array.mapMaybe (renderEdge state renderPane) edges

    keyedEdgeTextFields   = Array.mapMaybe (renderEdgeTextField state renderPane) edges

    keyedEdgeBorders      = Array.mapMaybe (renderEdgeBorder state renderPane) edges

    drawingEdges          = state.drawingEdges # Map.values # Array.fromFoldable

    keyedDrawingEdges     = Array.mapMaybe (renderDrawingEdge state renderPane) drawingEdges

    keyedTitle = [ renderTitle renderPane.graphId graph.title.text ]

    keyedTitleInvalidIndicator = if graph.title.isValid
                                 then []
                                 else [ renderTitleInvalidIndicator renderPane.graphId ]

    zoom                    = renderPane.zoom
    PageSpacePoint2D origin = renderPane.origin
    boundingRect            = renderPane.boundingRect
    focused                 = state.focusedPane == Just renderPane.graphId
  in
    HH.div
    [ HCSS.style do
        CSS.left    $ CSS.px boundingRect.left
        CSS.top     $ CSS.px boundingRect.top
        CSS.width   $ CSS.px boundingRect.width
        CSS.height  $ CSS.px boundingRect.height
    , HP.classes $ [ HH.ClassName "pane" ] <> if focused then [ HH.ClassName "focused" ] else []
    ]
    [ SE.svg
      [ SA.viewBox
        -- scale then shift
        ( - origin.x * zoom )
        ( - origin.y * zoom )
        ( boundingRect.width  * zoom )
        ( boundingRect.height * zoom )
      , HE.onMouseDown \e -> Just
                             $ StopPropagation (ME.toEvent e)
                             $ BackgroundDragStart renderPane.graphId (PageSpacePoint2D origin) e
      , HE.onDoubleClick \e -> Just
                               $ StopPropagation (ME.toEvent e)
                               $ AppCreateNode renderPane e
      , HE.onWheel $ Just <<< Zoom renderPane.graphId
      ]
      [ svgDefs
      , SK.g
        []
        (ghostNodes
         <> keyedNodeMappingEdges
         <> keyedEdgeMappingEdges
         <> keyedEdges
         <> keyedEdgeBorders
         <> keyedDrawingEdges
         <> keyedNodes
         <> keyedEdgeTextFields
         <> keyedTitle
         <> keyedTitleInvalidIndicator
        )
      ]
    ]

render :: AppState -> H.ComponentHTML Action Slots Aff
render state =
  let
    renderedGraphs =
      state.megagraph.graphs
      # Map.toUnfoldable
      <#> (\(Tuple graphId graphState) ->
             Tuple (show graphId) $ renderSingleGraph state graphState.view graphState.graph)
  in
    HK.div
    [ HP.ref (H.RefLabel "panes")
    , HP.classes [ HH.ClassName "panes" ]
    ]
    (renderedGraphs
     <> [ Tuple "input" $
                HH.input
                [ HP.type_ InputFile
                , HE.onChange $ Just <<< FetchLocalFile
                ]
        ]
    )
