module GraphComponent.Render where

import Prelude

import AppState (AppState, DrawingEdge, EdgeSourceElement(..), HoveredElementId(..), MegagraphElement(..), _drawingEdges, _graphAtId, _mappingFocus, _mappings, _megagraph, _paneAtId)
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
import GraphComponent.Utils (bezierControlPointFromParabolaPoints, drawingEdgeWithinNodeHalo, lookupNodePositionInPane, parallelParabola)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Elements.Keyed as HK
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Megagraph (Edge, EdgeMappingEdge, Focus(..), Graph, GraphId, GraphSpacePoint2D(..), GraphView, Mapping, Node, NodeMappingEdge, PageSpacePoint2D(..), _nodes, _position, _subgraph, edgeArray, graphEdgeSpaceToGraphSpace, graphSpaceToPageSpace, lookupEdgeById, pageEdgeSpaceToPageSpace, pageSpaceToGraphSpace)
import Svg.Attributes as SA
import Svg.Elements as SE
import Svg.Elements.Keyed as SK
import Svg.Types as SVGT
import UI.Constants (defaultTextFieldShape, defaultTitleShape, edgeHaloOffset, edgeTextBoxOffset, groupNodeRadius, haloRadius, invalidIndicatorOffset, invalidIndicatorSize, maxTextFieldShape, maxTitleShape, nodeBorderRadius, nodeRadius, nodeTextBoxOffset)
import UI.SvgDefs (svgDefs)
import Web.UIEvent.MouseEvent as ME


renderGraphNode :: AppState -> GraphView -> Node -> Tuple String (H.ComponentHTML Action Slots Aff)
renderGraphNode state pane node =
  let
    nodeFocused = pane.focus == Just (FocusNode node.id)
    hoveredOverBorder = state.hoveredElementId == Just (NodeBorderId pane.graphId node.id)
    hoveredOverHalo   = state.hoveredElementId == Just (NodeHaloId pane.graphId node.id)
    noDrawingEdgeFromNode =
      state ^. _drawingEdges
      # Map.filter (\drawingEdge -> drawingEdge.source == NodeSource node.id)
      # Map.isEmpty
    drawingEdgeOverNode =
      state.drawingEdges
      # Map.filter (\drawingEdge -> drawingEdgeWithinNodeHalo drawingEdge pane node
                                    && case drawingEdge.source of
                                         EdgeSource _ -> false
                                         NodeSource drawingEdgeSourceNode -> drawingEdgeSourceNode /= node.id)
      # Map.size
      # \n -> n > 0
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
      , if hoveredOverHalo
        then Just "hovered"
        else Nothing
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
                               $ EdgeDrawStart pane (NodeSource node.id) e
        , HE.onMouseEnter \_ -> Just $ Hover $ Just $ NodeHaloId node.graphId node.id
        , HE.onMouseLeave \_ -> Just $ Hover Nothing
        ]
      -- Node core
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
        , HE.onMouseEnter \_ -> Just $ Hover $ Just $ NodeBorderId node.graphId node.id
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
  GraphSpacePoint2D sourcePos <- lookupNodePositionInPane state mapping.sourceGraph nodeMappingEdge.sourceNode renderPane
  GraphSpacePoint2D targetPos <- lookupNodePositionInPane state mapping.targetGraph nodeMappingEdge.targetNode renderPane
  let
    focused = (state ^? _megagraph <<< _mappings <<< at mapping.id <<< traversed <<< _mappingFocus <<< traversed)
              == Just nodeMappingEdge.id
    edgeClasses = joinWith " " $ ["edge", "nodeMappingEdge"] <> if focused then ["focused"] else []
    edgeBorderClasses =
      joinWith " " $ Array.catMaybes
      [ Just "edgeBorder"
      , if state.hoveredElementId == (Just $ EdgeBorderId (MappingElement mapping.id mapping.sourceGraph mapping.targetGraph) nodeMappingEdge.id)
        then Just "hovered"
        else Nothing
      , if focused
        then Just "focused"
        else Nothing
      ]
    targetHasSubgraph =
      isJust $ state ^? _graphAtId mapping.targetGraph <<< _nodes <<< at nodeMappingEdge.targetNode <<< traversed <<< _subgraph <<< traversed
    markerRef = if targetHasSubgraph
                then "url(#arrow-to-group)"
                else "url(#arrow)"
    sourcePosPageSpace = graphSpaceToPageSpace renderPane (GraphSpacePoint2D sourcePos)
    targetPosPageSpace = graphSpaceToPageSpace renderPane (GraphSpacePoint2D targetPos)
    GraphSpacePoint2D midpointGraphSpace =
      nodeMappingEdge.midpoint
      # pageEdgeSpaceToPageSpace sourcePosPageSpace targetPosPageSpace
      # pageSpaceToGraphSpace renderPane
    bezierControlPoint = bezierControlPointFromParabolaPoints {p0: sourcePos, p1: midpointGraphSpace, p2: targetPos}
    edgeKey = show nodeMappingEdge.id <> "_edge_" <> show renderPane.graphId
  pure $ Tuple edgeKey $
    SE.g
    []
    [ SE.path
      [ SA.class_ edgeClasses
      , SA.d [ SVGT.Abs (SVGT.M sourcePos.x sourcePos.y)
             , SVGT.Abs (SVGT.Q bezierControlPoint.x bezierControlPoint.y targetPos.x targetPos.y)
             ]
      , SA.markerEnd markerRef
      ]
    , SE.path
      [ SA.class_ edgeBorderClasses
      , SA.d [ SVGT.Abs (SVGT.M sourcePos.x sourcePos.y)
             , SVGT.Abs (SVGT.Q bezierControlPoint.x bezierControlPoint.y targetPos.x targetPos.y)
             ]
      , HE.onMouseDown \e -> Just $ StopPropagation (ME.toEvent e)
                                  $ NodeMappingEdgeDragStart mapping.id nodeMappingEdge e
      , HE.onMouseEnter \_ -> Just $ Hover $ Just $ EdgeBorderId (MappingElement mapping.id mapping.sourceGraph mapping.targetGraph) nodeMappingEdge.id
      , HE.onMouseLeave \_ -> Just $ Hover Nothing
      , HE.onDoubleClick \e -> Just $ StopPropagation (ME.toEvent e)
                                    $ AppDeleteNodeMappingEdge mapping.id nodeMappingEdge.id
      ]
    ]

renderEdgeMappingEdge :: AppState -> GraphView -> Mapping -> EdgeMappingEdge -> Maybe (Tuple String (H.ComponentHTML Action Slots Aff))
renderEdgeMappingEdge state renderPane mapping edgeMappingEdge = do
  sourceGraph <- state ^? _graphAtId mapping.sourceGraph
  sourceEdge <- lookupEdgeById edgeMappingEdge.sourceEdge sourceGraph
  sourcePane <- state ^? _paneAtId mapping.sourceGraph
  sourceEdgeSourcePos <- sourceGraph ^? _position sourceEdge.source
  sourceEdgeTargetPos <- sourceGraph ^? _position sourceEdge.target
  targetGraph <- state ^? _graphAtId mapping.targetGraph
  targetEdge <- lookupEdgeById edgeMappingEdge.targetEdge targetGraph
  targetPane <- state ^? _paneAtId mapping.targetGraph
  targetEdgeSourcePos <- targetGraph ^? _position targetEdge.source
  targetEdgeTargetPos <- targetGraph ^? _position targetEdge.target
  let
    sourcePosGraphSpace = graphEdgeSpaceToGraphSpace sourceEdgeSourcePos sourceEdgeTargetPos sourceEdge.midpoint
    targetPosGraphSpace = graphEdgeSpaceToGraphSpace targetEdgeSourcePos targetEdgeTargetPos targetEdge.midpoint
    sourcePosPageSpace = graphSpaceToPageSpace sourcePane sourcePosGraphSpace
    targetPosPageSpace = graphSpaceToPageSpace targetPane targetPosGraphSpace
    focused = (state ^? _megagraph <<< _mappings <<< at mapping.id <<< traversed <<< _mappingFocus <<< traversed)
              == Just edgeMappingEdge.id
    edgeClasses = joinWith " " $ ["edge", "edgeMappingEdge"] <> if focused then ["focused"] else []
    edgeBorderClasses =
      joinWith " " $ Array.catMaybes
      [ Just "edgeBorder"
      , if state.hoveredElementId == (Just $ EdgeBorderId (MappingElement mapping.id mapping.sourceGraph mapping.targetGraph) edgeMappingEdge.id)
        then Just "hovered"
        else Nothing
      , if focused
        then Just "focused"
        else Nothing
      ]
    markerRef = "url(#arrow-to-edge)"
    targetHasSubgraph = false
    GraphSpacePoint2D midpointGraphSpace =
      edgeMappingEdge.midpoint
      # pageEdgeSpaceToPageSpace sourcePosPageSpace targetPosPageSpace
      # pageSpaceToGraphSpace renderPane
    GraphSpacePoint2D sourcePos = pageSpaceToGraphSpace renderPane sourcePosPageSpace
    GraphSpacePoint2D targetPos = pageSpaceToGraphSpace renderPane targetPosPageSpace
    bezierControlPoint = bezierControlPointFromParabolaPoints {p0: sourcePos, p1: midpointGraphSpace, p2: targetPos}
    edgeKey = show edgeMappingEdge.id <> "_edge_" <> show renderPane.graphId
  pure $ Tuple edgeKey $
    SE.g
    []
    [ SE.path
      [ SA.class_ edgeClasses
      , SA.d [ SVGT.Abs (SVGT.M sourcePos.x sourcePos.y)
             , SVGT.Abs (SVGT.Q bezierControlPoint.x bezierControlPoint.y targetPos.x targetPos.y)
             ]
      , SA.markerEnd markerRef
      ]
    , SE.path
      [ SA.class_ edgeBorderClasses
      , SA.d [ SVGT.Abs (SVGT.M sourcePos.x sourcePos.y)
             , SVGT.Abs (SVGT.Q bezierControlPoint.x bezierControlPoint.y targetPos.x targetPos.y)
             ]
      , HE.onMouseDown \e -> Just
                             $ StopPropagation (ME.toEvent e)
                             $ EdgeMappingEdgeDragStart mapping.id edgeMappingEdge e
      , HE.onMouseEnter \_ -> Just $ Hover $ Just $ EdgeBorderId (MappingElement mapping.id mapping.sourceGraph mapping.targetGraph) edgeMappingEdge.id
      , HE.onMouseLeave \_ -> Just $ Hover Nothing
      , HE.onDoubleClick \e -> Just $ StopPropagation (ME.toEvent e)
                               $ AppDeleteEdgeMappingEdge mapping.id edgeMappingEdge.id
      ]
    ]

renderEdge :: AppState -> GraphView -> Edge -> Maybe (Tuple String (H.ComponentHTML Action Slots Aff))
renderEdge state renderPane edge = do
  GraphSpacePoint2D sourcePos <- lookupNodePositionInPane state edge.graphId edge.source renderPane
  GraphSpacePoint2D targetPos <- lookupNodePositionInPane state edge.graphId edge.target renderPane
  let
    focused = renderPane.focus == (Just $ FocusEdge edge.id [])
    drawingEdgeFromDifferentGraphEdgeExists =
      state.drawingEdges
      # Map.filter (\drawingEdge -> case drawingEdge.source of
                                      EdgeSource _ -> drawingEdge.sourceGraph /= edge.graphId
                                      NodeSource _ -> false)
      # Map.size
      # \n -> n > 0
    borderHovered = state.hoveredElementId == (Just $ EdgeBorderId (GraphElement edge.graphId) edge.id)
    haloHovered = state.hoveredElementId == (Just $ EdgeHaloId (GraphElement edge.graphId) edge.id)
    edgeClasses = joinWith " " $ ["edge"] <> if focused then ["focused"] else []
    edgeBorderClasses =
      joinWith " " $ Array.catMaybes
      [ Just "edgeBorder"
      , if borderHovered
        then Just "hovered"
        else Nothing
      ]
    edgeHaloClasses =
      joinWith " " $ Array.catMaybes
      [ Just "edgeHalo"
      , if haloHovered
        then Just "hovered"
        else Nothing
      , if haloHovered && drawingEdgeFromDifferentGraphEdgeExists
        then Just "ready"
        else Nothing
      ]
    edgeKey = show edge.id <> "_edge_" <> show renderPane.graphId
    targetHasSubgraph = isJust do
      graphState <- Map.lookup edge.graphId state.megagraph.graphs
      targetNode <- Map.lookup edge.target graphState.graph.nodes
      targetNode.subgraph
    markerRef = if targetHasSubgraph
                then "url(#arrow-to-group)"
                else "url(#arrow)"
    GraphSpacePoint2D midpointGraphSpace =
      graphEdgeSpaceToGraphSpace
        (GraphSpacePoint2D sourcePos)
        (GraphSpacePoint2D targetPos)
        edge.midpoint
    bezierParabola = {p0: sourcePos, p1: midpointGraphSpace, p2: targetPos}
    haloParabolaPos = parallelParabola edgeHaloOffset bezierParabola
    haloParabolaNeg = parallelParabola (- edgeHaloOffset) bezierParabola
    bezierControlPoint = bezierControlPointFromParabolaPoints bezierParabola
    bezierControlPointHaloPos = bezierControlPointFromParabolaPoints haloParabolaPos
    bezierControlPointHaloNeg = bezierControlPointFromParabolaPoints haloParabolaNeg
  pure $ Tuple edgeKey $
    SE.g
    []
    -- Edge halo for drawing edge-mapping edges
    [ SE.path
      [ SA.class_ edgeHaloClasses
      , SA.d [ SVGT.Abs (SVGT.M haloParabolaPos.p0.x haloParabolaPos.p0.y)
             , SVGT.Abs (SVGT.Q bezierControlPointHaloPos.x bezierControlPointHaloPos.y haloParabolaPos.p2.x haloParabolaPos.p2.y)
             ]
      , HE.onMouseDown \e -> Just
                             $ StopPropagation (ME.toEvent e)
                             $ EdgeDrawStart renderPane (EdgeSource edge.id) e
      , HE.onMouseEnter \_ -> Just $ Hover $ Just $ EdgeHaloId (GraphElement edge.graphId) edge.id
      , HE.onMouseLeave \_ -> Just $ Hover Nothing
      ]
    , SE.path
      [ SA.class_ edgeHaloClasses
      , SA.d [ SVGT.Abs (SVGT.M haloParabolaNeg.p0.x haloParabolaNeg.p0.y)
             , SVGT.Abs (SVGT.Q bezierControlPointHaloNeg.x bezierControlPointHaloNeg.y haloParabolaNeg.p2.x haloParabolaNeg.p2.y)
             ]
      , HE.onMouseDown \e -> Just
                             $ StopPropagation (ME.toEvent e)
                             $ EdgeDrawStart renderPane (EdgeSource edge.id) e
      , HE.onMouseEnter \_ -> Just $ Hover $ Just $ EdgeHaloId (GraphElement edge.graphId) edge.id
      , HE.onMouseLeave \_ -> Just $ Hover Nothing
      ]
    -- Edge line
    , SE.path
      [ SA.class_ edgeClasses
      , SA.d [ SVGT.Abs (SVGT.M sourcePos.x sourcePos.y)
             , SVGT.Abs (SVGT.Q bezierControlPoint.x bezierControlPoint.y targetPos.x targetPos.y)
             ]
      , SA.markerEnd markerRef
      ]
    -- Edge border for grabbing and dragging
    , SE.path
      [ SA.class_ edgeBorderClasses
      , SA.d [ SVGT.Abs (SVGT.M sourcePos.x sourcePos.y)
             , SVGT.Abs (SVGT.Q bezierControlPoint.x bezierControlPoint.y targetPos.x targetPos.y)
             ]
      , HE.onMouseDown \e -> Just
                             $ StopPropagation (ME.toEvent e)
                             $ EdgeDragStart edge.graphId edge.id edge.midpoint e
      , HE.onMouseEnter \_ -> Just $ Hover $ Just $ EdgeBorderId (GraphElement edge.graphId) edge.id
      , HE.onMouseLeave \_ -> Just $ Hover Nothing
      , HE.onDoubleClick \e -> Just $ StopPropagation (ME.toEvent e)
                               $ AppDeleteEdge renderPane.graphId edge
      ]
    ]


renderEdgeTextField :: AppState -> GraphView -> Edge -> Maybe (Tuple String (H.ComponentHTML Action Slots Aff))
renderEdgeTextField state renderPane edge = do
  targetNode <- state ^? _graphAtId edge.graphId <<< _nodes <<< at edge.target <<< traversed
  sourceNode <- state ^? _graphAtId edge.graphId <<< _nodes <<< at edge.source <<< traversed
  let
    GraphSpacePoint2D midpoint =
      graphEdgeSpaceToGraphSpace
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
