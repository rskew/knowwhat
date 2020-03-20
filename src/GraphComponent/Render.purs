module GraphComponent.Render where

import Prelude

import AppState (Action(..), AppState, DrawingEdge, EdgeSourceElement(..), HoveredElementId(..), Slots, TextFieldElement(..), _drawingEdges, _edgeTextField, _nodeTextField, _titleTextField)
import CSS as CSS
import ContentEditable.SVGComponent as SVGContentEditable
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Array as Array
import Data.Lens ((^.), (^?), traversed)
import Data.Lens.At (at)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Set as Set
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import GraphComponent.Utils (bezierControlPointFromParabolaPoints, drawingEdgeWithinNodeHalo, lookupNodePositionInPane, parallelParabola)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Elements.Keyed as HK
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Megagraph (Edge, EdgeMappingEdge, Graph, GraphId, GraphSpacePoint2D(..), GraphView, Mapping, MegagraphElement(..), Node, NodeMappingEdge, PageEdgeSpacePoint2D(..), PageSpacePoint2D(..), _graph, _isValid, _node, _nodes, _pane, _position, _subgraph, _title, edgeArray, edgeMidpoint, graphEdgeSpaceToGraphSpace, graphSpaceToPageSpace, lookupEdgeById, nodePosition, pageEdgeSpaceToPageSpace, pageSpaceToGraphSpace)
import MegagraphOperation (GraphOperation(..), MegagraphComponent(..), MegagraphOperation(..))
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
    nodeFocused = state.focus == Just (NodeElement node.graphId node.id)
    hoveredOverBorder = state.hoveredElements # Set.member (NodeBorderId pane.graphId node.id)
    hoveredOverHalo   = state.hoveredElements # Set.member (NodeHaloId pane.graphId node.id)
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
    nodePendingServerResponse = isJust $ Map.lookup node.id state.pending
    nodeTextFocused = (state.textFocused <#> _.textFieldElement) == Just (NodeTextField node.graphId node.id)
    nodeClasses =
      joinWith " " $ Array.catMaybes
      [ Just "node"
      , if nodeFocused
        then Just "focused"
        else Nothing
      , if nodeTextFocused
        then Just "textFocused"
        else Nothing
      ]
    nodeBorderClasses =
        joinWith " " $ Array.catMaybes
        [ Just "nodeBorder"
        , if hoveredOverBorder
             &&
             (not drawingEdgeOverNode)
             &&
             (not nodeFocused)
          then Just "hovered"
          else Nothing
        , if not node.isValid
          then Just "invalid"
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
      , if nodePendingServerResponse
        then Just "pending"
        else Nothing
      ]
    textBoxHTML textBoxOffset =
      [ SE.g
        [ SA.transform [ SVGT.Translate textBoxOffset.x textBoxOffset.y ]
        , HE.onMouseDown \e -> Just $ StopPropagation (ME.toEvent e)
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
          -- Handle messages raised by component
          case _ of
            SVGContentEditable.TextUpdate text -> Just $ NodeTextInput node.graphId node.id text
            SVGContentEditable.Focused text -> Just $ OnFocusText (NodeTextField node.graphId node.id) \updateText ->
              if text == updateText
              then Nothing
              else let
                     op = [GraphComponentOperation node.graphId $ UpdateNodes [node] [(node {text = updateText})]]
                     target = GraphComponent node.graphId
                   in
                     Just $ Tuple op target
            SVGContentEditable.Blurred _ -> Just $ OnBlurText
        ]
      ]
    graphNodeHTML =
      -- Node Halo, for creating edges from
      [ SE.circle
        [ SA.class_ haloClasses
        , SA.r $ show haloRadius
        , HE.onMouseDown \e -> Just $ DoMany [ StopPropagation (ME.toEvent e)
                                             , EdgeDrawStart pane (NodeSource node.id) e
                                             ]
        , HE.onMouseEnter \_ -> Just $ Hover $ NodeHaloId node.graphId node.id
        , HE.onMouseLeave \_ -> Just $ UnHover $ NodeHaloId node.graphId node.id
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
        , HE.onMouseDown \e -> Just $ DoMany [ StopPropagation (ME.toEvent e)
                                             , NodeDragStart node.graphId node.id (nodePosition node) e
                                             ]
        , HE.onDoubleClick \e -> Just $ DoMany [ StopPropagation (ME.toEvent e)
                                               , AppDeleteNode node
                                               ]
        , HE.onMouseEnter \_ -> Just $ Hover $ NodeBorderId node.graphId node.id
        , HE.onMouseLeave \_ -> Just $ UnHover $ NodeBorderId node.graphId node.id
        ]
      ] <> textBoxHTML nodeTextBoxOffset
    GraphSpacePoint2D nodePos = nodePosition node
    shiftedNodeHTML = SE.g
               [ SA.transform [ SVGT.Translate nodePos.x nodePos.y ] ]
               graphNodeHTML
  in
    Tuple (show node.id) shiftedNodeHTML

renderGhostNode :: AppState -> GraphView -> Node -> Maybe (Tuple String (H.ComponentHTML Action Slots Aff))
renderGhostNode state renderPane node =
  case state.megagraph ^? _pane node.graphId of
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
        GraphSpacePoint2D nodePos = (nodePosition node) # graphSpaceToPageSpace nativePane # pageSpaceToGraphSpace renderPane
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
    focused = state.focus == Just (NodeMappingEdgeElement mapping.id nodeMappingEdge.id)
    edgePendingServerResponse = isJust $ Map.lookup nodeMappingEdge.id state.pending
    edgeClasses = joinWith " " $ ["edge", "nodeMappingEdge"] <> if focused then ["focused"] else []
    edgeBorderClasses =
      joinWith " " $ Array.catMaybes
      [ Just "edgeBorder"
      , if state.hoveredElements # Set.member (EdgeBorderId (MappingComponent mapping.id mapping.sourceGraph mapping.targetGraph) nodeMappingEdge.id)
        then Just "hovered"
        else Nothing
      , if focused
        then Just "focused"
        else Nothing
      , if edgePendingServerResponse
        then Just "pending"
        else Nothing
      ]
    targetHasSubgraph =
      isJust $ state.megagraph ^? _graph mapping.targetGraph <<< _node nodeMappingEdge.targetNode <<< _subgraph <<< traversed
    markerRef = if targetHasSubgraph
                then "url(#arrow-to-group)"
                else "url(#arrow)"
    sourcePosPageSpace = graphSpaceToPageSpace renderPane (GraphSpacePoint2D sourcePos)
    targetPosPageSpace = graphSpaceToPageSpace renderPane (GraphSpacePoint2D targetPos)
    GraphSpacePoint2D midpointGraphSpace =
      PageEdgeSpacePoint2D {angle: nodeMappingEdge.midpointAngle, radius: nodeMappingEdge.midpointRadius}
      # pageEdgeSpaceToPageSpace sourcePosPageSpace targetPosPageSpace
      # pageSpaceToGraphSpace renderPane
    bezierControlPoint = bezierControlPointFromParabolaPoints {p0: sourcePos, p1: midpointGraphSpace, p2: targetPos}
    edgeKey = show nodeMappingEdge.id <> "_edge_" <> show renderPane.graphId
  pure $ Tuple edgeKey $
    SE.g
    []
    -- Edge line
    [ SE.path
      [ SA.class_ edgeClasses
      , SA.d [ SVGT.Abs (SVGT.M sourcePos.x sourcePos.y)
             , SVGT.Abs (SVGT.Q bezierControlPoint.x bezierControlPoint.y targetPos.x targetPos.y)
             ]
      , SA.markerEnd markerRef
      ]
    -- Edge border
    , SE.path
      [ SA.class_ edgeBorderClasses
      , SA.d [ SVGT.Abs (SVGT.M sourcePos.x sourcePos.y)
             , SVGT.Abs (SVGT.Q bezierControlPoint.x bezierControlPoint.y targetPos.x targetPos.y)
             ]
      , HE.onMouseDown \e -> Just $ DoMany [ StopPropagation (ME.toEvent e)
                                           , NodeMappingEdgeDragStart mapping.id nodeMappingEdge e
                                           ]
      , HE.onMouseEnter \_ -> Just $ Hover $ EdgeBorderId (MappingComponent mapping.id mapping.sourceGraph mapping.targetGraph) nodeMappingEdge.id
      , HE.onMouseLeave \_ -> Just $ UnHover $ EdgeBorderId (MappingComponent mapping.id mapping.sourceGraph mapping.targetGraph) nodeMappingEdge.id
      , HE.onDoubleClick \e -> Just $ DoMany [ StopPropagation (ME.toEvent e)
                                             , AppDeleteNodeMappingEdge mapping.id nodeMappingEdge.id
                                             ]
      ]
    ]

renderEdgeMappingEdge :: AppState -> GraphView -> Mapping -> EdgeMappingEdge -> Maybe (Tuple String (H.ComponentHTML Action Slots Aff))
renderEdgeMappingEdge state renderPane mapping edgeMappingEdge = do
  sourceGraph <- state.megagraph ^? _graph mapping.sourceGraph
  sourceEdge <- lookupEdgeById edgeMappingEdge.sourceEdge sourceGraph
  sourcePane <- state.megagraph ^? _pane mapping.sourceGraph
  sourceEdgeSourcePos <- sourceGraph ^? _nodes <<< at sourceEdge.source <<< traversed <<< _position
  sourceEdgeTargetPos <- sourceGraph ^? _nodes <<< at sourceEdge.target <<< traversed <<< _position
  targetGraph <- state.megagraph ^? _graph mapping.targetGraph
  targetEdge <- lookupEdgeById edgeMappingEdge.targetEdge targetGraph
  targetPane <- state.megagraph ^? _pane mapping.targetGraph
  targetEdgeSourcePos <- targetGraph ^? _nodes <<< at targetEdge.source <<< traversed <<< _position
  targetEdgeTargetPos <- targetGraph ^? _nodes <<< at targetEdge.target <<< traversed <<< _position
  let
    sourcePosGraphSpace = graphEdgeSpaceToGraphSpace sourceEdgeSourcePos sourceEdgeTargetPos (edgeMidpoint sourceEdge)
    targetPosGraphSpace = graphEdgeSpaceToGraphSpace targetEdgeSourcePos targetEdgeTargetPos (edgeMidpoint targetEdge)
    sourcePosPageSpace = graphSpaceToPageSpace sourcePane sourcePosGraphSpace
    targetPosPageSpace = graphSpaceToPageSpace targetPane targetPosGraphSpace
    focused = state.focus == Just (EdgeMappingEdgeElement mapping.id edgeMappingEdge.id)
    edgePendingServerResponse = isJust $ Map.lookup edgeMappingEdge.id state.pending
    edgeClasses = joinWith " " $ ["edge", "edgeMappingEdge"] <> if focused then ["focused"] else []
    edgeBorderClasses =
      joinWith " " $ Array.catMaybes
      [ Just "edgeBorder"
      , if state.hoveredElements # Set.member (EdgeBorderId (MappingComponent mapping.id mapping.sourceGraph mapping.targetGraph) edgeMappingEdge.id)
        then Just "hovered"
        else Nothing
      , if focused
        then Just "focused"
        else Nothing
      , if edgePendingServerResponse
        then Just "pending"
        else Nothing
      ]
    markerRef = "url(#arrow-to-edge)"
    targetHasSubgraph = false
    GraphSpacePoint2D midpointGraphSpace =
      PageEdgeSpacePoint2D {angle: edgeMappingEdge.midpointAngle, radius: edgeMappingEdge.midpointRadius}
      # pageEdgeSpaceToPageSpace sourcePosPageSpace targetPosPageSpace
      # pageSpaceToGraphSpace renderPane
    GraphSpacePoint2D sourcePos = pageSpaceToGraphSpace renderPane sourcePosPageSpace
    GraphSpacePoint2D targetPos = pageSpaceToGraphSpace renderPane targetPosPageSpace
    bezierControlPoint = bezierControlPointFromParabolaPoints {p0: sourcePos, p1: midpointGraphSpace, p2: targetPos}
    edgeKey = show edgeMappingEdge.id <> "_edge_" <> show renderPane.graphId
  pure $ Tuple edgeKey $
    SE.g
    []
    -- Edge line
    [ SE.path
      [ SA.class_ edgeClasses
      , SA.d [ SVGT.Abs (SVGT.M sourcePos.x sourcePos.y)
             , SVGT.Abs (SVGT.Q bezierControlPoint.x bezierControlPoint.y targetPos.x targetPos.y)
             ]
      , SA.markerEnd markerRef
      , SA.markerStart "url(#edge-mapping-edge-begin)"
      ]
    -- Edge border
    , SE.path
      [ SA.class_ edgeBorderClasses
      , SA.d [ SVGT.Abs (SVGT.M sourcePos.x sourcePos.y)
             , SVGT.Abs (SVGT.Q bezierControlPoint.x bezierControlPoint.y targetPos.x targetPos.y)
             ]
      , HE.onMouseDown \e -> Just $ DoMany [ StopPropagation (ME.toEvent e)
                                           , EdgeMappingEdgeDragStart mapping.id edgeMappingEdge e
                                           ]
      , HE.onMouseEnter \_ -> Just $ Hover $ EdgeBorderId (MappingComponent mapping.id mapping.sourceGraph mapping.targetGraph) edgeMappingEdge.id
      , HE.onMouseLeave \_ -> Just $ UnHover $ EdgeBorderId (MappingComponent mapping.id mapping.sourceGraph mapping.targetGraph) edgeMappingEdge.id
      , HE.onDoubleClick \e -> Just $ DoMany [ StopPropagation (ME.toEvent e)
                                             , AppDeleteEdgeMappingEdge mapping.id edgeMappingEdge.id
                                             ]
      ]
    ]

renderEdge :: AppState -> GraphView -> Edge -> Maybe (Tuple String (H.ComponentHTML Action Slots Aff))
renderEdge state renderPane edge = do
  GraphSpacePoint2D sourcePos <- lookupNodePositionInPane state edge.graphId edge.source renderPane
  GraphSpacePoint2D targetPos <- lookupNodePositionInPane state edge.graphId edge.target renderPane
  let
    focused = state.focus == Just (EdgeElement edge.graphId edge.id)
    drawingEdgeFromDifferentGraphEdgeExists =
      state.drawingEdges
      # Map.filter (\drawingEdge -> case drawingEdge.source of
                                      EdgeSource _ -> drawingEdge.sourceGraph /= edge.graphId
                                      NodeSource _ -> false)
      # Map.size
      # \n -> n > 0
    borderHovered = state.hoveredElements # Set.member (EdgeBorderId (GraphComponent edge.graphId) edge.id)
    haloHovered = state.hoveredElements # Set.member (EdgeHaloId (GraphComponent edge.graphId) edge.id)
    edgeTextFocused = (state.textFocused <#> _.textFieldElement) == Just (EdgeTextField edge.graphId edge.id)
    edgePendingServerResponse = isJust $ Map.lookup edge.id state.pending
    edgeClasses = joinWith " " $ Array.catMaybes
                  [ Just "edge"
                  , if focused
                    then Just "focused"
                    else Nothing
                  , if edgeTextFocused
                    then Just "textFocused"
                    else Nothing
                  ]
    edgeBorderClasses =
      joinWith " " $ Array.catMaybes
      [ Just "edgeBorder"
      , if borderHovered
        then Just "hovered"
        else Nothing
      , if edgePendingServerResponse
        then Just "pending"
        else Nothing
      ]
    edgeHaloClasses =
      joinWith " " $ Array.catMaybes
      [ Just "edgeHalo"
      , if haloHovered
        then Just "hovered"
        else Nothing
      , if (haloHovered || borderHovered) && drawingEdgeFromDifferentGraphEdgeExists
        then Just "ready"
        else Nothing
      ]
    edgeKey = show edge.id <> "_edge_" <> show renderPane.graphId
    targetHasSubgraph = isJust do
      graph <- Map.lookup edge.graphId state.megagraph.graphs
      targetNode <- Map.lookup edge.target graph.nodes
      targetNode.subgraph
    markerRef = if targetHasSubgraph
                then "url(#arrow-to-group)"
                else "url(#arrow)"
    GraphSpacePoint2D midpointGraphSpace =
      graphEdgeSpaceToGraphSpace
        (GraphSpacePoint2D sourcePos)
        (GraphSpacePoint2D targetPos)
        (edgeMidpoint edge)
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
      , HE.onMouseDown \e -> Just $ DoMany [ StopPropagation (ME.toEvent e)
                                           , EdgeDrawStart renderPane (EdgeSource edge.id) e
                                           ]
      , HE.onMouseEnter \_ -> Just $ Hover $ EdgeHaloId (GraphComponent edge.graphId) edge.id
      , HE.onMouseLeave \_ -> Just $ UnHover $ EdgeHaloId (GraphComponent edge.graphId) edge.id
      ]
    , SE.path
      [ SA.class_ edgeHaloClasses
      , SA.d [ SVGT.Abs (SVGT.M haloParabolaNeg.p0.x haloParabolaNeg.p0.y)
             , SVGT.Abs (SVGT.Q bezierControlPointHaloNeg.x bezierControlPointHaloNeg.y haloParabolaNeg.p2.x haloParabolaNeg.p2.y)
             ]
      , HE.onMouseDown \e -> Just $ DoMany [ StopPropagation (ME.toEvent e)
                                           , EdgeDrawStart renderPane (EdgeSource edge.id) e
                                           ]
      , HE.onMouseEnter \_ -> Just $ Hover $ EdgeHaloId (GraphComponent edge.graphId) edge.id
      , HE.onMouseLeave \_ -> Just $ UnHover $ EdgeHaloId (GraphComponent edge.graphId) edge.id
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
      , HE.onMouseDown \e -> Just $ DoMany [ StopPropagation (ME.toEvent e)
                                           , EdgeDragStart edge.graphId edge.id (edgeMidpoint edge) e
                                           ]
      , HE.onMouseEnter \_ -> Just $ Hover $ EdgeBorderId (GraphComponent edge.graphId) edge.id
      , HE.onMouseLeave \_ -> Just $ UnHover $ EdgeBorderId (GraphComponent edge.graphId) edge.id
      , HE.onDoubleClick \e -> Just $ DoMany [ StopPropagation (ME.toEvent e)
                                             , AppDeleteEdge edge
                                             ]
      ]
    ]


renderEdgeTextField :: AppState -> GraphView -> Edge -> Maybe (Tuple String (H.ComponentHTML Action Slots Aff))
renderEdgeTextField state renderPane edge = do
  targetNode <- state.megagraph ^? _graph edge.graphId <<< _node edge.target
  sourceNode <- state.megagraph ^? _graph edge.graphId <<< _node edge.source
  let
    GraphSpacePoint2D midpoint =
      graphEdgeSpaceToGraphSpace
        (nodePosition sourceNode)
        (nodePosition targetNode)
        (edgeMidpoint edge)
    focused = state.focus == Just (EdgeElement edge.graphId edge.id)
    hidden = if not focused && (edge.text == "")
             then " hidden"
             else ""
  Just $ Tuple (show edge.id <> "_textField_" <> show renderPane.graphId) $
    SE.g
    [ SA.class_ $ "edgeTextField" <> hidden
    , SA.transform [ SVGT.Translate
                     (midpoint.x + edgeTextBoxOffset.x)
                     (midpoint.y + edgeTextBoxOffset.y)
                   ]
    , HE.onMouseDown \e -> Just $ StopPropagation (ME.toEvent e)
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
      case _ of
        SVGContentEditable.TextUpdate text -> Just $ EdgeTextInput edge.graphId edge.id text
        SVGContentEditable.Focused text -> Just $ OnFocusText (EdgeTextField edge.graphId edge.id) \updateText ->
          if text == updateText
          then Nothing
          else let
                 op = [GraphComponentOperation edge.graphId $ UpdateEdges [edge] [(edge {text = updateText})]]
                 target = GraphComponent edge.graphId
               in
                 Just $ Tuple op target
        SVGContentEditable.Blurred _ -> Just $ OnBlurText
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
    , HE.onMouseDown \e -> Just $ StopPropagation (ME.toEvent e)
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
      case _ of
        SVGContentEditable.TextUpdate text -> Just $ TitleTextInput graphId text
        SVGContentEditable.Focused text -> Just $ OnFocusText (TitleTextField graphId) \updateText ->
          if text == updateText
          then Nothing
          else let
                 op = [GraphComponentOperation graphId $ UpdateTitle text updateText]
                 target = GraphComponent graphId
               in
                 Just $ Tuple op target
        SVGContentEditable.Blurred _ -> Just $ OnBlurText
    ]

renderTitleInvalidIndicator :: AppState -> GraphId -> Tuple String (H.ComponentHTML Action Slots Aff)
renderTitleInvalidIndicator state graphId =
  let
    invalid = case state.megagraph ^? _graph graphId <<< _title <<< _isValid of
      Nothing -> ""
      Just true -> ""
      Just false -> " invalid"
    pending = case Map.lookup graphId state.pending of
      Nothing -> ""
      Just _ -> " pending"
  in
    Tuple (show graphId <> "_titleInvalidIndicator") $
      SE.rect
      [ SA.class_ $ "invalidIndicator" <> pending <> invalid
      , SA.width  $ show invalidIndicatorSize
      , SA.height $ show invalidIndicatorSize
      , SA.x $ show invalidIndicatorOffset.x
      , SA.y $ show invalidIndicatorOffset.y
      ]

renderSinglePane :: AppState -> GraphView -> Graph -> H.ComponentHTML Action Slots Aff
renderSinglePane state renderPane graph =
  let
    nodes                 = graph.nodes
                            # Map.values # Array.fromFoldable
                            # Array.filter \node -> not node.deleted

    keyedNodes            = nodes <#> renderGraphNode state renderPane

    allNodesInOtherGraphs = state.megagraph.graphs
                            # Map.values >>> Array.fromFoldable
                            # Array.filter (\graph' -> graph'.id /= graph.id)
                            # Array.concatMap (Array.fromFoldable <<< Map.values <<< _.nodes)
                            # Array.filter (\node -> not node.deleted)

    ghostNodes            = Array.catMaybes $ (allNodesInOtherGraphs <#> renderGhostNode state renderPane)

    allMappings           = state.megagraph.mappings
                            # Map.values >>> Array.fromFoldable
                            # Array.filter (\mapping -> mapping.sourceGraph == graph.id
                                                     || mapping.targetGraph == graph.id)

    keyedNodeMappingEdges :: Array (Tuple String (H.ComponentHTML Action Slots Aff))
    keyedNodeMappingEdges = allMappings # Array.concatMap \mapping ->
                              mapping.nodeMappingEdges
                              # Array.fromFoldable
                              # Array.filter (\edge -> not edge.deleted)
                              <#> renderNodeMappingEdge state renderPane mapping
                              # Array.catMaybes

    keyedEdgeMappingEdges = allMappings # Array.concatMap \mapping ->
                              mapping.edgeMappingEdges
                              # Array.fromFoldable
                              # Array.filter (\edge -> not edge.deleted)
                              <#> renderEdgeMappingEdge state renderPane mapping
                              # Array.catMaybes

    edges                 = edgeArray graph
                            # Array.filter \edge -> not edge.deleted

    keyedEdges            = Array.mapMaybe (renderEdge state renderPane) edges

    keyedEdgeTextFields   = Array.mapMaybe (renderEdgeTextField state renderPane) edges

    drawingEdges          = state.drawingEdges # Map.values # Array.fromFoldable

    keyedDrawingEdges     = Array.mapMaybe (renderDrawingEdge state renderPane) drawingEdges

    keyedTitle = [ renderTitle renderPane.graphId graph.title.text ]

    keyedTitleInvalidIndicator = [ renderTitleInvalidIndicator state renderPane.graphId ]

    zoom                    = renderPane.zoom
    PageSpacePoint2D origin = renderPane.origin
    boundingRect            = renderPane.boundingRect
    focusedClass = case state.focusedPane of
      Just (GraphComponent graphId) ->
        if graphId == renderPane.graphId then "focused" else ""
      Just (MappingComponent _ sourceGraphId targetGraphId) ->
        if sourceGraphId == renderPane.graphId then "focusedSource" else
          if targetGraphId == renderPane.graphId then "focusedTarget" else ""
      Nothing -> ""

  in
    HH.div
    [ HCSS.style do
        CSS.left    $ CSS.px boundingRect.left
        CSS.top     $ CSS.px boundingRect.top
        CSS.width   $ CSS.px boundingRect.width
        CSS.height  $ CSS.px boundingRect.height
    , HP.classes $ [ HH.ClassName "pane",  HH.ClassName focusedClass ]
    ]
    [ SE.svg
      [ SA.viewBox
        -- scale then shift
        ( - origin.x * zoom )
        ( - origin.y * zoom )
        ( boundingRect.width  * zoom )
        ( boundingRect.height * zoom )
      , HE.onMouseDown \e -> Just $ DoMany [ StopPropagation (ME.toEvent e)
                                           , BackgroundDragStart renderPane.graphId (PageSpacePoint2D origin) e
                                           ]
      , HE.onDoubleClick \e -> Just $ DoMany [ StopPropagation (ME.toEvent e)
                                             , AppCreateNode renderPane e
                                             ]
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
      Map.keys state.megagraph.graphs
      # Array.fromFoldable
      <#> (\graphId -> do
        graph <- Map.lookup graphId state.megagraph.graphs
        pane <- Map.lookup graphId state.megagraph.panes
        pure $ Tuple (show graphId) $ renderSinglePane state pane graph
      )
      # Array.catMaybes
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
