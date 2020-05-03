module GraphComponent.HandleAction where

import Prelude

import AppState (Action(..), AppState, EdgeSourceElement(..), HistoryUpdate(..), HoveredElementId(..), Message, Query(..), Slots, TextFieldElement(..), _controlDown, _drawingEdgePosition, _drawingEdgeTargetGraph, _drawingEdges, _edgeTextField, _graphHistory, _history, _hoveredElements, _keyHoldState, _liveMegagraph, _mappingHistory, _megagraph, _megagraphHistory, _nodeTextField, _origin, _pane, _panes, _selectedEdges, _spaceDown, _titleTextField, _undone, _zoom, applyMegagraphStateUpdates, lookupMapping, updateHistory, updateUndone)
import Config as Config
import ContentEditable.SVGComponent as SVGContentEditable
import Control.Alt ((<|>))
import Control.Monad.Except (runExcept)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT, lift)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Lens (traversed, (%~), (.~), (^..), (^?), (?~))
import Data.Lens.At (at)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Set as Set
import Data.String (joinWith)
import Data.Traversable (for_)
import Data.Tuple (Tuple(..), fst, snd)
import Data.UUID (genUUID)
import Data.UUID as UUID
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Console as Console
import Foreign (F, renderForeignError)
import Foreign as Foreign
import Foreign.Generic (encodeJSON, decodeJSON)
import GraphComponent.Utils (mouseEventPosition, updatedNodes)
import Halogen as H
import Halogen.Component.Utils.Drag as Drag
import Halogen.Query.EventSource as ES
import LiveMegagraph (MegagraphMutation(..), invertMegagraphMutation)
import LiveMegagraph as LiveMegagraph
import Math as Math
import Megagraph (GraphEdgeSpacePoint2D(..), GraphId, GraphSpacePoint2D(..), MappingId, MegagraphElement(..), PageEdgeSpacePoint2D(..), PageSpacePoint2D(..), _edge, _edgeMappingEdge, _graph, _graphs, _isValid, _mapping, _mappings, _node, _nodeMappingEdge, _position, _subgraph, _text, _title, edgeArray, edgeMidpoint, edgeSetToPathEquation, freshEdge, freshEdgeMappingEdge, freshNode, freshNodeMappingEdge, freshPane, graphEdgeSpaceToGraphSpace, graphSpaceToGraphEdgeSpace, graphSpaceToPageSpace, lookupEdgeById, mappingEdgeMidpoint, nodePosition, pageEdgeSpaceToPageSpace, pageSpaceToGraphSpace, pageSpaceToPageEdgeSpace)
import MegagraphStateUpdate (MegagraphComponent(..), MegagraphStateUpdate(..), encodeMegagraphAsMegagraphStateUpdates)
import UI.Constants (pendingIndicatorHeightPx, selfEdgeInitialAngle, selfEdgeInitialRadius, zoomScaling)
import UI.Panes (arrangePanes, paneContainingPoint, rescaleWindow, zoomAtPoint)
import Utils (tupleApply)
import Web.Event.Event as WE
import Web.Event.EventTarget as ET
import Web.File.File as File
import Web.File.FileList as FileList
import Web.File.FileReader as FileReader
import Web.HTML as WH
import Web.HTML.Event.EventTypes as WHET
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as WHE
import Web.HTML.HTMLInputElement as WHIE
import Web.HTML.Window as WHW
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET
import Web.UIEvent.WheelEvent as WhE
import WireData (WireDataRaw, decodeWireData, encodeWireData)


foreign import loadFile :: Effect Unit
foreign import saveJSON :: String -> String -> Effect Unit


handleAction :: Action -> H.HalogenM AppState Action Slots Message Aff Unit
handleAction = case _ of
  DoMany actions ->
    for_ actions handleAction

  -- Keep this component's version of the megagraph up to date with the
  -- LiveMegagraph managed version.
  -- This component (UI layer) keeps a copy for rendering purposes only.
  -- keep the panes in sync with graphs when LiveMegagraph is updated
  MegagraphUpdated newMegagraph -> do
    state <- H.get
    let
      oldMegagraph = state.megagraph
    -- check for new graphs
    for_ newMegagraph.graphs \graph ->
      if not $ Set.member graph.id (oldMegagraph.graphs # Map.keys)
      then
        handleAction $ NewPane graph.id
      else
        pure unit
    -- check for removed graphs
    for_ oldMegagraph.graphs \graph ->
      if not $ Set.member graph.id (newMegagraph.graphs # Map.keys)
      then
        handleAction $ RemovePane graph.id
      else
        pure unit
    -- update megagraph
    H.modify_ $ _megagraph .~ newMegagraph

  ApplyMegagraphUpdate op -> applyReceivedMegagraphUpdate op

  PreventDefault e -> do
    H.liftEffect $ WE.preventDefault e

  StopPropagation e -> do
    H.liftEffect $ WE.stopPropagation e

  EvalQuery query -> do
    _ <- handleQuery query
    pure unit

  Init -> do
    -- Initialize bounding rectangle
    void $ handleQuery $ UpdateBoundingRect (const unit)
    -- Subscribe to updates in bounding rectangle
    window <- H.liftEffect $ WH.window
    void $ H.subscribe $ ES.eventListenerEventSource
                         (WE.EventType "resize")
                         (WHW.toEventTarget window)
                         \event -> Just $ EvalQuery $ UpdateBoundingRect unit
    -- Add keydown event listener to body
    document <- H.liftEffect $ WHW.document =<< WH.window
    void $ H.subscribe $ ES.eventListenerEventSource KET.keydown (HTMLDocument.toEventTarget document) (map Keypress <<< KE.fromEvent)
    -- keyup listener
    void $ H.subscribe $ ES.eventListenerEventSource KET.keyup   (HTMLDocument.toEventTarget document) (map Keyup <<< KE.fromEvent)

    -- Load the home graph
    void $ H.query _liveMegagraph unit $ H.tell
      $ LiveMegagraph.LoadGraphWithTitle Config.homeGraphTitle {onFail: const CreateNewHomeGraph, onSuccess: DoNothing}

  CreateNewHomeGraph -> do
    newGraphId <- H.liftEffect genUUID
    let megagraphMutation = StateUpdate [CreateGraph newGraphId Config.homeGraphTitle]
    applyMegagraphMutation_ megagraphMutation
    H.modify_ arrangePanes


  NewPane graphId -> do
    state <- H.get
    let
      height = case state.panes # Map.toUnfoldable # Array.head of
        Nothing -> state.windowBoundingRect.height
        Just (Tuple _ pane) -> pane.boundingRect.height
      newRect = {height: height, width: 0.0, top: 0.0, bottom: height, left: 0.0, right: 0.0}
      newPane = freshPane graphId newRect
    H.modify_ $ _panes <<< at graphId ?~ newPane
    handleAction $ UpdateFocusPane $ GraphComponent graphId
    H.modify_ $ arrangePanes

  UpdateContentEditableText graphId -> do
    state <- H.get
    case Map.lookup graphId state.megagraph.graphs of
      Nothing -> pure unit
      Just singleGraph -> do
        for_ singleGraph.nodes \node ->
          handleAction $ UpdateNodeContentEditableText node.graphId node.id
        for_ (edgeArray singleGraph) \edge ->
          H.query _edgeTextField edge.id $ H.tell $ SVGContentEditable.SetText edge.text
        _ <- H.query _titleTextField graphId $ H.tell $ SVGContentEditable.SetText singleGraph.title.text
        pure unit

  UpdateNodeContentEditableText graphId nodeId -> do
    state <- H.get
    case state.megagraph ^? _graph graphId <<< _node nodeId of
      Nothing -> pure unit
      Just node -> do
        _ <- H.query _nodeTextField node.id $ H.tell $ SVGContentEditable.SetText node.text
        pure unit

  NodeTextInput graphId nodeId text -> do
    state <- H.get
    case state.megagraph ^? _graph graphId <<< _node nodeId of
      Nothing -> pure unit
      Just node -> do
        let
          megagraphMutation = UpdateNodeText node text
        applyMegagraphMutation
          megagraphMutation
          { onFail: ConsoleLog
          , onSuccess: DoMany [ case node.subgraph of
                                  Nothing -> DoNothing
                                  Just subgraphId -> UpdateContentEditableText subgraphId
                              ]
          }
        handleAction $ UpdateFocus $ Just $ NodeElement node.graphId node.id

  EdgeTextInput graphId edgeId text -> do
    state <- H.get
    case lookupEdgeById edgeId =<< (state.megagraph ^? _graph graphId) of
      Nothing -> pure unit
      Just edge ->
        let
          op = StateUpdate [UpdateEdges [edge] [(edge {text = text})]]
        in do
          applyMegagraphMutation_ op
          handleAction $ UpdateFocus $ Just $ EdgeElement graphId edgeId

  TitleTextInput graphId newTitleText -> do
    state <- H.get
    case state.megagraph ^? _graph graphId of
      Nothing -> pure unit
      Just graph ->
        let
          megagraphMutation = UpdateTitleText graphId graph.title.text newTitleText
        in
          applyMegagraphMutation
            megagraphMutation
            { onFail: ConsoleLog
            -- Can't tell from here which graphs will have updated nodes, so just
            -- refresh the text for all graphs not being edited.
            , onSuccess: DoMany (state.megagraph.graphs # Map.values # Array.fromFoldable
                                 # Array.filter (\graph' -> graph'.id /= graphId)
                                 <#> _.id
                                 <#> UpdateContentEditableText)
            }

  OnFocusText textFieldElement giveStrForMegagraphUpdate ->
    H.modify_ _{ textFocused = Just { textFieldElement: textFieldElement
                                    , historyUpdater: giveStrForMegagraphUpdate
                                    }
               }

  OnBlurText -> do
    state <- H.get
    case do -- Maybe
      {textFieldElement, historyUpdater} <- state.textFocused
      text <- case textFieldElement of
        NodeTextField graphId nodeId -> state.megagraph ^? _graph graphId <<< _node nodeId <<< _text
        EdgeTextField graphId edgeId -> state.megagraph ^? _graph graphId <<< _edge edgeId <<< _text
        TitleTextField graphId -> state.megagraph ^? _graph graphId <<< _title <<< _text
      historyUpdater text
    of
      Nothing -> pure unit
      Just (Tuple op target) -> H.modify_ $ updateHistory (Insert op) target
    H.modify_ _{ textFocused = Nothing }

  BlurFocusedTextField -> do
    state <- H.get
    case state.textFocused <#> _.textFieldElement of
      Nothing -> pure unit
      Just textFieldElement -> case textFieldElement of
        NodeTextField graphId nodeId ->
          void $ H.query _nodeTextField nodeId $ H.tell $ SVGContentEditable.Blur
        EdgeTextField graphId edgeId ->
          void $ H.query _edgeTextField edgeId $ H.tell $ SVGContentEditable.Blur
        TitleTextField graphId ->
          void $ H.query _titleTextField graphId $ H.tell $ SVGContentEditable.Blur

  UpdateTitleValidity graphId isValid ->
    H.modify_ $ _megagraph <<< _graph graphId <<< _title <<< _isValid .~ isValid

  UpdateNodeValidity graphId nodeId isValid ->
    H.modify_ $ _megagraph <<< _graph graphId <<< _node nodeId <<< _isValid .~ isValid

  BackgroundDragStart graphId initialGraphOrigin mouseEvent -> do
    state <- H.get
    if state.keyHoldState.controlDown
    -- ctrl-click to select the mapping from the current focused pane
    -- to the clicked pane
    then do
      handleAction $ UpdateFocus Nothing
      let
        newMappingFocus sourceGraphId targetGraphId = do
          case lookupMapping sourceGraphId targetGraphId state of
            Nothing -> pure unit
            Just mapping ->
              handleAction $ UpdateFocusPane $ MappingComponent mapping.id
      case state.focusedPane of
        Just (GraphComponent sourceGraphId) -> newMappingFocus sourceGraphId graphId
        Just (MappingComponent mappingId) ->
          case Map.lookup mappingId state.megagraph.mappings of
             Nothing -> pure unit
             Just mapping -> newMappingFocus mapping.sourceGraph graphId
        _ -> handleAction $ UpdateFocusPane $ GraphComponent graphId
    else do
      handleAction $ UpdateFocus Nothing
      handleAction $ UpdateFocusPane $ GraphComponent graphId
      H.subscribe' \subscriptionId ->
        Drag.dragEventSource mouseEvent
          \e -> BackgroundDragMove e graphId initialGraphOrigin subscriptionId

  BackgroundDragMove (Drag.Move _ dragData) graphId (PageSpacePoint2D initialGraphOrigin) _ -> do
    state <- H.get
    let
      newGraphOrigin =
        PageSpacePoint2D
          { x : initialGraphOrigin.x + dragData.offsetX
          , y : initialGraphOrigin.y + dragData.offsetY
          }
    H.modify_ $ _pane graphId <<< _origin .~ newGraphOrigin

  BackgroundDragMove (Drag.Done _) _ _ subscriptionId ->
    H.unsubscribe subscriptionId

  NodeDragStart graphId nodeId initialNodePos mouseEvent -> do
    H.subscribe' \subscriptionId ->
      Drag.dragEventSource mouseEvent
        \e -> NodeDragMove e graphId nodeId initialNodePos subscriptionId
    handleAction $ UpdateFocus $ Just $ NodeElement graphId nodeId

  NodeDragMove (Drag.Move _ dragData) graphId nodeId (GraphSpacePoint2D initialNodePos) _ -> do
    state <- H.get
    case state ^? _pane graphId of
      Nothing -> pure unit
      Just pane -> do
        let
          dragOffsetGraphSpace = { x : dragData.offsetX * pane.zoom
                                 , y : dragData.offsetY * pane.zoom
                                 }
          GraphSpacePoint2D newNodePos =
            GraphSpacePoint2D
              { x : initialNodePos.x + dragOffsetGraphSpace.x
              , y : initialNodePos.y + dragOffsetGraphSpace.y
              }
        case state.megagraph ^? _graph graphId <<< _node nodeId of
          Nothing -> pure unit
          Just node ->
            let
              op = StateUpdate [UpdateNodes [node] [(node {positionX = newNodePos.x, positionY = newNodePos.y})]]
            in do
              applyMegagraphMutation_ op

  NodeDragMove (Drag.Done _) graphId nodeId (GraphSpacePoint2D initialNodePos) subscriptionId -> do
    H.unsubscribe subscriptionId
    state <- H.get
    case state.megagraph ^? _graph graphId <<< _node nodeId of
      Nothing -> pure unit
      Just node ->
        if initialNodePos == {x: node.positionX, y: node.positionY}
        then pure unit
        else
          let
            op = StateUpdate [UpdateNodes [(node {positionX = initialNodePos.x, positionY = initialNodePos.y})] [node]]
          in
            H.modify_ $ updateHistory (Insert op) (GraphComponent graphId)

  NodeMappingEdgeDragStart mappingId initialNodeMappingEdge mouseEvent -> do
    state <- H.get
    case state.megagraph ^? _mapping mappingId of
      Nothing -> pure unit
      Just mapping ->
        H.subscribe' \subscriptionId -> Drag.dragEventSource mouseEvent \e ->
          NodeMappingEdgeDragMove e mapping initialNodeMappingEdge.id (mappingEdgeMidpoint initialNodeMappingEdge) subscriptionId
    handleAction $ UpdateFocus $ Just $ NodeMappingEdgeElement mappingId initialNodeMappingEdge.id

  NodeMappingEdgeDragMove (Drag.Move _ dragData) mapping nodeMappingEdgeId initialMidpoint _ -> do
    state <- H.get
    case do
      currentNodeMappingEdge <- mapping ^? _nodeMappingEdge nodeMappingEdgeId
      sourcePos <- state.megagraph ^? _graph mapping.sourceGraph <<< _node currentNodeMappingEdge.sourceNode <<< _position
      sourcePane <- state ^? _pane mapping.sourceGraph
      targetPos <- state.megagraph ^? _graph mapping.targetGraph <<< _node currentNodeMappingEdge.targetNode <<< _position
      targetPane <- state ^? _pane mapping.targetGraph
      let
        sourcePosPageSpace = graphSpaceToPageSpace sourcePane sourcePos
        targetPosPageSpace = graphSpaceToPageSpace targetPane targetPos
        PageSpacePoint2D initialMidpointCartesian =
          pageEdgeSpaceToPageSpace sourcePosPageSpace targetPosPageSpace initialMidpoint
        newMidpointCartesian =
          PageSpacePoint2D { x : initialMidpointCartesian.x + dragData.offsetX
                           , y : initialMidpointCartesian.y + dragData.offsetY
                           }
        PageEdgeSpacePoint2D newMidpoint = pageSpaceToPageEdgeSpace sourcePosPageSpace targetPosPageSpace newMidpointCartesian
        updatedNodeMappingEdge = currentNodeMappingEdge {midpointAngle = newMidpoint.angle, midpointRadius = newMidpoint.radius}
        op = StateUpdate [UpdateNodeMappingEdges [currentNodeMappingEdge] [updatedNodeMappingEdge]]
      pure op
    of
      Nothing -> pure unit
      Just op -> applyMegagraphMutation_ op

  NodeMappingEdgeDragMove (Drag.Done _) mapping nodeMappingEdgeId (PageEdgeSpacePoint2D initialMidpoint) subscriptionId -> do
    H.unsubscribe subscriptionId
    state <- H.get
    case state.megagraph ^? _mapping mapping.id <<< _nodeMappingEdge nodeMappingEdgeId of
      Nothing -> pure unit
      Just nodeMappingEdge ->
        if initialMidpoint == {angle: nodeMappingEdge.midpointAngle, radius: nodeMappingEdge.midpointRadius}
        then pure unit
        else
          let
            initialNodeMappingEdge = nodeMappingEdge {midpointAngle = initialMidpoint.angle, midpointRadius = initialMidpoint.radius}
            op = StateUpdate [UpdateNodeMappingEdges [initialNodeMappingEdge] [nodeMappingEdge]]
            target = MappingComponent mapping.id
          in
            H.modify_ $ updateHistory (Insert op) target

  EdgeMappingEdgeDragStart mappingId initialEdgeMappingEdge mouseEvent -> do
    state <- H.get
    case state.megagraph ^? _mapping mappingId of
      Nothing -> pure unit
      Just mapping ->
        H.subscribe' \subscriptionId -> Drag.dragEventSource mouseEvent \e ->
          EdgeMappingEdgeDragMove e mapping initialEdgeMappingEdge.id (mappingEdgeMidpoint initialEdgeMappingEdge) subscriptionId
    handleAction $ UpdateFocus $ Just $ EdgeMappingEdgeElement mappingId initialEdgeMappingEdge.id

  EdgeMappingEdgeDragMove (Drag.Move _ dragData) mapping edgeMappingEdgeId initialMidpoint _ -> do
    state <- H.get
    case do
      currentEdgeMappingEdge <- mapping ^? _edgeMappingEdge edgeMappingEdgeId
      sourceGraph <- state.megagraph ^? _graph mapping.sourceGraph
      sourceEdge <- lookupEdgeById currentEdgeMappingEdge.sourceEdge sourceGraph
      sourcePane <- state ^? _pane mapping.sourceGraph
      sourceEdgeSourcePos <- sourceGraph ^? _node sourceEdge.source <<< _position
      sourceEdgeTargetPos <- sourceGraph ^? _node sourceEdge.target <<< _position
      targetGraph <- state.megagraph ^? _graph mapping.targetGraph
      targetEdge <- lookupEdgeById currentEdgeMappingEdge.targetEdge targetGraph
      targetPane <- state ^? _pane mapping.targetGraph
      targetEdgeSourcePos <- targetGraph ^? _node targetEdge.source <<< _position
      targetEdgeTargetPos <- targetGraph ^? _node targetEdge.target <<< _position
      let
        sourcePos = graphEdgeSpaceToGraphSpace sourceEdgeSourcePos sourceEdgeTargetPos (edgeMidpoint sourceEdge)
        targetPos = graphEdgeSpaceToGraphSpace targetEdgeSourcePos targetEdgeTargetPos (edgeMidpoint targetEdge)
        sourcePosPageSpace = graphSpaceToPageSpace sourcePane sourcePos
        targetPosPageSpace = graphSpaceToPageSpace targetPane targetPos
        PageSpacePoint2D initialMidpointCartesian =
          pageEdgeSpaceToPageSpace sourcePosPageSpace targetPosPageSpace initialMidpoint
        newMidpointCartesian =
          PageSpacePoint2D { x : initialMidpointCartesian.x + dragData.offsetX
                           , y : initialMidpointCartesian.y + dragData.offsetY
                           }
        PageEdgeSpacePoint2D newMidpoint = pageSpaceToPageEdgeSpace sourcePosPageSpace targetPosPageSpace newMidpointCartesian
        newEdgeMappingEdge = currentEdgeMappingEdge {midpointAngle = newMidpoint.angle, midpointRadius = newMidpoint.radius}
        op = StateUpdate [UpdateEdgeMappingEdges [currentEdgeMappingEdge] [newEdgeMappingEdge]]
      pure op
    of
      Nothing -> pure unit
      Just op -> applyMegagraphMutation_ op

  EdgeMappingEdgeDragMove (Drag.Done _) mapping edgeMappingEdgeId (PageEdgeSpacePoint2D initialMidpoint) subscriptionId -> do
    H.unsubscribe subscriptionId
    state <- H.get
    case state.megagraph ^? _mapping mapping.id <<< _edgeMappingEdge edgeMappingEdgeId of
      Nothing -> pure unit
      Just edgeMappingEdge ->
        if initialMidpoint == {angle: edgeMappingEdge.midpointAngle, radius: edgeMappingEdge.midpointRadius}
        then pure unit
        else
          let
            initialEdgeMappingEdge = edgeMappingEdge {midpointAngle = initialMidpoint.angle, midpointRadius = initialMidpoint.radius}
            op = StateUpdate [UpdateEdgeMappingEdges [initialEdgeMappingEdge] [edgeMappingEdge]]
            target = MappingComponent mapping.id
          in
            H.modify_ $ updateHistory (Insert op) target

  EdgeDragStart graphId edgeId initialMidpoint mouseEvent -> do
    H.subscribe' \subscriptionId ->
      Drag.dragEventSource mouseEvent
      \e -> EdgeDragMove e graphId edgeId initialMidpoint subscriptionId
    handleAction $ UpdateFocus $ Just $ EdgeElement graphId edgeId

  EdgeDragMove (Drag.Move _ dragData) graphId edgeId initialMidpoint _ -> do
    state <- H.get
    case do
      graph <- state.megagraph ^? _graph graphId
      edge <- lookupEdgeById edgeId graph
      sourcePos <- graph ^? _node edge.source <<< _position
      targetPos <- graph ^? _node edge.target <<< _position
      pane <- state ^? _pane graphId
      let
        dragOffsetGraphSpace = { x : dragData.offsetX * pane.zoom
                               , y : dragData.offsetY * pane.zoom
                               }
        GraphSpacePoint2D initialMidpointGraphSpace = graphEdgeSpaceToGraphSpace sourcePos targetPos initialMidpoint
        newMidpointGraphSpace =
          GraphSpacePoint2D
            { x : initialMidpointGraphSpace.x + dragOffsetGraphSpace.x
            , y : initialMidpointGraphSpace.y + dragOffsetGraphSpace.y
            }
        GraphEdgeSpacePoint2D newMidpoint = graphSpaceToGraphEdgeSpace sourcePos targetPos newMidpointGraphSpace
        op = StateUpdate [UpdateEdges [edge] [(edge {midpointAngle = newMidpoint.angle, midpointRadius = newMidpoint.radius})]]
      pure $ applyMegagraphMutation_ op
    of
      Nothing -> pure unit
      Just thingsToDo -> thingsToDo

  EdgeDragMove (Drag.Done _) graphId edgeId (GraphEdgeSpacePoint2D initialMidpoint) subscriptionId -> do
    H.unsubscribe subscriptionId
    state <- H.get
    case state.megagraph ^? _graph graphId <<< _edge edgeId of
      Nothing -> pure unit
      Just edge ->
        if initialMidpoint == {angle: edge.midpointAngle, radius: edge.midpointRadius}
        then pure unit
        else
          let
            op = StateUpdate [UpdateEdges [(edge {midpointAngle = initialMidpoint.angle, midpointRadius = initialMidpoint.radius})] [edge]]
            target = GraphComponent graphId
          in
            H.modify_ $ updateHistory (Insert op) target

  EdgeDrawStart pane sourceElement mouseEvent -> do
    state <- H.get
    case
      case sourceElement of
        NodeSource nodeId -> do
          node <- state.megagraph ^? _graph pane.graphId <<< _node nodeId
          pure (nodePosition node)
        EdgeSource edgeId -> do
          graph <- state.megagraph ^? _graph pane.graphId
          edge <- lookupEdgeById edgeId graph
          sourceNode <- Map.lookup edge.source graph.nodes
          targetNode <- Map.lookup edge.target graph.nodes
          pure $ graphEdgeSpaceToGraphSpace (nodePosition sourceNode) (nodePosition targetNode) (edgeMidpoint edge)
    of
      Nothing -> pure unit
      Just sourcePosition ->
        let
          mousePosition = mouseEventPosition mouseEvent
          sourcePositionPageSpace = graphSpaceToPageSpace pane sourcePosition
          drawingEdge = { source         : sourceElement
                        , sourcePosition : sourcePositionPageSpace
                        , sourceGraph    : pane.graphId
                        , pointPosition  : mousePosition
                        , targetGraph    : pane.graphId
                        }
        in do
          drawingEdgeId <- H.liftEffect UUID.genUUID
          H.modify_ $ _drawingEdges %~ Map.insert drawingEdgeId drawingEdge
          handleAction $ UpdateFocus $ Just case sourceElement of
            NodeSource nodeId -> NodeElement pane.graphId nodeId
            EdgeSource edgeId -> EdgeElement pane.graphId edgeId
          H.subscribe' \subscriptionId ->
            Drag.dragEventSource mouseEvent
            $ \e -> EdgeDrawMove e pane.graphId drawingEdgeId subscriptionId

  -- | Check which pane the point of the drawing edge is in and update
  -- | the drawingEdgeState so it can be drawn properly in both panes
  EdgeDrawMove (Drag.Move _ dragData) graphId drawingEdgeId _ -> do
    state <- H.get
    let
      panes = state.panes # Map.values # Array.fromFoldable
      edgePageTargetPosition = PageSpacePoint2D { x : dragData.x, y : dragData.y }
    case paneContainingPoint panes edgePageTargetPosition <|> state ^? _pane graphId of
      Nothing -> pure unit
      Just targetPane -> do
        H.modify_ $ (_drawingEdgePosition drawingEdgeId .~ edgePageTargetPosition)
                    >>> (_drawingEdgeTargetGraph drawingEdgeId .~ targetPane.graphId)

  -- Create:
  -- - a new edge if the source is a node and the drawn edge is dropped
  --   within the halo of another node of the same graph,
  -- - a new nodeMappingEdge of the source is a node and the drawn edge
  --   is dropped within the halo of a node from a different graph,
  -- - a new edgeMappingEdge if the source is an edge and the drawn edge
  --   is dropped within the halo of an edge from a different graph
  EdgeDrawMove (Drag.Done _) graphId drawingEdgeId subscriptionId -> do
    state <- H.get
    newEdgeId <- H.liftEffect UUID.genUUID
    let
      createEdgeBetweenNodes sourceNodeId sourceGraphId targetNodeId targetGraphId =
        if sourceGraphId == targetGraphId
        then handleAction $ AppCreateEdge {id: newEdgeId, graphId : sourceGraphId, source: sourceNodeId, target: targetNodeId}
        else
          handleAction $ AppCreateNodeMappingEdge newEdgeId sourceNodeId sourceGraphId targetNodeId targetGraphId
      createEdgeBetweenEdges sourceEdgeId sourceGraphId targetEdgeId targetGraphId =
        if sourceGraphId == targetGraphId
        then pure unit
        else
          handleAction $ AppCreateEdgeMappingEdge newEdgeId sourceEdgeId sourceGraphId targetEdgeId targetGraphId
    case Map.lookup drawingEdgeId state.drawingEdges of
      Nothing -> pure unit
      Just drawingEdge ->
        for_ state.hoveredElements \hoveredElementId ->
          case drawingEdge.source, hoveredElementId of
            NodeSource sourceNodeId, NodeHaloId targetGraphId targetNodeId ->
              createEdgeBetweenNodes sourceNodeId drawingEdge.sourceGraph targetNodeId targetGraphId
            NodeSource sourceNodeId, NodeBorderId targetGraphId targetNodeId ->
              createEdgeBetweenNodes sourceNodeId drawingEdge.sourceGraph targetNodeId targetGraphId
            EdgeSource sourceEdgeId, EdgeHaloId (GraphComponent targetGraphId) targetEdgeId ->
              createEdgeBetweenEdges sourceEdgeId drawingEdge.sourceGraph targetEdgeId targetGraphId
            EdgeSource sourceEdgeId, EdgeBorderId (GraphComponent targetGraphId) targetEdgeId ->
              createEdgeBetweenEdges sourceEdgeId drawingEdge.sourceGraph targetEdgeId targetGraphId
            _, _ -> pure unit
    -- Remove the drawing edge
    H.modify_ $ _{ drawingEdges = Map.delete drawingEdgeId state.drawingEdges }
    H.unsubscribe subscriptionId

  AppCreateNode pane mouseEvent -> do
    newNodeId <- H.liftEffect genUUID
    state <- H.get
    let
      GraphSpacePoint2D newNodePosition = pageSpaceToGraphSpace pane $ mouseEventPosition mouseEvent
      newNode = (freshNode pane.graphId newNodeId) {positionX = newNodePosition.x, positionY = newNodePosition.y}
      op = StateUpdate [UpdateNodes [newNode {deleted = true}] [newNode]]
      target = GraphComponent pane.graphId
    applyMegagraphMutation_ op
    H.modify_ $ updateHistory (Insert op) target
    handleAction $ UpdateFocus $ Just $ NodeElement pane.graphId newNodeId

  AppDeleteNode node -> do
    state <- H.get
    case state.megagraph ^? _graph node.graphId of
      Nothing -> pure unit
      Just graph ->
        let
          edgesTouchingNode =
            edgeArray graph
            # Array.filter (\edge ->
                             edge.source == node.id || edge.target == node.id)
          --nodeMappingEdgesTouchingNode =
          --  state.megagraph.mappings
          --  # Map.values # Array.fromFoldable
          --  # Array.concatMap (\mapping -> mapping.nodeMappingEdges
          --                                 # Map.values # Array.fromFoldable)
          --  # Array.filter (\edge ->
          --                   edge.sourceNode == node.id || edge.targetNode == node.id)
          focus = case Array.head edgesTouchingNode of
            Just edge ->
              case edge.source == node.id, edge.target == node.id of
                true, false -> Just $ NodeElement edge.graphId edge.target
                false, true -> Just $ NodeElement edge.graphId edge.source
                _, _ -> Nothing
            Nothing -> Nothing
        in do
          --for_ edgesTouchingNode $ handleAction <<< AppDeleteEdge
          --for_ nodeMappingEdgesTouchingNode $ handleAction <<< AppDeleteNodeMappingEdge
          let
            op = StateUpdate $ [ UpdateNodes [node] [node {deleted = true}]
                               , UpdateEdges edgesTouchingNode (edgesTouchingNode <#> _{deleted = true})
                               ]
            target = GraphComponent node.graphId
          applyMegagraphMutation_ op
          H.modify_ $ updateHistory (Insert op) target
          --handleAction $ UnHover $ NodeBorderId node.graphId node.id
          --handleAction $ UnHover $ NodeHaloId node.graphId node.id
          -- UnHover everything
          H.modify_ $ _hoveredElements .~ Set.empty
          handleAction $ UpdateFocus focus

  AppCreateEdge edgeMetadata ->
    let
      midpoint = if edgeMetadata.source == edgeMetadata.target
                 then {angle: selfEdgeInitialAngle, radius: selfEdgeInitialRadius}
                 else {angle: 0.0, radius: 0.0}
      newEdge = (freshEdge edgeMetadata) {midpointAngle = midpoint.angle, midpointRadius = midpoint.radius}
      op = StateUpdate [UpdateEdges [newEdge {deleted = true}] [newEdge]]
      target = GraphComponent edgeMetadata.graphId
    in do
      applyMegagraphMutation_ op
      H.modify_ $ updateHistory (Insert op) target
      handleAction $ UpdateFocus $ Just $ EdgeElement edgeMetadata.graphId edgeMetadata.id

  AppDeleteEdge edge -> do
    state <- H.get
    let
      edgeMappingEdgesTouchingEdge =
        state.megagraph.mappings
        # Map.values # Array.fromFoldable
        # Array.concatMap (\mapping -> mapping.edgeMappingEdges
                                       # Map.values # Array.fromFoldable)
        # Array.filter (\edge' ->
                         edge'.sourceEdge == edge.id || edge'.targetEdge == edge.id)
      op = StateUpdate [UpdateEdges [edge] [edge {deleted = true}]]
      target = GraphComponent edge.graphId
    -- for_ edgeMappingEdgesTouchingEdge $ handleAction <<< AppDeleteEdgeMappingEdge
    applyMegagraphMutation_ op
    H.modify_ $ updateHistory (Insert op) target
    handleAction $ UnHover $ EdgeBorderId (GraphComponent edge.graphId) edge.id
    handleAction $ UnHover $ EdgeHaloId (GraphComponent edge.graphId) edge.id
    handleAction $ UpdateFocus $ Just $ NodeElement edge.graphId edge.source

  AppCreateNodeMappingEdge edgeId sourceNodeId sourceGraphId targetNodeId targetGraphId -> do
    state <- H.get
    mappingId <- createMappingIfNotExist state sourceGraphId targetGraphId
    let
      nodeMappingEdge = freshNodeMappingEdge edgeId mappingId sourceNodeId targetNodeId
      op = StateUpdate [UpdateNodeMappingEdges [nodeMappingEdge {deleted = true}] [nodeMappingEdge]]
      target = MappingComponent mappingId
    applyMegagraphMutation_ op
    H.modify_ $ updateHistory (Insert op) target
    handleAction $ UpdateFocusPane target

  AppDeleteNodeMappingEdge nodeMappingEdge ->
    let
      op = StateUpdate [UpdateNodeMappingEdges [nodeMappingEdge] [nodeMappingEdge {deleted = true}]]
      target = MappingComponent nodeMappingEdge.mappingId
    in do
    applyMegagraphMutation_ op
    H.modify_ $ updateHistory (Insert op) target
    handleAction $ UnHover $ EdgeBorderId target nodeMappingEdge.id
    handleAction $ UpdateFocusPane target

  AppCreateEdgeMappingEdge edgeId sourceEdgeId sourceGraphId targetEdgeId targetGraphId -> do
    state <- H.get
    mappingId <- createMappingIfNotExist state sourceGraphId targetGraphId
    let
      edgeMappingEdge = freshEdgeMappingEdge edgeId mappingId sourceEdgeId targetEdgeId
      op = StateUpdate [UpdateEdgeMappingEdges [edgeMappingEdge {deleted = true}] [edgeMappingEdge]]
      target = MappingComponent mappingId
    applyMegagraphMutation_ op
    H.modify_ $ updateHistory (Insert op) target
    handleAction $ UpdateFocusPane target

  AppDeleteEdgeMappingEdge edgeMappingEdge ->
    let
      op = StateUpdate [UpdateEdgeMappingEdges [edgeMappingEdge] [edgeMappingEdge {deleted = true}]]
      target = MappingComponent edgeMappingEdge.mappingId
    in do
    applyMegagraphMutation_ op
    H.modify_ $ updateHistory (Insert op) target
    handleAction $ UnHover $ EdgeBorderId (MappingComponent edgeMappingEdge.mappingId) edgeMappingEdge.id
    handleAction $ UpdateFocusPane target

  UpdateNodeSubgraph graphId nodeId -> do
    state <- H.get
    case state.megagraph ^? _graph graphId <<< _node nodeId of
      Nothing -> pure unit
      Just node ->
        let
          op = LinkNodeSubgraph node
          target = GraphComponent graphId
        in do
          H.liftEffect $ Console.log $ "doing: " <> show op
          applyMegagraphMutation_ op
          H.modify_ $ updateHistory (Insert op) target

  UpdateFocus newFocus -> do
    H.modify_ _{ focus = newFocus }
    _   <- case newFocus of
      Just (NodeElement _ nodeId)   -> H.query _nodeTextField nodeId $ H.tell SVGContentEditable.Focus
      Just (EdgeElement _ edgeId) -> H.query _edgeTextField edgeId $ H.tell SVGContentEditable.Focus
      _                         -> pure $ Just unit
    state <- H.get
    case newFocus of
      Just (NodeElement graphId _) -> handleAction $ UpdateFocusPane $ GraphComponent graphId
      Just (EdgeElement graphId _) -> handleAction $ UpdateFocusPane $ GraphComponent graphId
      Just (NodeMappingEdgeElement mappingId _) ->
        case state.megagraph ^? _mapping mappingId of
          Nothing -> pure unit
          Just mapping -> handleAction $ UpdateFocusPane $ MappingComponent mapping.id
      Just (EdgeMappingEdgeElement mappingId _) ->
        case state.megagraph ^? _mapping mappingId of
          Nothing -> pure unit
          Just mapping -> handleAction $ UpdateFocusPane $ MappingComponent mapping.id
      _ -> pure unit

  UpdateFocusPane megagraphElement ->
    H.modify_ _{ focusedPane = Just megagraphElement }

  DeleteFocus -> do
    state <- H.get
    let maybeAction = case state.focus of
          Just (NodeElement graphId nodeId) -> do
            node <- state.megagraph ^? _graph graphId <<< _node nodeId
            pure $ AppDeleteNode node
          Just (EdgeElement graphId edgeId) -> do
            edge <- state.megagraph ^? _graph graphId >>= lookupEdgeById edgeId
            pure $ AppDeleteEdge edge
          Just (NodeMappingEdgeElement mappingId nodeMappingEdgeId) -> do
            nodeMappingEdge <- state.megagraph ^? _mapping mappingId <<< _nodeMappingEdge nodeMappingEdgeId
            pure $ AppDeleteNodeMappingEdge nodeMappingEdge
          Just (EdgeMappingEdgeElement mappingId edgeMappingEdgeId) -> do
            edgeMappingEdge <- state.megagraph ^? _mapping mappingId <<< _edgeMappingEdge edgeMappingEdgeId
            pure $ AppDeleteEdgeMappingEdge edgeMappingEdge
          Nothing -> Nothing
    case maybeAction of
      Nothing -> pure unit
      Just action -> handleAction action

  Hover elementId -> do
    H.modify_ $ _hoveredElements %~ Set.insert elementId

  UnHover hoveredElementId -> do
    H.modify_ $ _hoveredElements %~ Set.delete hoveredElementId

  -- | Zoom in/out holding the mouse position invariant
  Zoom graphId wheelEvent -> do
    state <- H.get
    case state ^? _pane graphId of
      Nothing -> pure unit
      Just pane ->
        let
          scaledZoom = (WhE.deltaY wheelEvent) * zoomScaling
          newZoom = (Math.exp scaledZoom) * pane.zoom
          newPane = zoomAtPoint
                      newZoom
                      (mouseEventPosition $ WhE.toMouseEvent wheelEvent)
                      pane
        in
          H.modify_ $ _pane graphId .~ newPane

  CenterGraphOriginAndZoom -> do
    state <- H.get
    case state.focusedPane of
      Just (GraphComponent graphId) ->
        H.modify_ $ _pane graphId %~
          ((_origin .~ PageSpacePoint2D { x : 0.0, y : 0.0 })
           >>>
           (_zoom .~ 1.0))
      _ -> pure unit

  Undo (GraphComponent graphId) -> do
    state <- H.get
    case state.megagraphHistory.graphHistory ^? at graphId <<< traversed <<< _history >>= Array.uncons of
      Nothing -> pure unit
      Just {head, tail} -> do
        undoOp head (GraphComponent graphId)
        handleAction $ UpdateContentEditableText graphId

  Undo (MappingComponent mappingId) -> do
    state <- H.get
    case state.megagraphHistory.mappingHistory ^? at mappingId <<< traversed <<< _history >>= Array.uncons of
      Nothing -> pure unit
      Just {head, tail} -> undoOp head (MappingComponent mappingId)

  Redo (GraphComponent graphId) -> do
    state <- H.get
    case state.megagraphHistory.graphHistory ^? at graphId <<< traversed <<< _undone >>= Array.uncons of
      Nothing -> pure unit
      Just {head, tail} -> do
        redoOp head (GraphComponent graphId)
        handleAction $ UpdateContentEditableText graphId

  Redo (MappingComponent mappingId) -> do
    state <- H.get
    case state.megagraphHistory.mappingHistory ^? at mappingId <<< traversed <<< _undone >>= Array.uncons of
      Nothing -> pure unit
      Just {head, tail} -> redoOp head (MappingComponent mappingId)

  RemovePane graphId -> do
    -- Remove graph and any mappings incident on graph
    H.modify_ $ _megagraph %~
      (_graphs %~ Map.delete graphId)
      >>>
      (_mappings %~ Map.filter (not <<< mappingTouchesGraph))
    -- Remove graph and mappings in 'LiveMegagraph' which handles the state replication
    -- with the backend
    void $ H.query _liveMegagraph unit $ H.tell $ LiveMegagraph.Drop (GraphComponent graphId)
    state <- H.get
    for_ state.megagraph.mappings \mapping ->
      if mappingTouchesGraph mapping
      then
        void $ H.query _liveMegagraph unit $ H.tell $ LiveMegagraph.Drop (MappingComponent mapping.id)
      else pure unit
    H.modify_ $ _panes %~ Map.delete graphId
    _ <- H.modify $ _megagraphHistory <<< _graphHistory <<< at graphId .~ Nothing
    state' <- H.get
    for_ (state'.megagraphHistory.mappingHistory # Map.keys) (\mappingId ->
      H.modify_ $ _megagraphHistory <<< _mappingHistory <<< at mappingId .~ Nothing)
    -- Remove hovered elements from graph being removed
    for_ state'.hoveredElements \hoveredElementId -> case hoveredElementId of
      NodeHaloId graphId' _ ->
        if graphId' == graphId then handleAction (UnHover hoveredElementId) else pure unit
      NodeBorderId graphId' _ ->
        if graphId' == graphId then handleAction (UnHover hoveredElementId) else pure unit
      EdgeHaloId megagraphComponent _ ->
        case megagraphComponent of
          GraphComponent graphId' ->
            if graphId' == graphId then handleAction (UnHover hoveredElementId) else pure unit
          _ -> pure unit
      EdgeBorderId megagraphComponent _ ->
        pure unit
    H.modify_ arrangePanes
      where
        mappingTouchesGraph mapping =
          mapping.sourceGraph == graphId
          ||
          mapping.targetGraph == graphId

  FetchLocalFile changeEvent -> unit <$ runMaybeT do
    target <- MaybeT $ pure $ WE.target changeEvent
    inputElement <- MaybeT $ pure $ WHIE.fromEventTarget target
    files <- MaybeT $ H.liftEffect $ WHIE.files inputElement
    file <- MaybeT $ pure $ FileList.item 0 files
    fileReader <- lift $ H.liftEffect $ FileReader.fileReader
    lift $ H.liftEffect $ FileReader.readAsText (File.toBlob file) fileReader
    lift $ H.subscribe' \subscriptionId ->
      ES.effectEventSource \emitter -> do
        listener <- ET.eventListener \event -> do
          ES.emit emitter $ LoadLocalFile fileReader subscriptionId event
        ET.addEventListener WHET.load listener false $ FileReader.toEventTarget fileReader
        pure mempty

  LoadLocalFile fileReader subscriptionId _ -> do
    foreignJSON <- H.liftEffect $ FileReader.result fileReader
    case
      lmap (show <<< map renderForeignError)
      $ runExcept $ (Foreign.readString foreignJSON
                     >>= decodeJSON :: F WireDataRaw)
                     >>= decodeWireData
    of
      Left errors ->
        H.liftEffect $ Console.log $ "Failed to parse JSON: " <> errors
      Right wireData -> do
        H.liftEffect $ Console.log $ "Loading saved graph encoded with version "
                                     <> wireData.version
        state <- H.get
        H.modify_ $ applyMegagraphStateUpdates wireData.op
        H.modify_ arrangePanes
        -- Keep text fields in sync
        state' <- H.get
        for_ (state'.megagraph.graphs # Map.keys) \graphId ->
          handleAction $ UpdateContentEditableText graphId
    H.unsubscribe subscriptionId

  SaveLocalFile -> do
    state <- H.get
    rawWireDatas <- H.liftEffect $ encodeWireData $ encodeMegagraphAsMegagraphStateUpdates state.megagraph
    let
      titles = state ^.. _megagraph <<< _graphs <<< traversed <<< _title <<< _text
      title = joinWith "_" $ Array.fromFoldable titles
    H.liftEffect $ saveJSON (encodeJSON rawWireDatas) $ title <> ".graph.json"

  Keyup keyboardEvent -> handleKeyup keyboardEvent

  Keypress keyboardEvent -> handleKeypress keyboardEvent

  ConsoleLog str -> H.liftEffect $ Console.log str

  DoNothing ->
    pure unit

handleQuery :: forall a. Query a -> H.HalogenM AppState Action Slots Message Aff (Maybe a)
handleQuery = case _ of
  UpdateBoundingRect a -> Just a <$ runMaybeT do
    panesElement <- MaybeT $ H.getHTMLElementRef (H.RefLabel "panes")
    panesRect <- lift $ H.liftEffect $ WHE.getBoundingClientRect panesElement
    lift $ H.modify_ $ rescaleWindow (panesRect {height = panesRect.height - pendingIndicatorHeightPx})

createMappingIfNotExist :: AppState -> GraphId -> GraphId -> H.HalogenM AppState Action Slots Message Aff MappingId
createMappingIfNotExist state sourceGraphId targetGraphId =
  case state.megagraph.mappings
       # Map.values
       # List.filter (\mapping -> mapping.sourceGraph == sourceGraphId && mapping.targetGraph == targetGraphId)
       # List.head
  of
    Just mapping -> pure mapping.id
    Nothing -> do
      mappingId <- H.liftEffect UUID.genUUID
      applyMegagraphMutation_ $ StateUpdate [CreateMapping mappingId sourceGraphId targetGraphId ""]
      pure mappingId

applyReceivedMegagraphUpdate :: Array MegagraphStateUpdate -> H.HalogenM AppState Action Slots Message Aff Unit
applyReceivedMegagraphUpdate op = do
  state <- H.get
  H.modify_ $ applyMegagraphStateUpdates op
  for_ (updatedNodes op) (handleAction <<< tupleApply UpdateNodeContentEditableText)
  H.modify_ arrangePanes

undoOp :: MegagraphMutation -> MegagraphComponent -> H.HalogenM AppState Action Slots Message Aff Unit
undoOp megagraphMutation target =
  let
    reversedLastOp = invertMegagraphMutation megagraphMutation
    onSuccess = case reversedLastOp of
      UpdateNodeText node text ->
        case node.subgraph of
          Nothing -> DoNothing
          Just subgraphId -> UpdateContentEditableText subgraphId
      _ -> DoNothing
  in do
    applyMegagraphMutation reversedLastOp {onFail: ConsoleLog, onSuccess: onSuccess}
    H.modify_ $ updateHistory Pop target
    H.modify_ $ updateUndone (Insert megagraphMutation) target

redoOp :: MegagraphMutation -> MegagraphComponent -> H.HalogenM AppState Action Slots Message Aff Unit
redoOp megagraphMutation target =
  let
    onSuccess = case megagraphMutation of
      UpdateNodeText node text ->
        case node.subgraph of
          Nothing -> DoNothing
          Just subgraphId -> UpdateContentEditableText subgraphId
      _ -> DoNothing
  in do
    applyMegagraphMutation megagraphMutation {onFail: ConsoleLog, onSuccess: onSuccess}
    H.modify_ $ updateHistory (Insert megagraphMutation) target
    H.modify_ $ updateUndone Pop target

-- | Interpret the operation, send the operation to the backend,
-- | and register a callback to track when the backend has
-- | actioned the operation.
applyMegagraphMutation :: MegagraphMutation -> {onFail :: String -> Action, onSuccess :: Action} -> H.HalogenM AppState Action Slots Message Aff Unit
applyMegagraphMutation megagraphMutation callbacks =
  void $ H.query _liveMegagraph unit $ H.tell $ LiveMegagraph.Mutate megagraphMutation callbacks


applyMegagraphMutation_ :: MegagraphMutation -> H.HalogenM AppState Action Slots Message Aff Unit
applyMegagraphMutation_ megagraphMutation =
  applyMegagraphMutation megagraphMutation {onFail: ConsoleLog, onSuccess: DoNothing}

loadGraph :: GraphId -> H.HalogenM AppState Action Slots Message Aff Unit
loadGraph graphId = void $ H.query _liveMegagraph unit $ H.tell
                    $ LiveMegagraph.LoadGraph graphId {onFail: ConsoleLog, onSuccess: DoNothing}

loadGraphWithTitle :: String -> H.HalogenM AppState Action Slots Message Aff Unit
loadGraphWithTitle title = void $ H.query _liveMegagraph unit $ H.tell
                           $ LiveMegagraph.LoadGraphWithTitle title {onFail: ConsoleLog, onSuccess: DoNothing}


------
-- Key Commands

handleKeyup :: KE.KeyboardEvent -> H.HalogenM AppState Action Slots Message Aff Unit
handleKeyup keyboardEvent =
  case KE.key keyboardEvent of
    "Control" -> do
      state <- H.get
      H.modify_ $ _keyHoldState <<< _controlDown .~ false

    " " -> do
      state <- H.get
      H.modify_ $ _keyHoldState <<< _spaceDown .~ false

    _ -> pure unit

handleKeypress :: KE.KeyboardEvent -> H.HalogenM AppState Action Slots Message Aff Unit
handleKeypress keyboardEvent = do
  H.liftEffect $ Console.log $ "Keypress: " <> show (KE.key keyboardEvent)
  case KE.key keyboardEvent of
    "Control" -> do
      H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
      H.modify_ $ _keyHoldState <<< _controlDown .~ true
      state <- H.get
      if state.keyHoldState.spaceDown
      then handleAction BlurFocusedTextField
      else pure unit

    " " -> do
      H.modify_ $ _keyHoldState <<< _spaceDown .~ true
      state <- H.get
      if state.keyHoldState.controlDown
      then handleAction BlurFocusedTextField
      else pure unit

    -- Undo
    "z" -> do
      state <- H.get
      if isJust state.textFocused || not state.keyHoldState.controlDown
      then pure unit
      else do
        H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
        case state.focusedPane of
          Nothing -> pure unit
          Just megagraphElement -> handleAction $ Undo megagraphElement

    -- Redo
    "y" -> do
      state <- H.get
      if not state.keyHoldState.controlDown then pure unit else do
        H.liftEffect $ WE.preventDefault  $ KE.toEvent keyboardEvent
        case state.focusedPane of
          Nothing -> pure unit
          Just megagraphElement -> handleAction $ Redo megagraphElement

    "l" -> do
      state <- H.get
      if not state.keyHoldState.controlDown then pure unit else do
        if not state.keyHoldState.spaceDown
        then do
          -- Load saved graph from local JSON file
          H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
          H.liftEffect loadFile
        -- Link focused node to Subgraph with same title as node text.
        else do
          H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
          case state.focus of
            Just (NodeElement graphId nodeId) -> do
              handleAction $ UpdateNodeSubgraph graphId nodeId
            _ -> pure unit

    -- {ctrl} Save the current graph to a local file
    -- {ctrl+space} Connect the focused node to an existing graph which has
    --              the node text as its title.
    "s" -> do
      state <- H.get
      if not state.keyHoldState.controlDown
      then pure unit
      else
         if not state.keyHoldState.spaceDown
         -- Save current graph to local JSON file
         then do
           H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
           H.liftEffect $ Console.log "Saving current graphs to file"
           handleAction $ SaveLocalFile
         -- Select the current focused edge for the selected edge set
         else do
           H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
           case state.focus of
             Just (EdgeElement graphId edgeId) ->
               if Set.member (Tuple graphId edgeId) state.selectedEdges
               then
                 H.modify_ $ _selectedEdges %~ Set.delete (Tuple graphId edgeId)
               else
                 H.modify_ $ _selectedEdges %~ Set.insert (Tuple graphId edgeId)
             _ -> pure unit

    "o" -> do
      state <- H.get
      if not state.keyHoldState.controlDown then pure unit else do
        H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
        if not state.keyHoldState.spaceDown
        then do
          -- Return to graph origin and reset zoom
          handleAction CenterGraphOriginAndZoom
        else
          -- Open subgraph in new pane
          case state.focus of
            Just (NodeElement graphId nodeId) ->
              case state.megagraph ^? _graph graphId <<< _node nodeId <<< _subgraph <<< traversed of
                Nothing -> pure unit
                Just subgraphId -> loadGraph subgraphId
            _ -> pure unit

    -- Delete node/edge currently in focus
    "Delete" -> do
      state <- H.get
      case state.focusedPane of
        Nothing -> pure unit
        Just graphId -> handleAction DeleteFocus

    -- Unfocus
    "Escape" -> do
      state <- H.get
      for_ (Map.keys state.megagraph.graphs) (\graphId -> do
        handleAction $ UpdateFocus Nothing
        handleAction $ UpdateContentEditableText graphId)
      H.modify_ $ _selectedEdges .~ Set.empty

    -- Close pane
    "c" -> do
      state <- H.get
      if not state.keyHoldState.controlDown || not state.keyHoldState.spaceDown then pure unit else
        unit <$ runMaybeT do
          lift $ H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
          focusedElement <- MaybeT $ pure $ state.focusedPane
          case focusedElement of
            GraphComponent graphId -> lift $ handleAction $ RemovePane graphId
            _ -> pure unit

    -- new Graph
    "g" -> do
      state <- H.get
      if not state.keyHoldState.controlDown || not state.keyHoldState.spaceDown then pure unit else do
        H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
        newGraphId <- H.liftEffect genUUID
        let megagraphMutation = StateUpdate [CreateGraph newGraphId (UUID.toString newGraphId)]
        applyMegagraphMutation_ megagraphMutation
        H.modify_ arrangePanes

    -- jump Down into the subgraph of the focused node
    "d" -> do
      state <- H.get
      if not state.keyHoldState.controlDown || not state.keyHoldState.spaceDown then pure unit else do
        H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
        case state.focus of
          Just (NodeElement graphId nodeId) ->
            case state.megagraph ^? _graph graphId <<< _node nodeId <<< _subgraph <<< traversed of
              Nothing -> pure unit
              Just subgraphId -> do
                loadGraph subgraphId
                handleAction $ RemovePane graphId
          _ -> pure unit

    -- Jump Up to the graphs that have nodes that point to the current graph
    -- as a subgraph.
    "u" -> do
      state <- H.get
      if not state.keyHoldState.controlDown || not state.keyHoldState.spaceDown then pure unit else do
        H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
        case state.focusedPane of
          Just (GraphComponent graphId) -> do
            maybeNodesWithSubgraph <- H.query _liveMegagraph unit $ H.request
                                      $ LiveMegagraph.NodesWithSubgraph graphId {onFail: ConsoleLog, onSuccess: DoNothing}
            case maybeNodesWithSubgraph of
              Nothing -> pure unit
              Just nodesWithSubgraph -> do
                for_ nodesWithSubgraph \node -> loadGraph node.graphId
                handleAction $ RemovePane graphId
          _ -> pure unit

    -- load Home graph
    "h" -> do
      state <- H.get
      if not state.keyHoldState.controlDown || not state.keyHoldState.spaceDown then pure unit else do
        H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
        loadGraphWithTitle Config.homeGraphTitle

    -- create path Equation
    "e" -> do
      state <- H.get
      if not (state.keyHoldState.controlDown && state.keyHoldState.spaceDown) then pure unit else do
        H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
        -- Check that the selectedEdges set forms a valid path equation
        case Set.findMin state.selectedEdges <#> fst
             >>= flip Map.lookup state.megagraph.graphs
        of
          Nothing -> pure unit
          Just graph -> do
            case graph.pathEquations
                 # Map.filter (\pathEquation' ->
                                  not pathEquation'.deleted
                                  && Set.fromFoldable (pathEquation'.pathA <> pathEquation'.pathB) == Set.map snd state.selectedEdges)
                 # Map.values >>> List.head
            of
              Just selectedEquation ->
                let
                  op = StateUpdate [UpdatePathEquation selectedEquation (selectedEquation {deleted = not selectedEquation.deleted})]
                in do
                  applyMegagraphMutation_ op
                  H.modify_ $ updateHistory (Insert op) (GraphComponent graph.id)
                  H.modify_ $ _selectedEdges .~ Set.empty
              Nothing -> do
                newId <- H.liftEffect $ UUID.genUUID
                case edgeSetToPathEquation graph newId state.selectedEdges of
                  Nothing -> pure unit
                  Just newPathEquation ->
                    let
                      op = StateUpdate [UpdatePathEquation (newPathEquation {deleted = true}) newPathEquation]
                    in do
                      applyMegagraphMutation_ op
                      H.modify_ $ updateHistory (Insert op) (GraphComponent graph.id)
                      H.modify_ $ _selectedEdges .~ Set.empty

    -- don't refresh page if space is down (common mistake to hit 'r' and cause refresh)
    "r" -> do
      state <- H.get
      if not (state.keyHoldState.controlDown && state.keyHoldState.spaceDown) then pure unit else
        H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent

    _ -> pure unit
