module GraphComponent.HandleAction where

import Prelude

import AppOperation (AppOperation(..), HistoryUpdate(..), MegagraphElement(..), encodeMegagraphStateAsAppOperations)
import AppOperation.Utils (removeEdgeMappingEdgesOp, removeEdgeOp, removeNodeMappingEdgesOp, removeNodeOp)
import AppState (AppState, _drawingEdgePosition, _drawingEdgeTargetGraph, _drawingEdges, _graph, _graphAtId, _graphs, _mappings, _megagraph, _paneAtId)
import ContentEditable.SVGComponent as SVGContentEditable
import Control.Alt ((<|>))
import Control.Monad.Except (runExcept)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT, lift)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Lens ((%~), (.~), (^?), (^..), traversed)
import Data.Lens.At (at)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.Traversable (sequence)
import Data.UUID (genUUID)
import Data.UUID as UUID
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Console as Console
import Foreign (F, renderForeignError)
import Foreign as Foreign
import Foreign.Generic (encodeJSON, decodeJSON)
import GraphComponent.Types (Action(..), Message(..), Query(..), Slots, _edgeTextField, _nodeTextField, _titleTextField)
import GraphComponent.Utils (drawingEdgeWithinNodeHalo, mouseEventPosition, nodesWithTextUpdate)
import Halogen as H
import Halogen.Component.Utils.Drag as Drag
import Halogen.Query.EventSource as ES
import Interpreter (interpretAppOperation)
import Math as Math
import Megagraph (Focus(..), GraphId, GraphSpacePoint2D(..), PageSpacePoint2D(..), _edge, _focus, _nodes, _origin, _position, _text, _title, _zoom, edgeArray, edgeSpaceToGraphSpace, graphSpaceToEdgeSpace, graphSpaceToPageSpace, lookupEdgeById, pageSpaceToGraphSpace)
import MegagraphOperation (GraphOperation(..), MegagraphOperation(..), invertMegagraphUpdate)
import UI.Constants (zoomScaling)
import UI.Panes (arrangePanes, insertPane, paneContainingPoint, rescaleWindow, zoomAtPoint)
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
  PreventDefault e next -> do
    H.liftEffect $ WE.preventDefault e
    handleAction next

  StopPropagation e next -> do
    H.liftEffect $ WE.stopPropagation e
    handleAction next

  EvalQuery query -> do
    _ <- handleQuery query
    pure unit

  Init -> do
    -- Initialize bounding rectangle
    _ <- handleQuery $ UpdateBoundingRect (const unit)
    -- Subscribe to updates in bounding rectangle
    window <- H.liftEffect $ WH.window
    _ <- H.subscribe $ ES.eventListenerEventSource
                       (WE.EventType "resize")
                       (WHW.toEventTarget window)
                       \event -> Just $ EvalQuery $ UpdateBoundingRect unit
    -- Add keydown event listener to body
    document <- H.liftEffect $ WHW.document =<< WH.window
    _ <- H.subscribe $ ES.eventListenerEventSource KET.keydown (HTMLDocument.toEventTarget document) (map Keypress <<< KE.fromEvent)
    -- keyup listener
    _ <- H.subscribe $ ES.eventListenerEventSource KET.keyup   (HTMLDocument.toEventTarget document) (map Keyup <<< KE.fromEvent)

    -- TODO remove when backend hooked up again
    initialGraphId <- H.liftEffect UUID.genUUID
    H.modify_ $ insertPane initialGraphId

    pure unit

  UpdateContentEditableText graphId -> do
    state <- H.get
    case Map.lookup graphId state.megagraph.graphs <#> _.graph of
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
    case Map.lookup graphId state.megagraph.graphs >>= (_.graph.nodes >>> Map.lookup nodeId) of
      Nothing -> pure unit
      Just node -> do
        _ <- H.query _nodeTextField node.id $ H.tell $ SVGContentEditable.SetText node.text
        pure unit

  NodeTextInput graphId nodeId (SVGContentEditable.TextUpdate text) -> do
    state <- H.get
    case state ^? _graphAtId graphId <<< _nodes <<< at nodeId <<< traversed of
      Nothing -> pure unit
      Just node ->
        let
          appOp = undoableGraphOp node.graphId $ UpdateNodeText nodeId node.text text
        in do
          interpretAndSend appOp
          handleAction $ FocusOn node.graphId $ Just $ FocusNode node.id

  EdgeTextInput graphId edgeId (SVGContentEditable.TextUpdate text) -> do
    state <- H.get
    case lookupEdgeById edgeId =<< (state ^? _graphAtId graphId) of
      Nothing -> pure unit
      Just edge ->
        let
          appOp = undoableGraphOp graphId $ UpdateEdgeText edge.id edge.text text
        in do
          interpretAndSend appOp
          handleAction $ FocusOn graphId $ Just $ FocusEdge edgeId []

  TitleTextInput graphId (SVGContentEditable.TextUpdate newTitleText) -> do
    state <- H.get
    case state ^? _graphAtId graphId <<< _title of
      Nothing -> pure unit
      Just oldTitle ->
        let
          appOp = undoableGraphOp graphId $ UpdateTitle oldTitle.text newTitleText
        in
          interpretAndSend appOp

  BackgroundDragStart graphId initialGraphOrigin mouseEvent -> do
    handleAction $ FocusOn graphId Nothing
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
    H.modify_ $ _paneAtId graphId <<< _origin .~ newGraphOrigin

  BackgroundDragMove (Drag.Done _) _ _ subscriptionId ->
    H.unsubscribe subscriptionId

  NodeDragStart graphId nodeId initialNodePos mouseEvent -> do
    H.subscribe' \subscriptionId ->
      Drag.dragEventSource mouseEvent
        \e -> NodeDragMove e graphId nodeId initialNodePos subscriptionId
    handleAction $ FocusOn graphId $ Just $ FocusNode nodeId

  NodeDragMove (Drag.Move _ dragData) graphId nodeId (GraphSpacePoint2D initialNodePos) _ -> do
    state <- H.get
    case state ^? _paneAtId graphId of
      Nothing -> pure unit
      Just pane -> do
        let
          dragOffsetGraphSpace = { x : dragData.offsetX * pane.zoom
                                 , y : dragData.offsetY * pane.zoom
                                 }
          newNodePos =
            GraphSpacePoint2D
              { x : initialNodePos.x + dragOffsetGraphSpace.x
              , y : initialNodePos.y + dragOffsetGraphSpace.y
              }
        case state ^? _graphAtId graphId <<< _nodes <<< at nodeId <<< traversed of
          Nothing -> pure unit
          Just node ->
            let
              appOp = undoableGraphOp graphId $ MoveNode node.id node.position newNodePos
            in
              interpretAndSend appOp

  NodeDragMove (Drag.Done _) _ _ _ subscriptionId ->
    H.unsubscribe subscriptionId

  EdgeDragStart graphId edgeId initialMidpoint mouseEvent -> do
    H.subscribe' \subscriptionId ->
      Drag.dragEventSource mouseEvent
        \e -> EdgeDragMove e graphId edgeId initialMidpoint subscriptionId
    handleAction $ FocusOn graphId $ Just $ FocusEdge edgeId []

  EdgeDragMove (Drag.Move _ dragData) graphId edgeId initialMidpoint _ -> do
    state <- H.get
    case do
      graph <- state ^? _graphAtId graphId
      edge <- lookupEdgeById edgeId graph
      sourcePos <- graph ^? _position edge.source
      targetPos <- graph ^? _position edge.target
      pane <- state ^? _paneAtId graphId
      let
        dragOffsetGraphSpace = { x : dragData.offsetX * pane.zoom
                               , y : dragData.offsetY * pane.zoom
                               }
        GraphSpacePoint2D initialMidpointGraphSpace = edgeSpaceToGraphSpace sourcePos targetPos initialMidpoint
        newMidpointGraphSpace =
          GraphSpacePoint2D
            { x : initialMidpointGraphSpace.x + dragOffsetGraphSpace.x
            , y : initialMidpointGraphSpace.y + dragOffsetGraphSpace.y
            }
        newMidpoint = graphSpaceToEdgeSpace sourcePos targetPos newMidpointGraphSpace
      pure $ undoableGraphOp graphId $ MoveEdgeMidpoint edge.id edge.midpoint newMidpoint
    of
      Nothing -> pure unit
      Just appOp ->
        interpretAndSend appOp

  EdgeDragMove (Drag.Done _) _ _ _ subscriptionId ->
    H.unsubscribe subscriptionId

  EdgeDrawStart pane drawingEdgeId mouseEvent -> do
    state <- H.get
    case state ^? _graphAtId pane.graphId <<< _nodes <<< at drawingEdgeId <<< traversed of
      Nothing -> pure unit
      Just source -> do
        let
          mousePosition = mouseEventPosition mouseEvent
          sourcePosition = source.position # graphSpaceToPageSpace pane
        H.modify_ $ (_drawingEdges %~ Map.insert drawingEdgeId { sourcePosition : sourcePosition
                                                               , source         : drawingEdgeId
                                                               , sourceGraph    : pane.graphId
                                                               , pointPosition  : mousePosition
                                                               , targetGraph    : pane.graphId
                                                               })
        handleAction $ FocusOn pane.graphId $ Just $ FocusNode drawingEdgeId
        H.subscribe' \subscriptionId ->
          Drag.dragEventSource mouseEvent $ \e -> EdgeDrawMove e pane.graphId drawingEdgeId subscriptionId

  -- | Check which pane the point of the drawing edge is in and update
  -- | the drawingEdgeState so it can be drawn properly in both panes
  EdgeDrawMove (Drag.Move _ dragData) graphId drawingEdgeId _ -> do
    state <- H.get
    let
      panes = _.view <$> Array.fromFoldable (Map.values state.megagraph.graphs)
      edgePageTargetPosition = PageSpacePoint2D { x : dragData.x, y : dragData.y }
    case paneContainingPoint panes edgePageTargetPosition <|> state ^? _paneAtId graphId of
      Nothing -> pure unit
      Just targetPane -> do
        H.modify_ $ (_drawingEdgePosition drawingEdgeId .~ edgePageTargetPosition)
                    >>> (_drawingEdgeTargetGraph drawingEdgeId .~ targetPane.graphId)

  EdgeDrawMove (Drag.Done _) graphId drawingEdgeId subscriptionId -> do
    -- Create a new edge if the drawn edge is dropped within the
    -- halo of another node.
    state <- H.get
    newEdgeId <- H.liftEffect UUID.genUUID
    let
      maybeNewEdgeMetadata = do
        drawingEdgeState <- Map.lookup drawingEdgeId state.drawingEdges
        targetPane <- state ^? _paneAtId drawingEdgeState.targetGraph
        targetGraph <- state ^? _graphAtId drawingEdgeState.targetGraph
        drawingEdgeTarget <-
          Map.values targetGraph.nodes
            # List.filter (\node -> node.id /= drawingEdgeId)
            # List.filter (drawingEdgeWithinNodeHalo drawingEdgeState targetPane)
            # List.head
        pure { id : newEdgeId
             , graphId : drawingEdgeTarget.graphId
             , source      : drawingEdgeState.source
             , target      : drawingEdgeTarget.id
             }
    -- Remove the drawing edge
    H.modify_ $ _{ drawingEdges = Map.delete drawingEdgeId state.drawingEdges }
    H.unsubscribe subscriptionId
    case maybeNewEdgeMetadata of
      Nothing -> pure unit
      Just newEdgeMetadata -> do
        handleAction $ AppCreateEdge graphId newEdgeMetadata

  AppCreateNode pane mouseEvent -> do
    newNodeId <- H.liftEffect genUUID
    state <- H.get
    let
      newNodePosition = pageSpaceToGraphSpace pane $ mouseEventPosition mouseEvent
      op = GraphElementOperation pane.graphId
           <$> [ InsertNode newNodeId
               , MoveNode newNodeId (GraphSpacePoint2D {x: 0.0, y: 0.0}) newNodePosition
               ]
      appOp = AppOperation { target : GraphElement pane.graphId
                           , op : op
                           , historyUpdate : Insert op
                           , undoneUpdate : NoOp
                           }
    interpretAndSend appOp
    handleAction $ FocusOn pane.graphId $ Just $ FocusNode newNodeId

  AppDeleteNode node -> do
    state <- H.get
    case Map.lookup node.graphId state.megagraph.graphs <#> _.graph of
      Nothing -> pure unit
      Just graph ->
        let
          allEdges = edgeArray graph
          focus = case Array.head allEdges of
            Just edge ->
              case edge.source == node.id, edge.target == node.id of
                true, false -> Just $ FocusNode edge.target
                false, true -> Just $ FocusNode edge.source
                _, _ -> Nothing
            Nothing -> Nothing
        in do
          handleAction $ FocusOn node.graphId focus
          state' <- H.get
          let
            removeEdgesOp = Array.concatMap (removeEdgeMappingEdgesOp state') allEdges
                         <> Array.concatMap removeEdgeOp allEdges
            op = removeEdgesOp <> removeNodeMappingEdgesOp state node <> removeNodeOp node
            appOp = AppOperation { target : GraphElement node.graphId
                                 , op : op
                                 , historyUpdate : Insert op
                                 , undoneUpdate : NoOp
                                 }
          interpretAndSend appOp

  AppCreateEdge graphId edgeMetadata ->
    let
      appOp = undoableGraphOp graphId
              $ InsertEdge edgeMetadata
    in do
      interpretAndSend appOp
      handleAction $ FocusOn graphId $ Just $ FocusEdge edgeMetadata.id []

  AppDeleteEdge graphId edge -> do
    state <- H.get
    let
      op = removeEdgeMappingEdgesOp state edge <> removeEdgeOp edge
      appOp = AppOperation { target : GraphElement graphId
                           , op : op
                           , historyUpdate : Insert op
                           , undoneUpdate : NoOp
                           }
    interpretAndSend appOp
    handleAction $ FocusOn edge.graphId $ Just $ FocusNode edge.source

  FocusOn graphId newFocus -> do
    H.modify_
      $ (_paneAtId graphId <<< _focus .~ newFocus)
        >>> _{ focusedPane = Just graphId }
    _   <- case newFocus of
      Just (FocusNode nodeId)   -> H.query _nodeTextField nodeId $ H.tell SVGContentEditable.Focus
      Just (FocusEdge edgeId _) -> H.query _edgeTextField edgeId $ H.tell SVGContentEditable.Focus
      _                         -> pure $ Just unit
    pure unit

  DeleteFocus graphId -> do
    state <- H.get
    case Map.lookup graphId state.megagraph.graphs >>= _.view.focus of
      Nothing -> pure unit
      Just (FocusNode nodeId) ->
        case state ^? _graphAtId graphId <<< _nodes <<< at nodeId <<< traversed of
          Nothing -> pure unit
          Just node -> handleAction $ AppDeleteNode node
      Just (FocusEdge edgeId _) ->
        case state ^? _graphAtId graphId <<< _edge edgeId <<< traversed of
          Nothing -> pure unit
          Just edge -> handleAction $ AppDeleteEdge graphId edge

  Hover maybeElementId -> do
    H.modify_ $ _{ hoveredElementId = maybeElementId }

  -- | Zoom in/out holding the mouse position invariant
  Zoom graphId wheelEvent -> do
    state <- H.get
    case state ^? _paneAtId graphId of
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
          H.modify_ $ _paneAtId graphId .~ newPane

  CenterGraphOriginAndZoom -> do
    state <- H.get
    case state.focusedPane of
      Nothing -> pure unit
      Just graphId ->
        H.modify_ $ _paneAtId graphId %~
          ((_origin .~ PageSpacePoint2D { x : 0.0, y : 0.0 })
           >>>
           (_zoom .~ 1.0))

  Undo graphId -> do
    state <- H.get
    case Map.lookup graphId state.megagraph.graphs
         >>= (_.history >>> Array.uncons) of
      Nothing -> pure unit
      Just {head, tail} ->
        let
          reversedLastOp = invertMegagraphUpdate head
          appOp = AppOperation { target : GraphElement graphId
                               , op : reversedLastOp
                               , historyUpdate : Pop
                               , undoneUpdate : Insert head
                               }
        in do
          interpretAndSend appOp
          handleAction $ UpdateContentEditableText graphId

  Redo graphId -> do
    state <- H.get
    case Map.lookup graphId state.megagraph.graphs
         >>= (_.undone >>> Array.uncons) of
      Nothing -> pure unit
      Just {head, tail} ->
        let
          appOp = AppOperation { target : GraphElement graphId
                               , op : head
                               , historyUpdate : Insert head
                               , undoneUpdate : Pop
                               }
        in do
          interpretAndSend appOp
          handleAction $ UpdateContentEditableText graphId

  RemovePane graphId -> do
    H.modify_ $ _megagraph %~
      (_graphs %~ Map.delete graphId)
      >>>
      (_mappings %~ Map.filter (_.mapping >>> mappingTouchesGraph))
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

  LoadLocalFile fileReader subscriptionId event -> do
    -- state <- H.get
    foreignJSON <- H.liftEffect $ FileReader.result fileReader
    case
      lmap (show <<< map renderForeignError)
      $ runExcept $ (Foreign.readString foreignJSON
                     >>= decodeJSON :: F (Array WireDataRaw))
                     >>= (sequence <<< map decodeWireData)
    of
      Left errors ->
        H.liftEffect $ Console.log $ "Failed to parse JSON: " <> errors
      Right wireDatas -> for_ wireDatas \wireData -> do
        H.liftEffect $ Console.log $ "Loading saved graph encoded with version "
                                     <> wireData.metadata.version
        H.modify_ $ _megagraph %~ interpretAppOperation wireData.op
        H.modify_ arrangePanes
        -- Keep text fields in sync
        let AppOperation appOp = wireData.op
        case appOp.target of
          GraphElement graphId -> handleAction $ UpdateContentEditableText graphId
          _ -> pure unit
    H.unsubscribe subscriptionId

  SaveLocalFile -> do
    state <- H.get
    rawWireDatas <- H.liftEffect $ sequence $ encodeWireData <$> encodeMegagraphStateAsAppOperations state.megagraph
    let
      titles = state ^.. _megagraph <<< _graphs <<< traversed <<< _graph <<< _title <<< _text
      title = joinWith "_" $ Array.fromFoldable titles
    H.liftEffect $ saveJSON (encodeJSON rawWireDatas) $ title <> ".graph.json"

  Keyup keyboardEvent -> handleKeyup keyboardEvent

  Keypress keyboardEvent -> handleKeypress keyboardEvent

  DoNothing ->
    pure unit

handleQuery :: forall a. Query a -> H.HalogenM AppState Action Slots Message Aff (Maybe a)
handleQuery = case _ of
  UpdateBoundingRect a -> do
    map (const a) <$> runMaybeT do
      panesElement <- MaybeT $ H.getHTMLElementRef (H.RefLabel "panes")
      panesRect <- lift $ H.liftEffect $ WHE.getBoundingClientRect panesElement
      lift $ H.modify_ $ rescaleWindow panesRect

  ReceiveOperation op a -> do
    state <- H.get
    H.modify_ $ _megagraph %~ interpretAppOperation op
    for_ (nodesWithTextUpdate op) (handleAction <<< tupleApply UpdateNodeContentEditableText)
    pure $ Just a

undoableGraphOp :: GraphId -> GraphOperation -> AppOperation
undoableGraphOp graphId graphOp =
  let
    op = [GraphElementOperation graphId graphOp]
  in
    AppOperation { target : GraphElement graphId
                 , op : op
                 , historyUpdate : Insert op
                 , undoneUpdate : NoOp
                 }

interpretAndSend :: AppOperation -> H.HalogenM AppState Action Slots Message Aff Unit
interpretAndSend op = do
  H.raise $ SendOperation op
  H.modify_ $ _megagraph %~ interpretAppOperation op


------
-- Key Commands

handleKeyup :: KE.KeyboardEvent -> H.HalogenM AppState Action Slots Message Aff Unit
handleKeyup keyboardEvent =
      case KE.key keyboardEvent of
        " " -> do
          state <- H.get
          H.modify_ _{ keyHoldState = state.keyHoldState { spaceDown = false} }

        _ -> pure unit

handleKeypress :: KE.KeyboardEvent -> H.HalogenM AppState Action Slots Message Aff Unit
handleKeypress keyboardEvent = do
      H.liftEffect $ Console.log $ show $ KE.key keyboardEvent
      case KE.key keyboardEvent of
        " " -> if not (KE.ctrlKey keyboardEvent) then pure unit else do
          H.liftEffect $ WE.preventDefault  $ KE.toEvent keyboardEvent
          state <- H.get
          H.modify_ _{ keyHoldState = state.keyHoldState { spaceDown = true } }

        -- Undo
        "z" -> if not (KE.ctrlKey keyboardEvent) then pure unit else do
          H.liftEffect $ WE.preventDefault  $ KE.toEvent keyboardEvent
          state <- H.get
          case state.focusedPane of
            Nothing -> pure unit
            Just graphId -> handleAction $ Undo graphId

        -- Redo
        "y" -> if not (KE.ctrlKey keyboardEvent) then pure unit else do
          H.liftEffect $ WE.preventDefault  $ KE.toEvent keyboardEvent
          state <- H.get
          case state.focusedPane of
            Nothing -> pure unit
            Just graphId -> handleAction $ Redo graphId

        "l" -> if not (KE.ctrlKey keyboardEvent) then pure unit else do
          H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
          -- Load saved graph from local JSON file
          H.liftEffect loadFile

        "s" -> if not (KE.ctrlKey keyboardEvent) then pure unit else do
          H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
          state <- H.get
          if not state.keyHoldState.spaceDown
          then
            -- Save current graph to local JSON file
            handleAction $ SaveLocalFile
          else
            -- connect focused node to Subgraph with same title as node text
            -- TODO
            pure unit
            --case focusNode state of
            --  Nothing -> pure unit
            --  Just node ->
            --    let
            --      op = AppOperation node.graphId $ connectSubgraphIfTitleExists node.id node.text
            --    in
            --      H.raise $ SendOperation op

        "o" -> if not (KE.ctrlKey keyboardEvent) then pure unit else do
          H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
          state <- H.get
          if not state.keyHoldState.spaceDown
          then do
            -- Return to graph origin and reset zoom
            handleAction CenterGraphOriginAndZoom
          else
            -- Open subgraph in new pane
            -- TODO
            pure unit
            --if state.keyHoldState.spaceDown
            --then unit <$ runMaybeT do
            --  subgraphId <- MaybeT $ pure $ focusNodeSubgraph state
            --  lift $ H.raise $ SendOperation $ AppOperation subgraphId $ insertPane subgraphId
            --else
            --  pure unit

        -- Delete node/edge currently in focus
        "Delete" -> do
          state <- H.get
          case state.focusedPane of
            Nothing -> pure unit
            Just graphId -> handleAction $ DeleteFocus graphId

        -- Unfocus
        "Escape" -> do
          state <- H.get
          for_ (Map.keys state.megagraph.graphs) \graphId -> do
            handleAction $ FocusOn graphId Nothing
            handleAction $ UpdateContentEditableText graphId

        -- Close pane
        "c" -> if not (KE.ctrlKey keyboardEvent) then pure unit else do
          state <- H.get
          if not state.keyHoldState.spaceDown
          then
            pure unit
          else unit <$ runMaybeT do
            lift $ H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
            graphId <- MaybeT $ pure $ state.focusedPane
            lift $ handleAction $ RemovePane graphId

        -- new Graph
        -- TODO: if current selection is non-empty, push it down into the new subgraph
        "g" -> if not (KE.ctrlKey keyboardEvent) then pure unit else do
          state <- H.get
          if not state.keyHoldState.spaceDown
          then
            pure unit
          else
            -- TODO
            pure unit
            --case Tuple (focusNode state) (focusNode state >>= _.subgraph) of
            --  Tuple (Just node) Nothing -> do
            --    -- create new graph as a subgraph of focused node
            --    H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
            --    newGraphId <- H.liftEffect genUUID
            --    -- perform these ops separately so that connectSubraph is undoable
            --    H.raise $ SendOperation $ AppOperation node.graphId $ createGraph newGraphId node.text
            --    H.raise $ SendOperation $ AppOperation node.graphId $ connectSubgraph node (Just newGraphId)
            --  _ -> do
            --    -- create new graph in new pane
            --    H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
            --    newGraphId <- H.liftEffect genUUID
            --    let op = AppOperation newGraphId do
            --          insertPane newGraphId
            --          updateTitle newGraphId "" $ UUID.toString newGraphId
            --    interpretAndSend op

        -- TODO: highlighting
        ---- Highlight currently focused node/edge
        --"h" -> H.modify_ $ _graphAtId %~ toggleHighlightFocus

        -- jump Down into the subgraph of the focused node
        "d" -> if not (KE.ctrlKey keyboardEvent) then pure unit else do
          state <- H.get
          if not state.keyHoldState.spaceDown
          then
            pure unit
          else
            -- TODO
            pure unit
            --case Tuple state.focusedPane (focusNodeSubgraph state) of
            --  Tuple (Just focusedGraphId) (Just subgraphId) ->
            --    let
            --      op = AppOperation focusedGraphId do
            --        removePane focusedGraphId
            --        insertPane subgraphId
            --    in do
            --      H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
            --      interpretAndSend op
            --  _ -> pure unit

        -- jump Up to the graphs that have nodes that point to the current graph
        -- as a subgraph
        "u" -> if not (KE.ctrlKey keyboardEvent) then pure unit else do
          state <- H.get
          if not state.keyHoldState.spaceDown
          then
            pure unit
          else do
            -- TODO
            pure unit
            --H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
            --unit <$ runMaybeT do
            --  focusedGraphId <- MaybeT $ pure state.focusedPane
            --  lift $ interpretAndSend $ AppOperation focusedGraphId $ openGraphsWithSubgraph focusedGraphId

        -- load the Knowledge navigator
        "k" -> if not (KE.ctrlKey keyboardEvent) then pure unit else do
          state <- H.get
          if not state.keyHoldState.spaceDown
          then
            pure unit
          else do
            -- TODO
            pure unit
            --H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
            --H.raise $ SendOperation $ AppOperation config.knowledgeNavigatorId $
            --  insertPane config.knowledgeNavigatorId

        _ -> pure unit
