module GraphComponent where

import Prelude

import AppState (AppState(..), DrawingEdge, DrawingEdgeId, GraphElementId(..), GraphId, GraphSpacePos(..), PageSpacePos(..), Shape, _AppState, _drawingEdgePos, _drawingEdges, _graph, _graphNodePos, _graphOrigin, _textFieldShapes, _zoom, drawingEdgeKey, edgeIdStr, toGraphSpace, appStateVersion)
import AppState.JSON (appStateFromJSON, appStateToJSON)
import ContentEditable.Component as ContentEditable
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT, lift)
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (for_, traverse_)
import Data.Identity (Identity(..))
import Data.Int (toNumber)
import Data.Lens (preview, traversed, (%~), (.~), (?~), (^.))
import Data.Lens.At (at)
import Data.List as List
import Data.Map as Map
import Data.Set as Set
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Console (log)
import Effect.Now (now)
import Foreign (renderForeignError)
import Foreign as Foreign
import Halogen as H
import Halogen.Component.Utils.Drag as Drag
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import Math as Math
import Svg.Attributes as SA
import Svg.Elements as SE
import Svg.Elements.Keyed as SK
import Web.Event.Event as WE
import Web.Event.EventTarget as ET
import Web.File.File as File
import Web.File.FileList as FileList
import Web.File.FileReader as FileReader
import Web.HTML as WH
import Web.HTML.Event.EventTypes as WHET
import Web.HTML.HTMLDocument (HTMLDocument)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLInputElement as WHIE
import Web.HTML.Window as WHW
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET
import Web.UIEvent.MouseEvent as ME
import Web.UIEvent.WheelEvent as WhE
import Workflow.Core (NodeId, EdgeId, _id, _subgraph, _nodes, _source, _target, _edgeId, allEdges, deleteNode, insertEdge, deleteEdgeId, insertNode, lookupParents, lookupChildren)
import Workflow.UIGraph (UIGraph, UINode, UIEdge, Focus(..), Point2D, _pos, _text, _focus, freshUIEdge, freshUINode, graphTitle, uiGraphVersion)

foreign import loadFile :: Effect Unit
foreign import saveJSON :: String -> String -> Effect Unit

nodeRadius :: Number
nodeRadius = 7.0

groupNodeRadius :: Number
groupNodeRadius = 12.0

nodeBorderRadius :: Number
nodeBorderRadius = 28.0

haloRadius :: Number
haloRadius = 40.0

textBoxOffset :: Point2D
textBoxOffset = { x : 20.0, y : - 10.0 }

zoomScaling :: Number
zoomScaling = 0.01

defaultTextFieldShape :: Shape
defaultTextFieldShape = { width : 100.0, height : 50.0 }

maxTextFieldShape :: Shape
maxTextFieldShape = { width : 700.0, height : 500.0 }

initialState :: { graphId :: GraphId, windowSize :: Shape, graph :: UIGraph } -> AppState
initialState inputs = AppState
  { graph : inputs.graph
  , textFieldShapes : (\_ -> defaultTextFieldShape) <$> inputs.graph ^. _nodes
  , drawingEdges : Map.empty
  , hoveredElementId : Nothing
  , windowSize : inputs.windowSize
  , graphOrigin : PageSpacePos { x : 0.0, y : 0.0 }
  , zoom : 1.0
  , graphId : inputs.graphId
  }

data Query a =
    PreventDefault WE.Event (Query a)
  | StopPropagation WE.Event (Query a)
  | Init a
  | ResizeWindow Shape a
  | BackgroundDragStart PageSpacePos ME.MouseEvent a
  | BackgroundDragMove Drag.DragEvent PageSpacePos a
  | NodeDragStart NodeId GraphSpacePos ME.MouseEvent a
  | NodeDragMove Drag.DragEvent NodeId GraphSpacePos a
  | EdgeDrawStart DrawingEdgeId ME.MouseEvent a
  | EdgeDrawMove Drag.DragEvent DrawingEdgeId a
  | TextInput NodeId ContentEditable.Message a
  | CreateNode ME.MouseEvent a
  | DeleteNode NodeId a
  | CreateEdge EdgeId a
  | DeleteEdge EdgeId a
  | FocusOn Focus a
  | DeleteFocus a
  | Hover (Maybe GraphElementId) a
  | Zoom WhE.WheelEvent a
  | CenterGraphOrigin a
  | FetchLocalFile WE.Event a
  | LoadLocalFile FileReader.FileReader WE.Event (H.SubscribeStatus -> a)
  | SaveLocalFile a
  | Keypress KE.KeyboardEvent (H.SubscribeStatus -> a)
  | DoNothing a

data Message = Message Unit

type Input = { graphId :: GraphId, windowSize :: Shape, graph :: UIGraph }

data Slot = TextField NodeId
derive instance eqTextFieldSlot :: Eq Slot
derive instance ordTextFieldSlot :: Ord Slot

graph :: H.Component HH.HTML Query Input Message Aff
graph =
  H.lifecycleParentComponent
    { initialState : initialState
    , render : render
    , eval : eval
    , receiver : HE.input ResizeWindow <<< _.windowSize
    , initializer : Just $ H.action Init
    , finalizer : Nothing
    }
  where

  svgDefs :: H.ParentHTML Query ContentEditable.Query Slot Aff
  svgDefs = SE.defs
            [ SE.marker
              [ SA.id "drawing-arrow"
              , SA.markerWidth 10.0
              , SA.markerHeight 10.0
              , SA.refX $ SA.Offset 3.0
              , SA.refY $ SA.Offset 5.0
              , SA.orient SA.OrientAuto
              , SA.markerUnits SA.UserSpaceOnUse
              ]
              [ SE.path [ SA.d [ SA.Abs (SA.M 0.0 0.0)
                               , SA.Abs (SA.L 10.0 5.0)
                               , SA.Abs (SA.L 0.0 10.0)
                               , SA.Abs SA.Z
                               ]
                        ]
              ]
            , SE.marker
              [ SA.id "arrow"
              , SA.markerWidth 10.0
              , SA.markerHeight 10.0
              , SA.refX $ SA.Offset 15.0
              , SA.refY $ SA.Offset 5.0
              , SA.orient SA.OrientAuto
              , SA.markerUnits SA.UserSpaceOnUse
              ]
              [ SE.path [ SA.d [ SA.Abs (SA.M 0.0 0.0)
                               , SA.Abs (SA.L 0.0 10.0)
                               , SA.Abs (SA.L 8.0 5.0)
                               , SA.Abs SA.Z
                               ]
                        ]
              ]
            , SE.marker
              [ SA.id "arrow-to-group"
              , SA.markerWidth 10.0
              , SA.markerHeight 10.0
              , SA.refX $ SA.Offset 19.0
              , SA.refY $ SA.Offset 5.0
              , SA.orient SA.OrientAuto
              , SA.markerUnits SA.UserSpaceOnUse
              ]
              [ SE.path [ SA.d [ SA.Abs (SA.M 0.0 0.0)
                               , SA.Abs (SA.L 0.0 10.0)
                               , SA.Abs (SA.L 8.0 5.0)
                               , SA.Abs SA.Z
                               ]
                        ]
              ]
            ]

  renderGraphNode :: UINode -> AppState -> H.ParentHTML Query ContentEditable.Query Slot Aff
  renderGraphNode node (AppState state) =
    let
      textFieldShape = fromMaybe defaultTextFieldShape
                       (Map.lookup (node ^. _id) state.textFieldShapes)
      hoveredOverBorder = state.hoveredElementId == (Just $ NodeBorderId $ node ^. _id)
      hoveredOverHalo = state.hoveredElementId == (Just $ NodeHaloId $ node ^. _id)
      noDrawingEdgeFromNode =
        Map.isEmpty (Map.filter (\drawingEdge -> drawingEdge.source == node ^. _id) state.drawingEdges)
      existsDrawingEdgeHoveredOverNode =
        (0 < Map.size (Map.filter ((flip drawingEdgeWithinNodeHalo) node) state.drawingEdges))
      drawingEdgeOverNode =
        (hoveredOverBorder || hoveredOverHalo)
        &&
        noDrawingEdgeFromNode
        &&
        existsDrawingEdgeHoveredOverNode
      nodeClasses =
        joinWith " " $ Array.catMaybes
        [ Just "node"
        , if (state.graph ^. _focus == FocusNode (node ^. _id))
          then Just "focused"
          else Nothing
        ]
      nodeBorderClasses =
          joinWith " " $ Array.catMaybes
          [ Just "nodeBorder"
          -- TODO: decide if grouping/ungrouping is useful for ologs
          --, if (Set.member (node ^. _id) (state.graph ^. _highlighted))
          --  then Just "highlighted"
          --  else Nothing
          , if hoveredOverBorder
               &&
               (not drawingEdgeOverNode)
            then Just "hovered"
            else Nothing
          ]
      haloClasses =
        joinWith " " $ Array.catMaybes
        [ Just "nodeHalo"
        , if (state.graph ^. _focus == FocusNode (node ^. _id))
             &&
             (not hoveredOverBorder)
          then Just "focused"
          else Nothing
        , if drawingEdgeOverNode
          then Just "ready"
          else Nothing
        ]
    in
      SE.g []
      [ SE.circle
        -- Node Halo, for creating edges from
        [ SA.class_ haloClasses
        , SA.r haloRadius
        , SA.cx (node ^. _pos).x
        , SA.cy (node ^. _pos).y
        , HE.onMouseDown \e -> Just $ StopPropagation (ME.toEvent e)
                               $ H.action $ EdgeDrawStart (node ^. _id) e
        , HE.onMouseEnter $ HE.input_ $ Hover $ Just $ NodeHaloId $ node ^. _id
        , HE.onMouseLeave $ HE.input_ $ Hover Nothing
        ]
      -- Node center
      , SE.circle
        [ SA.class_ nodeClasses
        , SA.r $ if (not (Map.isEmpty (node ^. _subgraph <<< _nodes)))
                 then groupNodeRadius
                 else nodeRadius
        , SA.cx (node ^. _pos).x
        , SA.cy (node ^. _pos).y
        ]
      -- Node border, for grabbing
      , SE.circle
        [ SA.class_ nodeBorderClasses
        , SA.r nodeBorderRadius
        , SA.cx (node ^. _pos).x
        , SA.cy (node ^. _pos).y
        , HE.onMouseDown \e -> Just
                               $ StopPropagation (ME.toEvent e)
                               $ H.action $ NodeDragStart (node ^. _id) (GraphSpacePos (node ^. _pos)) e
        , HE.onDoubleClick \e -> Just $ StopPropagation (ME.toEvent e)
                                 $ H.action $ DeleteNode $ node ^. _id
        , HE.onMouseEnter $ HE.input_ $ Hover $ Just $ NodeBorderId $ node ^. _id
        , HE.onMouseLeave $ HE.input_ $ Hover Nothing
        ]
      -- Node Textbox
      , SE.foreignObject
        [ SA.x $ (node ^. _pos).x + textBoxOffset.x
        , SA.y $ (node ^. _pos).y + textBoxOffset.y
        , SA.height textFieldShape.height
        , SA.width textFieldShape.width
        , HE.onMouseDown \e -> Just
                               $ StopPropagation (ME.toEvent e)
                               $ H.action DoNothing
        ]
        [ HH.slot
          (TextField (node ^. _id))
          ContentEditable.contenteditable
          (node ^. _text)
          (HE.input (TextInput (node ^. _id)))
        ]
      ]

  renderEdge :: UIEdge -> AppState -> Maybe (H.ParentHTML Query ContentEditable.Query Slot Aff)
  renderEdge edge (AppState state) = do
    sourceNode <- Map.lookup (edge ^. _source) $ state.graph ^. _nodes
    targetNode <- Map.lookup (edge ^. _target) $ state.graph ^. _nodes
    let
      edgeClasses =
        joinWith " " $ Array.catMaybes
        [ Just "edge"
        , if (state.graph ^. _focus == FocusEdge (edge ^. _edgeId) [])
          then Just "focused"
          else Nothing
        ]
    pure $
      SE.line
      [ SA.class_ edgeClasses
      , SA.x1 (sourceNode ^. _pos).x
      , SA.y1 (sourceNode ^. _pos).y
      , SA.x2 (targetNode ^. _pos).x
      , SA.y2 (targetNode ^. _pos).y
      , SA.markerEnd $ if (Map.isEmpty (targetNode ^. _subgraph <<< _nodes))
                       then SA.URL "#arrow"
                       else SA.URL "#arrow-to-group"
      ]

  renderEdgeBorder :: UIEdge -> AppState -> Maybe (H.ParentHTML Query ContentEditable.Query Slot Aff)
  renderEdgeBorder edge (AppState state) = do
    sourceNode <- Map.lookup (edge ^. _source) $ state.graph ^. _nodes
    targetNode <- Map.lookup (edge ^. _target) $ state.graph ^. _nodes
    let
      edgeBorderClasses =
        joinWith " " $ Array.catMaybes
        [ Just "edgeBorder"
        , if state.hoveredElementId == (Just $ EdgeBorderId $ edge ^. _edgeId)
          then Just "hover"
          else Nothing
        ]
    pure $
      SE.line
      [ SA.class_ edgeBorderClasses
      , SA.x1 (sourceNode ^. _pos).x
      , SA.y1 (sourceNode ^. _pos).y
      , SA.x2 (targetNode ^. _pos).x
      , SA.y2 (targetNode ^. _pos).y
      , SA.strokeLinecap SA.Butt
      , HE.onClick $ HE.input_ $ FocusOn $ FocusEdge (edge ^. _edgeId) []
      , HE.onMouseEnter $ HE.input_ $ Hover $ Just $ EdgeBorderId $ edge ^. _edgeId
      , HE.onMouseLeave $ HE.input_ $ Hover Nothing
      , HE.onDoubleClick \e -> Just $ StopPropagation (ME.toEvent e)
                               $ H.action $ DeleteEdge $ edge ^. _edgeId
      ]

  renderDrawingEdge :: DrawingEdge -> AppState -> Maybe (H.ParentHTML Query ContentEditable.Query Slot Aff)
  renderDrawingEdge drawingEdgeState (AppState state) = do
    sourcePos <- preview (_graphNodePos drawingEdgeState.source) (AppState state)
    let
      GraphSpacePos sourceGraphPos = sourcePos
      GraphSpacePos drawingEdgeGraphPos = drawingEdgeState.pos
      drawingEdgeClasses =
        joinWith " " $ Array.catMaybes
        [ Just "edge" ]
    pure $
      SE.line
      [ SA.class_ drawingEdgeClasses
      , SA.x1 sourceGraphPos.x
      , SA.y1 sourceGraphPos.y
      , SA.x2 drawingEdgeGraphPos.x
      , SA.y2 drawingEdgeGraphPos.y
      , SA.markerEnd $ SA.URL "#drawing-arrow"
      ]

  render :: AppState -> H.ParentHTML Query ContentEditable.Query Slot Aff
  render (AppState state) =
    let
      keyedNodes = map
                   (\node ->
                     Tuple (show (node ^. _id)) $ renderGraphNode node $ AppState state
                   )
                   $ Array.fromFoldable $ Map.values $ state.graph ^. _nodes
      keyedEdges = Array.mapMaybe
                   (\edge -> do
                       edgeHTML <- renderEdge edge $ AppState state
                       pure $ Tuple (edgeIdStr edge) edgeHTML
                   )
                   $ Array.fromFoldable $ allEdges state.graph
      keyedEdgeBorders = Array.mapMaybe
                     (\edge -> do
                         edgeHTML <- renderEdgeBorder edge $ AppState state
                         pure $ Tuple ((edgeIdStr edge) <> "_border") edgeHTML
                     )
                     $ Array.fromFoldable $ allEdges state.graph
      keyedDrawingEdges = Array.mapMaybe
                     (\drawingEdgeState -> do
                         drawingEdgeHTML <- renderDrawingEdge drawingEdgeState $ AppState state
                         pure $ Tuple (drawingEdgeKey drawingEdgeState.source) drawingEdgeHTML
                     )
                     $ Array.fromFoldable $ Map.values state.drawingEdges
      PageSpacePos graphOrigin = state.graphOrigin
    in
      HH.div_
      [ SE.svg
        [ SA.viewBox
          -- scale then shift
          ( - graphOrigin.x * state.zoom )
          ( - graphOrigin.y * state.zoom )
          ( state.windowSize.width * state.zoom )
          ( state.windowSize.height * state.zoom )
        , HE.onMouseDown \e -> Just
                               $ StopPropagation (ME.toEvent e)
                               $ H.action $ BackgroundDragStart state.graphOrigin e
        , HE.onDoubleClick \e -> Just
                                 $ StopPropagation (ME.toEvent e)
                                 $ H.action $ CreateNode e
        , HE.onWheel $ HE.input Zoom
        ]
        [ svgDefs
        , SK.g
          []
          (
           keyedEdges
           <> keyedEdgeBorders
           <> keyedDrawingEdges
           <> keyedNodes
          )
        ]
      , HH.input
        [ HP.type_ InputFile
        , HE.onChange $ HE.input FetchLocalFile
        ]
      ]

  eval :: Query ~> H.ParentDSL AppState Query ContentEditable.Query Slot Message Aff
  eval = case _ of
    PreventDefault e q -> do
      H.liftEffect $ WE.preventDefault e
      eval q

    StopPropagation e q -> do
      H.liftEffect $ WE.stopPropagation e
      eval q

    Init next -> next <$ do
      -- Update foreignObject wrapper sizes for the initial text
      AppState state <- H.get
      for_ (state.graph ^. _nodes) \node -> do
        eval $ H.action $ TextInput (node ^. _id) (ContentEditable.TextUpdate (node ^. _text))
      -- Add keyboard event listener to body
      document <- H.liftEffect $ WHW.document =<< WH.window
      H.subscribe $ ES.eventSource (attachKeydownListener document) (Just <<< H.request <<< Keypress)

    ResizeWindow windowSize next -> next <$ do
      AppState state <- H.get
      H.put $ AppState $ state { windowSize = windowSize }

    TextInput nodeId (ContentEditable.TextUpdate text) next -> next <$ do
      -- Update foreignObject wrapper shape to fit content.
      -- The actual text box is dynamically sized, but the foreighObject wrapper
      -- can't be set to fit the text, so we update it manually.
      maybeMaybeTextFieldScrollShape <- H.query (TextField nodeId) $ H.request ContentEditable.GetScrollShape
      let textFieldScrollShape =
            fromMaybe defaultTextFieldShape
            $ fromMaybe Nothing maybeMaybeTextFieldScrollShape
      let clippedScrollShape = { width : min textFieldScrollShape.width
                                             maxTextFieldShape.width
                               , height : min textFieldScrollShape.height
                                              maxTextFieldShape.height
                               }
      H.modify_ $ _graph <<< _nodes <<< at nodeId <<< traversed <<< _text .~ text
      H.modify_ $ _textFieldShapes <<< at nodeId ?~ clippedScrollShape

    BackgroundDragStart initialGraphOrigin mouseEvent next -> next <$ do
      H.modify_ $ _graph <<< _focus .~ NoFocus
      H.subscribe $ Drag.dragEventSource mouseEvent
        $ \e -> Just $ BackgroundDragMove e initialGraphOrigin H.Listening

    BackgroundDragMove (Drag.Move _ dragData) (PageSpacePos initialGraphOrigin) next -> next <$ do
      AppState state <- H.get
      let newGraphOrigin = PageSpacePos { x : initialGraphOrigin.x + dragData.offsetX
                                        , y : initialGraphOrigin.y + dragData.offsetY
                                        }
      H.modify_ $ _graphOrigin .~ newGraphOrigin

    BackgroundDragMove (Drag.Done _) _ next -> pure next

    NodeDragStart nodeId initialNodePos mouseEvent next -> next <$ do
      H.subscribe $ Drag.dragEventSource mouseEvent
        $ \e -> Just $ NodeDragMove e nodeId initialNodePos H.Listening
      H.modify_ $ _graph <<< _focus .~ FocusNode nodeId

    NodeDragMove (Drag.Move _ dragData) nodeId (GraphSpacePos initialNodePos) next -> next <$ do
      AppState state <- H.get
      let
        dragOffsetGraphSpace = { x : dragData.offsetX * state.zoom
                               , y : dragData.offsetY * state.zoom
                               }
        dragGraphPos = GraphSpacePos { x : initialNodePos.x + dragOffsetGraphSpace.x
                                     , y : initialNodePos.y + dragOffsetGraphSpace.y
                                     }
      H.modify_ $ _graphNodePos nodeId .~ dragGraphPos

    NodeDragMove (Drag.Done _) _ _ next -> pure next

    EdgeDrawStart drawingEdgeId mouseEvent next -> next <$ do
      AppState state <- H.get
      let
        mouseGraphPos = mouseEventPosition mouseEvent
                        # toGraphSpace state.graphOrigin state.zoom
        drawingEdgeSourceId = drawingEdgeId
      H.modify_ $ (_drawingEdges <<< at drawingEdgeId ?~ { pos : mouseGraphPos
                                                         , source : drawingEdgeSourceId
                                                         })
                >>> (_graph <<< _focus .~ FocusNode drawingEdgeId)
      H.subscribe $ Drag.dragEventSource mouseEvent $ \e -> Just $ EdgeDrawMove e drawingEdgeId H.Listening

    EdgeDrawMove (Drag.Move _ dragData) drawingEdgeId next -> next <$ do
      AppState state <- H.get
      let dragGraphPos = PageSpacePos { x : dragData.x, y : dragData.y }
                         # toGraphSpace state.graphOrigin state.zoom
      H.modify_ $ _drawingEdgePos drawingEdgeId .~ dragGraphPos

    EdgeDrawMove (Drag.Done _) drawingEdgeId next -> next <$ do
      -- Create a new edge if the edge is drawn within the halo of another node
      AppState state <- H.get
      let
        maybeNewEdgeTarget = do
          drawingEdgeState <- Map.lookup drawingEdgeId state.drawingEdges
          Map.values (state.graph ^. _nodes)
           # (List.filter (drawingEdgeWithinNodeHalo drawingEdgeState))
           # List.head
        drawingEdgeSourceId = drawingEdgeId
      case maybeNewEdgeTarget of
        Just newEdgeTarget ->
          eval $ CreateEdge { source : drawingEdgeSourceId, target : newEdgeTarget ^. _id } unit
        Nothing -> pure unit
      -- Remove the drawing edge
      H.modify_ $ _drawingEdges <<< at drawingEdgeId .~ Nothing

    CreateNode mouseEvent next -> next <$ do
      newNode <- H.liftEffect freshUINode
      AppState state <- H.get
      let
        GraphSpacePos newPos = mouseEventPosition mouseEvent # toGraphSpace state.graphOrigin state.zoom
        newNode' = newNode # _pos .~ newPos
                           # _text .~ "new node hey"
      H.modify_ $ (_graph %~ insertNode newNode')
                >>> (_textFieldShapes <<< at (newNode' ^. _id) ?~ defaultTextFieldShape)
      eval $ FocusOn (FocusNode $ newNode' ^. _id) unit

    DeleteNode id next -> next <$ do
      AppState state <- H.get
      case Map.lookup id (state.graph ^. _nodes) of
        Nothing -> pure unit
        Just node -> do
          H.modify_ $ (_graph %~ deleteNode node)
                    >>> (_textFieldShapes <<< at (node ^. _id) .~ Nothing)
          case Set.findMin ((lookupParents state.graph node) <> (lookupChildren state.graph node)) of
            Just neighbor -> eval $ FocusOn (FocusNode (neighbor ^. _id)) unit
            Nothing -> pure unit

    CreateEdge edgeId next -> next <$ do
      H.modify_ $ _graph %~ insertEdge (freshUIEdge edgeId)
      eval $ FocusOn (FocusEdge edgeId []) unit

    DeleteEdge edgeId next -> next <$ do
      H.modify_ $ _graph %~ deleteEdgeId edgeId
      eval $ FocusOn (FocusNode edgeId.source) unit

    FocusOn newFocus next -> next <$ do
      H.modify_ $ _graph <<< _focus .~ newFocus

    DeleteFocus next -> next <$ do
      state <- H.get
      case state ^. _graph <<< _focus of
        NoFocus -> pure unit
        FocusNode nodeId -> eval $ DeleteNode nodeId unit
        FocusEdge edgeId _ -> eval $ DeleteEdge edgeId unit

    Hover maybeElementId next -> next <$ do
      H.modify_ $ _AppState %~ _{ hoveredElementId = maybeElementId }

    -- | Zoom in/out holding the mouse position invariant in graph space
    -- |
    -- | p = mousePagePos
    -- | prevMouseGraphPos = (p - oldGraphOrigin) * oldZoom
    -- | newMouseGraphPos = (p - newGraphOrigin) * newZoom
    -- | let newMouseGraphPos = prevMouseGraphPos
    -- | => newGraphOrigin = p - ((p - oldGraphOrigin) * (oldZoom / newZoom))
    -- | So if newZoom --> inf, newGraphOrigin --> mousePos, as expected.
    Zoom wheelEvent next -> next <$ do
      AppState state <- H.get
      let
        scaledZoom = (WhE.deltaY wheelEvent) * zoomScaling
        PageSpacePos mousePagePos = mouseEventPosition $ WhE.toMouseEvent wheelEvent
        PageSpacePos graphOrigin = state.graphOrigin
        newZoom = Math.exp (scaledZoom + (Math.log state.zoom))
        -- keep graph-space mouse pos invariant
        newGraphOriginDim mousePos oldGraphOrigin =
          mousePos - ((mousePos - oldGraphOrigin) * (state.zoom / newZoom))
        newGraphOrigin = PageSpacePos
                         { x : newGraphOriginDim mousePagePos.x graphOrigin.x
                         , y : newGraphOriginDim mousePagePos.y graphOrigin.y
                         }
      let
        PageSpacePos asdf = newGraphOrigin
        GraphSpacePos mpgso = toGraphSpace (PageSpacePos mousePagePos) state.zoom state.graphOrigin
        GraphSpacePos mpgsn = toGraphSpace (PageSpacePos mousePagePos) newZoom newGraphOrigin
      H.modify_
        $ (_zoom .~ newZoom)
        >>> (_graphOrigin .~ newGraphOrigin)

    CenterGraphOrigin next -> next <$ do
      H.modify $ _graphOrigin .~ PageSpacePos { x : 0.0, y : 0.0 }

    FetchLocalFile changeEvent next -> next <$ runMaybeT do
      target <- MaybeT $ pure $ WE.target changeEvent
      inputElement <- MaybeT $ pure $ WHIE.fromEventTarget target
      files <- MaybeT $ H.liftEffect $ WHIE.files inputElement
      file <- MaybeT $ pure $ FileList.item 0 files
      fileReader <- lift $ H.liftEffect $ FileReader.fileReader
      lift $ H.liftEffect $ FileReader.readAsText (File.toBlob file) fileReader
      let attachLoadListener = \fn -> do
            listener <- ET.eventListener fn
            ET.addEventListener WHET.load listener false $ FileReader.toEventTarget fileReader
      lift $ H.subscribe $ ES.eventSource attachLoadListener (Just <<< H.request <<< LoadLocalFile fileReader)

    LoadLocalFile fileReader event reply -> do
      exceptTJSON <- H.liftEffect $ FileReader.result fileReader
      let Identity eitherJSON = runExceptT $ Foreign.readString exceptTJSON
      case (eitherJSON
            # lmap (show <<< map renderForeignError))
           >>= appStateFromJSON of
        Left errors -> H.liftEffect $ log errors
        Right appStateWithMeta -> do
          H.put appStateWithMeta.appState
          -- Update foreignObject wrapper sizes for the initial text
          AppState state <- H.get
          for_ (state.graph ^. _nodes) \node -> do
            eval $ H.action $ TextInput (node ^. _id) (ContentEditable.TextUpdate (node ^. _text))
      pure $ reply H.Done

    SaveLocalFile next -> next <$ do
      state <- H.get
      timestamp <- H.liftEffect now
      let
        stateJSON = appStateToJSON state { version : appStateVersion
                                         , timestamp : timestamp
                                         , graphMetadata : { version : uiGraphVersion}
                                         }
        title = fromMaybe "untitled" $ (graphTitle (state ^. _graph))
      H.liftEffect $ saveJSON stateJSON $ title <> ".graph.json"

    Keypress keyboardEvent reply -> do
      H.liftEffect $ log $ show $ KE.key keyboardEvent
      case KE.key keyboardEvent of
        "l" -> if not (KE.ctrlKey keyboardEvent) then pure unit else do
          H.liftEffect $ loadFile
        "s" -> if not (KE.ctrlKey keyboardEvent) then pure unit else do
          -- Save app state as a local json file
          H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
          H.liftEffect $ WE.stopPropagation $ KE.toEvent keyboardEvent
          eval $ SaveLocalFile unit
        "o" -> if not (KE.ctrlKey keyboardEvent) then pure unit else do
          H.liftEffect $ WE.preventDefault $ KE.toEvent keyboardEvent
          H.liftEffect $ WE.stopPropagation $ KE.toEvent keyboardEvent
          eval $ CenterGraphOrigin unit
        "Delete" ->
          eval $ DeleteFocus unit
        "Escape" ->
          eval $ FocusOn NoFocus unit
        -- TODO: decide if grouping/ungrouping is useful for ologs
        --"s" -> H.modify_ $ _graph %~ toggleHighlightFocus
        --" " -> H.modify_ $ _graph %~ toggleGroupExpand
        _ -> pure unit
      pure $ reply H.Listening

    DoNothing next -> pure next

mouseEventPosition :: ME.MouseEvent -> PageSpacePos
mouseEventPosition e = PageSpacePos { x : toNumber $ ME.pageX e
                                    , y : toNumber $ ME.pageY e
                                    }

euclideanDistance :: GraphSpacePos -> GraphSpacePos -> Number
euclideanDistance (GraphSpacePos pos1) (GraphSpacePos pos2) =
  Math.sqrt
  $ (Math.pow (pos1.x - pos2.x) 2.0)
  + (Math.pow (pos1.y - pos2.y) 2.0)

attachKeydownListener :: HTMLDocument -> (KE.KeyboardEvent -> Effect Unit) -> Effect Unit
attachKeydownListener document fn = do
  let target = HTMLDocument.toEventTarget document
  listener <- ET.eventListener (traverse_ fn <<< KE.fromEvent)
  ET.addEventListener KET.keydown listener false target

drawingEdgeWithinNodeHalo :: DrawingEdge -> UINode -> Boolean
drawingEdgeWithinNodeHalo drawingEdgeState' node =
  haloRadius > euclideanDistance (GraphSpacePos (node ^. _pos)) drawingEdgeState'.pos
