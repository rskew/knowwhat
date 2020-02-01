module GraphComponent.Types where

import Prelude

import AppOperation (AppOperation)
import AppState (DrawingEdgeId, HoveredElementId, Shape)
import ContentEditable.SVGComponent as SVGContentEditable
import Data.Maybe (Maybe)
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.Component.Utils.Drag as Drag
import Megagraph (Edge, EdgeId, EdgeMetadata, EdgeSpacePoint2D, Focus, GraphId, GraphSpacePoint2D, GraphView, Node, NodeId, PageSpacePoint2D)
import Web.Event.Event as WE
import Web.File.FileReader as FileReader
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.MouseEvent as ME
import Web.UIEvent.WheelEvent as WhE


data Action
  = PreventDefault WE.Event Action
  | StopPropagation WE.Event Action
  | EvalQuery (Query Unit)
  | Init
  | UpdateContentEditableText GraphId
  | UpdateNodeContentEditableText GraphId NodeId
  | BackgroundDragStart GraphId PageSpacePoint2D ME.MouseEvent
  | BackgroundDragMove Drag.DragEvent GraphId PageSpacePoint2D H.SubscriptionId
  | NodeDragStart GraphId NodeId GraphSpacePoint2D ME.MouseEvent
  | NodeDragMove Drag.DragEvent GraphId NodeId GraphSpacePoint2D H.SubscriptionId
  | EdgeDragStart GraphId EdgeId EdgeSpacePoint2D ME.MouseEvent
  | EdgeDragMove Drag.DragEvent GraphId EdgeId EdgeSpacePoint2D H.SubscriptionId
  | EdgeDrawStart GraphView DrawingEdgeId ME.MouseEvent
  | EdgeDrawMove Drag.DragEvent GraphId DrawingEdgeId H.SubscriptionId
  | NodeTextInput GraphId NodeId SVGContentEditable.Message
  | EdgeTextInput GraphId EdgeId SVGContentEditable.Message
  | TitleTextInput GraphId SVGContentEditable.Message
  | AppCreateNode GraphView ME.MouseEvent
  | AppDeleteNode Node
  | AppCreateEdge GraphId EdgeMetadata
  | AppDeleteEdge GraphId Edge
  | FocusOn GraphId (Maybe Focus)
  | DeleteFocus GraphId
  | Hover (Maybe HoveredElementId)
  | Zoom GraphId WhE.WheelEvent
  | CenterGraphOriginAndZoom
  | Undo GraphId
  | Redo GraphId
  | RemovePane GraphId
  | FetchLocalFile WE.Event
  | LoadLocalFile FileReader.FileReader H.SubscriptionId WE.Event
  | SaveLocalFile
  | Keypress KE.KeyboardEvent
  | Keyup KE.KeyboardEvent
  | DoNothing

type Input = Shape

data Query a
  = UpdateBoundingRect a
  | ReceiveOperation AppOperation a

data Message
  = SendOperation AppOperation

    -- Slot type for parent components using a child GraphComponent
type Slot = H.Slot Query Message

type Slots =
  ( nodeTextField  :: SVGContentEditable.Slot NodeId
  , edgeTextField  :: SVGContentEditable.Slot EdgeId
  , titleTextField :: SVGContentEditable.Slot GraphId
  )

_nodeTextField :: SProxy "nodeTextField"
_nodeTextField = SProxy

_edgeTextField :: SProxy "edgeTextField"
_edgeTextField = SProxy

_titleTextField :: SProxy "titleTextField"
_titleTextField = SProxy
