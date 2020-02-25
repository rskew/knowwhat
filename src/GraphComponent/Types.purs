module GraphComponent.Types where

import Prelude

import AppOperation (AppOperation)
import AppState (DrawingEdgeId, EdgeSourceElement, HoveredElementId, MegagraphElement, Shape)
import ContentEditable.SVGComponent as SVGContentEditable
import Data.Maybe (Maybe)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple)
import Halogen as H
import Halogen.Component.Utils.Drag as Drag
import Megagraph (Edge, EdgeId, EdgeMappingEdge, EdgeMetadata, Focus, GraphEdgeSpacePoint2D, GraphId, GraphSpacePoint2D, GraphView, Mapping, MappingId, Node, NodeId, NodeMappingEdge, PageEdgeSpacePoint2D, PageSpacePoint2D)
import MegagraphOperation (MegagraphUpdate)
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
  | EdgeDragStart GraphId EdgeId GraphEdgeSpacePoint2D ME.MouseEvent
  | EdgeDragMove Drag.DragEvent GraphId EdgeId GraphEdgeSpacePoint2D H.SubscriptionId
  | NodeMappingEdgeDragStart MappingId NodeMappingEdge ME.MouseEvent
  | NodeMappingEdgeDragMove Drag.DragEvent Mapping EdgeId PageEdgeSpacePoint2D H.SubscriptionId
  | EdgeMappingEdgeDragStart MappingId EdgeMappingEdge ME.MouseEvent
  | EdgeMappingEdgeDragMove Drag.DragEvent Mapping EdgeId PageEdgeSpacePoint2D H.SubscriptionId
  | EdgeDrawStart GraphView EdgeSourceElement ME.MouseEvent
  | EdgeDrawMove Drag.DragEvent GraphId DrawingEdgeId H.SubscriptionId
  | NodeTextInput GraphId NodeId String
  | EdgeTextInput GraphId EdgeId String
  | TitleTextInput GraphId String
  | AppCreateNode GraphView ME.MouseEvent
  | AppDeleteNode Node
  | AppCreateEdge EdgeMetadata
  | AppDeleteEdge Edge
  | AppCreateNodeMappingEdge EdgeId NodeId GraphId NodeId GraphId
  | AppDeleteNodeMappingEdge MappingId EdgeId
  | AppCreateEdgeMappingEdge EdgeId EdgeId GraphId EdgeId GraphId
  | AppDeleteEdgeMappingEdge MappingId EdgeId
  | UpdateFocus (Maybe Focus)
  | UpdateFocusPane MegagraphElement
  | DeleteFocus
  | Hover HoveredElementId
  | UnHover HoveredElementId
  | FocusText (String -> Maybe (Tuple MegagraphUpdate MegagraphElement))
  | BlurText String
  | Zoom GraphId WhE.WheelEvent
  | CenterGraphOriginAndZoom
  | Undo MegagraphElement
  | Redo MegagraphElement
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
  = SendOperation String

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
