module Workflow.Context where

import Workflow.Core
import Workflow.Interaction.Impl

import Effect (Effect)
import Prelude (class Monad, pure)
import Web.DOM.Document as Doc

-- | Embeds a graph in a context with side effects associated with
-- | graph construction.
-- | Examples are DOM manipulation,
-- | web-audio synth wiring,

class (Monad c, Graph graph node edge, Node node, Edge edge) <=
      GraphContext c graph node edge | graph -> node, graph -> edge where
  create :: graph -> c graph
  insertNode :: graph -> node -> c graph
  updateNode :: graph -> node -> c graph
  deleteNode :: graph -> node -> c graph
  insertEdge :: graph -> edge -> c graph
  updateEdge :: graph -> edge -> c graph
  deleteEdge :: graph -> edge -> c graph

-- | Update the DOM with changes to the graph
instance graphContextDOM :: GraphContext Effect InterGraphImpl InterNodeImpl InterEdgeImpl where
  create = createDOM
  insertNode g n = pure g
  updateNode g n = pure g
  deleteNode g n = pure g
  insertEdge g e = pure g
  updateEdge g e = pure g
  deleteEdge g e = pure g


createDOM :: InterGraphImpl -> Effect InterGraphImpl
createDOM g = pure g

newtype ComponentHTML = ComponentHTML
                        String
                        (Array Attributes)
                        (Array EventHandlers)
                        (Array ComponentHTML)


nodeComponent :: ComponentHTML
nodeComponent = ComponentHTML
             [] -- attributes
             [] -- handlers
             [ El.g
               [ El.draggable ]
               [ Ev.drag \_ _ -> move ]
               [ El.circle -- node
               , El.circle -- node border
               , El.text -- node text
               ]
             , El.circle -- node halo
               [ El.draggable ]
               [ Ev.drag \_ _ -> drawingEdge
               , Ev.drop \_ _ -> createEdge
               ]
               []
             ]

createComponent :: 

initialiseGraphNodeComponent :: InterNodeImpl -> Effect Unit
initialiseGraphNodeComponent node =
  let
    el = Doc.createElement "g" #
      El.setAttribute "x" node.pos.x
      >>>
      El.setAttribute "y" node.pos.y
      >>>
      El.setAttribute "" node.pos.y


updateGraphNodeComponent :: InterNodeImpl -> Effect Unit
updateGraphNodeComponent node =
  -- grab 

