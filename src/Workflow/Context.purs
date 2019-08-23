module Workflow.Context where

--import Workflow.Core
--import Workflow.Interaction.Impl
--
--import Effect (Effect)
--import Prelude (class Monad, pure)

-- | Embeds a graph in a context with side effects associated with
-- | graph construction.
-- | Examples are DOM manipulation,
-- | web-audio synth wiring,

--class (Monad c, Graph graph node edge, Node node, Edge edge) <=
--      GraphContext c graph node edge | graph -> node, graph -> edge where
--  create :: graph -> c graph
--  insertNode :: graph -> node -> c graph
--  updateNode :: graph -> node -> c graph
--  deleteNode :: graph -> node -> c graph
--  insertEdge :: graph -> edge -> c graph
--  updateEdge :: graph -> edge -> c graph
--  deleteEdge :: graph -> edge -> c graph
