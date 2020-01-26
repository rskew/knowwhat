module Mapping where

import Core

type NodeMappingEdge
  = { sourceGraph :: GraphId
    , targetGraph :: GraphId
    , sourceNode  :: NodeId
    , targetNode  :: NodeId
    , text        :: String
    }

-- | Edge mappings can be drawn from an edge in the source graph to an edge in
-- | the target graph, or an edge in the source graph to a node in the target
-- | graph to indicate mapping the source morphism to the target node's identity
-- | morphism in the target category.
type EdgeMappingEdge
  = { sourceGraph :: GraphId
    , targetGraph :: GraphId
    , sourceEdge  :: EdgeId
    , target      :: Either NodeId EdgeId
    , text        :: String
    }

type Mapping
  = { sourceGraph :: GraphId
    , targetGraph :: GraphId
    , nodeEdgesSourceTarget :: Map NodeId (Map NodeId NodeMappingEdge)
    , edgeEdgesSourceTarget :: Map EdgeId (Map (Either NodeId EdgeId) EdgeMappingEdge)
    }
