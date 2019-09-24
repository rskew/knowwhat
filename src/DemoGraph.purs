module DemoGraph where

import Prelude
import Effect (Effect)

import Data.Map as Map
import Data.UUID (genUUID)
import Data.Tuple (fst)
import Run as Run

import Workflow.Core (Graph, Node(..), Edge(..))
import Workflow.Graph (emptyGraph)
import Workflow.Graph.GraphOp (interpretGraphOp, insertNode, insertEdge)

demo :: Effect Graph
demo = do
  oscillatorId <- genUUID
  delayId <- genUUID
  amplifierId <- genUUID
  outputId <- genUUID
  filterId <- genUUID
  pure $ emptyGraph # (fst $ Run.extract $ interpretGraphOp $
    insertNode (Node { text: "oscillator"
                     , isValid: true
                     , position : { x: 450.0
                                  , y: 270.0
                                  }
                     , id : oscillatorId
                     , parents : Map.empty
                     , children : Map.empty
                     , subgraph : emptyGraph
                     })
               emptyGraph
    >>= \_ ->
      insertNode (Node { text: "delay"
                       , isValid: true
                       , position : { x: 450.0
                                    , y: 170.0
                                    }
                       , id : delayId
                       , parents : Map.empty
                       , children : Map.empty
                       , subgraph : emptyGraph
                       })
                 emptyGraph
    >>= \_ ->
      insertNode (Node { text: "gain"
                       , isValid: true
                       , position : { x: 650.0
                                    , y: 270.0
                                    }
                       , id : amplifierId
                       , parents : Map.empty
                       , children : Map.empty
                       , subgraph : emptyGraph
                       })
                 emptyGraph
    >>= \_ ->
      insertNode (Node { text: "output"
                       , isValid: true
                       , position : { x: 800.0
                                    , y: 270.0
                                    }
                       , id : outputId
                       , parents : Map.empty
                       , children : Map.empty
                       , subgraph : emptyGraph
                       })
                 emptyGraph
    >>= \_ ->
      insertNode (Node { text: "filter"
                       , isValid: true
                       , position : { x: 550.0
                                    , y: 370.0
                                    }
                       , id : filterId
                       , parents : Map.empty
                       , children : Map.empty
                       , subgraph : emptyGraph
                       })
                 emptyGraph
    >>= \_ ->
      insertEdge (Edge  { id : { source : oscillatorId
                               , target : delayId
                               }
                        , text : ""
                        , isValid : false
                        })
    >>= \_ ->
      insertEdge (Edge  { id : { source : delayId
                               , target : amplifierId
                               }
                        , text : ""
                        , isValid : false
                        })
    >>= \_ ->
      insertEdge (Edge  { id : { source : amplifierId
                               , target : outputId
                               }
                        , text : ""
                        , isValid : false
                        })
    >>= \_ ->
      insertEdge (Edge  { id : { source : amplifierId
                               , target : filterId
                               }
                        , text : ""
                        , isValid : false
                        }))
