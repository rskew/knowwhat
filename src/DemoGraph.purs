module DemoGraph where

import Prelude
import Effect (Effect)

import Data.Map as Map
import Data.Set as Set

import Data.Lens ((%~), (.~))

import Data.UUID (genUUID)

import Workflow.Core (Graph, Node(..), Edge(..), Focus(..), _highlighted, _focus)
import Workflow.Graph (emptyGraph, insertNode, insertEdge)

demo :: Effect Graph
demo = do
  oscillatorId <- genUUID
  delayId <- genUUID
  amplifierId <- genUUID
  outputId <- genUUID
  filterId <- genUUID
  pure $
    emptyGraph
    # insertNode (Node { text: "oscillator"
                       , isValid: true
                       , position : { x: 450.0
                                    , y: 270.0
                                    }
                       , id : oscillatorId
                       , parents : Map.empty
                       , children : Map.empty
                       , subgraph : emptyGraph
                       })
    # insertNode (Node { text: "delay"
                       , isValid: true
                       , position : { x: 450.0
                                    , y: 170.0
                                    }
                       , id : delayId
                       , parents : Map.empty
                       , children : Map.empty
                       , subgraph : emptyGraph
                       })
    # insertNode (Node { text: "gain"
                       , isValid: true
                       , position : { x: 650.0
                                    , y: 270.0
                                    }
                       , id : amplifierId
                       , parents : Map.empty
                       , children : Map.empty
                       , subgraph : emptyGraph
                       })
    # insertNode (Node { text: "output"
                       , isValid: true
                       , position : { x: 800.0
                                    , y: 270.0
                                    }
                       , id : outputId
                       , parents : Map.empty
                       , children : Map.empty
                       , subgraph : emptyGraph
                       })
    # insertNode (Node { text: "filter"
                       , isValid: true
                       , position : { x: 550.0
                                    , y: 370.0
                                    }
                       , id : filterId
                       , parents : Map.empty
                       , children : Map.empty
                       , subgraph : emptyGraph
                       })
    # insertEdge (Edge  { id : { source : oscillatorId
                               , target : delayId
                               }
                        , text : ""
                        , isValid : false
                        })
    # insertEdge (Edge  { id : { source : delayId
                               , target : amplifierId
                               }
                        , text : ""
                        , isValid : false
                        })
    # insertEdge (Edge  { id : { source : amplifierId
                               , target : outputId
                               }
                        , text : ""
                        , isValid : false
                        })
    # insertEdge (Edge  { id : { source : amplifierId
                               , target : filterId
                               }
                        , text : ""
                        , isValid : false
                        })

demo_ :: Effect Graph
demo_ = do
  goofusId <- genUUID
  thingoId <- genUUID
  titleId <- genUUID
  pure $
    emptyGraph
    # insertNode (Node { text: "asdfasdfasdfasdf"
                       , isValid: true
                       , position : { x: 450.0
                                    , y: 270.0
                                    }
                       , id : goofusId
                       , parents : Map.empty
                       , children : Map.empty
                       , subgraph : emptyGraph
                       })
    # insertNode (Node { text: ""
                       , isValid: true
                       , position : { x : 205.0
                                    , y : 100.0
                                    }
                       , id : thingoId
                       , parents : Map.empty
                       , children : Map.empty
                       , subgraph : emptyGraph
                       })
    # insertNode (Node { text: "Title: Workflow"
                       , isValid: true
                       , position : { x : 205.0
                                    , y : 150.0
                                    }
                       , id : titleId
                       , parents : Map.empty
                       , children : Map.empty
                       , subgraph : emptyGraph
                       })
    # insertEdge (Edge { id : { source : thingoId
                              , target : goofusId
                              }
                       , text : ""
                       , isValid : false
                       })
    # insertEdge (Edge  { id : { source : titleId
                               , target : goofusId
                               }
                        , text : "hehehe"
                        , isValid : false
                        })
    # _highlighted %~ Set.insert thingoId
    # _focus .~ (FocusEdge { source: titleId
                           , target: goofusId
                           }
                 [{ source: titleId
                  , target: goofusId
                  },
                  { source: thingoId
                  , target: goofusId
                  }
                 ]
                )
