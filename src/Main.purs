module Main where

import Prelude
import Effect (Effect)
import Effect.Class.Console (log)

import Data.Int (toNumber)
import Data.Map as Map
import Data.Set as Set

import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Web.HTML (window)
import Web.HTML.Window (innerWidth, innerHeight)

import Data.Lens ((%~), (.~))

import Data.UUID (genUUID)

import GraphComponent as G

import Workflow.UIGraph (UIGraph, emptyUIGraph, UINode(..), UIEdge(..), Focus(..), _highlighted, _focus)
import Workflow.Core (insertNode, insertEdge)

main :: Effect Unit
main =
  HA.runHalogenAff do
  body <- HA.awaitBody
  w <- H.liftEffect window
  windowWidth <- H.liftEffect $ innerWidth w
  windowHeight <- H.liftEffect $ innerHeight w
  H.liftEffect $ log $ "Window size: " <> show windowWidth <> " " <> show windowHeight
  graphId <- H.liftEffect genUUID
  initGraph <- H.liftEffect demo
  let input =  { graph : initGraph
               , windowSize : { width : toNumber windowWidth
                              , height : toNumber windowHeight
                              }
               , graphId : graphId
               }
  runUI G.graph input body


--------
---- Demo

demo :: Effect UIGraph
demo = do
  goofusId <- genUUID
  thingoId <- genUUID
  titleId <- genUUID
  pure $
    emptyUIGraph
    # insertNode (UINode { text: "asdfasdfasdfasdf"
                         , isValid: true
                         , position : { x: 450.0
                                      , y: 270.0
                                      }
                         , id : goofusId
                         , parents : Map.empty
                         , children : Map.empty
                         , subgraph : emptyUIGraph
                         })
    # insertNode (UINode { text: ""
                         , isValid: true
                         , position : { x : 205.0
                                      , y : 100.0
                                      }
                         , id : thingoId
                         , parents : Map.empty
                         , children : Map.empty
                         , subgraph : emptyUIGraph
                         })
    # insertNode (UINode { text: "Title: Workflow"
                         , isValid: true
                         , position : { x : 205.0
                                      , y : 150.0
                                      }
                         , id : titleId
                         , parents : Map.empty
                         , children : Map.empty
                         , subgraph : emptyUIGraph
                         })
    # insertEdge (UIEdge { id : { source : thingoId
                                , target : goofusId
                                }
                         , text : ""
                         , isValid : false
                         })
    # insertEdge (UIEdge  { id : { source : titleId
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
