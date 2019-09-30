module DemoGraph where

import AppOperation (AppOperation)
import AppOperation.GraphOp (insertNode, insertEdge, moveNode, updateNodeText)
import AppOperation.UIOp (insertPane)
import Prelude
import Effect (Effect)
import Core (GraphId, GraphSpacePoint2D(..), freshNode)
import Data.Newtype (wrap)
import Data.UUID (genUUID)
import Data.Tuple (Tuple(..))


demo :: Effect (Tuple GraphId (AppOperation Unit))
demo = do
  graphId <- genUUID
  oscillatorId <- genUUID
  delayId <- genUUID
  amplifierId <- genUUID
  outputId <- genUUID
  filterId <- genUUID
  let
    oscillatorNode = freshNode graphId oscillatorId
    delayNode      = freshNode graphId delayId
    amplifierNode  = freshNode graphId amplifierId
    outputNode     = freshNode graphId outputId
    filterNode     = freshNode graphId filterId
  pure $ Tuple graphId $ (wrap do
    insertPane graphId

    insertNode graphId oscillatorId
    moveNode oscillatorNode (GraphSpacePoint2D { x : 450.0, y : 270.0 })
    updateNodeText oscillatorNode "oscillator"

    insertNode graphId delayId
    moveNode delayNode (GraphSpacePoint2D { x : 450.0, y : 170.0 })
    updateNodeText delayNode "delay"

    insertNode graphId amplifierId
    moveNode amplifierNode (GraphSpacePoint2D { x : 650.0, y : 270.0 })
    updateNodeText amplifierNode "gain"

    insertNode graphId outputId
    moveNode outputNode (GraphSpacePoint2D { x : 800.0, y : 270.0 })
    updateNodeText outputNode "output"

    insertNode graphId filterId
    moveNode filterNode (GraphSpacePoint2D { x : 550.0, y : 370.0 })
    updateNodeText filterNode "filter"

    insertEdge { source      : oscillatorId
               , sourceGraph : graphId
               , target      : delayId
               , targetGraph : graphId
               }
    insertEdge { source      : delayId
               , sourceGraph : graphId
               , target      : amplifierId
               , targetGraph : graphId
               }
    insertEdge { source      : amplifierId
               , sourceGraph : graphId
               , target      : outputId
               , targetGraph : graphId
               }
    insertEdge { source      : amplifierId
               , sourceGraph : graphId
               , target      : filterId
               , targetGraph : graphId
               })
