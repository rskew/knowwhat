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
  helloId <- genUUID
  smileId <- genUUID
  let
    helloNode = freshNode graphId helloId
    smileNode = freshNode graphId smileId
  pure $ Tuple graphId $ (wrap do
    insertPane graphId

    insertNode graphId helloId
    moveNode helloNode (GraphSpacePoint2D { x : 450.0, y : 270.0 })
    updateNodeText helloNode "hello"

    insertNode graphId smileId
    moveNode smileNode (GraphSpacePoint2D { x : 450.0, y : 170.0 })
    updateNodeText smileNode ":D"

    insertEdge { source      : helloId
               , sourceGraph : graphId
               , target      : smileId
               , targetGraph : graphId
               })
