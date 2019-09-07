module Utils where

import Prelude

import Effect (Effect)
import Effect.Ref as Ref
import Effect.Ref (Ref)
import Web.HTML as WH
import Web.HTML.Window as WHW


-- From https://github.com/paf31/purescript-behaviors/blob/master/src/FRP/Event/AnimationFrame.purs#L12
animationFrameLoop :: Ref Boolean -> Effect Unit -> Effect (Effect Unit)
animationFrameLoop stopperRef action = do
  w <- WH.window
  let loop = void do
        w # WHW.requestAnimationFrame do
          action
          unlessM (Ref.read stopperRef) loop
  loop
  pure (Ref.write true stopperRef)
