module Server.Main where

import Prelude

import AppOperation (AppOperation)
import Control.Monad.Except.Trans (runExceptT)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Newtype (unwrap)
import Effect.Class.Console as Console
import Foreign (renderForeignError)
import Foreign as Foreign
import Foreign.Generic (decodeJSON)
import HTTPure as HTTPure

main :: HTTPure.ServerM
main =
  HTTPure.serve 8080 router $ Console.log "Server now up on port 8080"
  where
    router { path: ["operation"], body, method: HTTPure.Post } = do
      case
        lmap (show <<< map renderForeignError)
        $ unwrap $ runExceptT $ (decodeJSON body :: Foreign.F (AppOperation Unit))
      of
        Left errors -> Console.log $ "received operation but could not decode: " <> errors
        Right operation -> Console.log $ "received operation: " <> show operation
      HTTPure.ok ""
    router _ = HTTPure.ok "hello world :D"
