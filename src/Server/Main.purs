module Server.Main where

import Prelude

import AppOperation (AppOperation)
import Control.Monad.Except (runExcept)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (launchAff, try)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Foreign (renderForeignError)
import Foreign as Foreign
import Foreign.Generic (decodeJSON)
import HTTPure as HTTPure
import Node.FS.Aff as FS
import Server.Config (config)
import Server.GraphDB (startDB)
import Server.GraphDB.Interpreter (interpretAppOperation)
import SQLite3 (DBConnection)


main :: Effect Unit
main = unit <$ launchAff do
  db <- startDB config.dbFile
  liftEffect $ startServer db

startServer :: DBConnection -> HTTPure.ServerM
startServer db =
  HTTPure.serve config.port router $ Console.log ("Server now up on port " <> show config.port)
  where
    -- Serve graph drawing app
    router { method: HTTPure.Get, path } =
      case path of
        [] -> serveFile "index.html"
        [ fileName ] -> serveFile fileName
        _ -> HTTPure.notFound
    -- Receive operations from graph drawing app
    router { path: ["operation"], body, method: HTTPure.Post } =
      case
        lmap (show <<< map renderForeignError)
        $ runExcept $ (decodeJSON body :: Foreign.F (AppOperation Unit))
      of
        Left errors -> do
          Console.log $ "received operation but could not decode: " <> errors
          HTTPure.badRequest body
        Right operation -> do
          Console.log $ "received operation: " <> show operation
          result <- try $ interpretAppOperation db operation
          case result of
            Left error -> do
              Console.log "Failed to interpret operation"
              HTTPure.badRequest body
            Right _ ->
              HTTPure.ok ":D"
    router _ = HTTPure.notFound

serveFile :: String -> HTTPure.ResponseM
serveFile fileName = do
  result <- try $ FS.readFile fileName
  case result of
    Left err -> do
      Console.log $ "file not found: " <> fileName
      HTTPure.notFound
    Right file -> do
      Console.log $ "serving file: " <> fileName
      HTTPure.ok file
