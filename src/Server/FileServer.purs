module Server.FileServer where

import Prelude

import Data.Either (Either(..))
import Effect.Aff (try)
import Effect.Class.Console as Console
import HTTPure as HTTPure
import Node.FS.Aff as FS
import Server.Config (config)

startFileServer :: HTTPure.ServerM
startFileServer =
  HTTPure.serve config.fileServerPort router
    $ Console.log ("File server now up on port " <> show config.fileServerPort)
  where
    -- Serve graph drawing app
    router { method: HTTPure.Get, path } =
      case path of
        [] -> serveFile "index.html"
        [ fileName ] -> serveFile fileName
        _ -> HTTPure.notFound
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
