module Server.Main where

import Prelude

import AppOperation (AppOperation)
import Control.Monad.Except.Trans (runExceptT)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Foreign (renderForeignError)
import Foreign as Foreign
import Foreign.Generic (decodeJSON)
import HTTPure as HTTPure
import Node.FS.Aff as FS
import Server.Schema as Schema
import SQLite3 (DBConnection)
import SQLite3 as SQLite


foreign import _enableForeignKey :: DBConnection -> EffectFnAff Unit

enableForeignKey :: DBConnection -> Aff Unit
enableForeignKey = fromEffectFnAff <<< _enableForeignKey

dbFile :: SQLite.FilePath
dbFile = "../server/testdb.sqlite"

main :: Effect Unit
main = unit <$ launchAff do
  db <- startDB dbFile
  liftEffect $ startServer db

startDB :: String -> Aff DBConnection
startDB dbFileName = do
  db <- SQLite.newDB dbFileName
  enableForeignKey db
  _ <- SQLite.queryDB db Schema.graphsTableSchema []
  _ <- SQLite.queryDB db Schema.nodesTableSchema []
  _ <- SQLite.queryDB db Schema.edgesTableSchema []
  pure db

startServer :: DBConnection -> HTTPure.ServerM
startServer dbConn =
  HTTPure.serve 8080 router $ Console.log "Server now up on port 8080"
  where
    router { method: HTTPure.Get, path } =
      case path of
        [] -> serveFile "index.html"
        [ fileName ] -> serveFile fileName
        _ -> HTTPure.notFound
    router { path: ["operation"], body, method: HTTPure.Post } = do
      case
        lmap (show <<< map renderForeignError)
        $ unwrap $ runExceptT $ (decodeJSON body :: Foreign.F (AppOperation Unit))
      of
        Left errors -> Console.log $ "received operation but could not decode: " <> errors
        Right operation -> Console.log $ "received operation: " <> show operation
      HTTPure.ok ""
    router _ = HTTPure.ok ":D"

serveFile :: String -> HTTPure.ResponseM
serveFile fileName = do
  Console.log $ "serving file: " <> fileName
  file <- FS.readFile fileName
  HTTPure.ok file
