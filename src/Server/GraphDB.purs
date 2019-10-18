module Server.GraphDB where

import Prelude

import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Server.GraphDB.Schema as Schema
import SQLite3 (DBConnection)
import SQLite3 as SQLite

foreign import _enableForeignKey :: DBConnection -> EffectFnAff Unit

enableForeignKey :: DBConnection -> Aff Unit
enableForeignKey = fromEffectFnAff <<< _enableForeignKey

startDB :: String -> Aff DBConnection
startDB dbFileName = do
  db <- SQLite.newDB dbFileName
  enableForeignKey db
  _ <- SQLite.queryDB db Schema.graphsTableSchema []
  _ <- SQLite.queryDB db Schema.nodesTableSchema []
  _ <- SQLite.queryDB db Schema.edgesTableSchema []
  pure db
