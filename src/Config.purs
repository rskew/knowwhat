module Config where

import Prelude

version :: String
version = "0.0.0.0.0.0.1"

webSocketURL :: String -> String
webSocketURL hostname = "ws://" <> hostname <> ":8086/v1/graphql"

homeGraphTitle :: String
homeGraphTitle = "home"
