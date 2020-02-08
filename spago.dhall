{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "halogen-svg"
, dependencies =
    [ "aff-coroutines"
    , "canvas"
    , "console"
    , "effect"
    , "foreign-generic"
    , "group"
    , "halogen"
    , "halogen-contenteditable"
    , "halogen-css"
    , "halogen-svg"
    , "httpure"
    , "node-sqlite3"
    , "node-websocket"
    , "nullable"
    , "prelude"
    , "profunctor-lenses"
    , "psci-support"
    , "quickcheck"
    , "run"
    , "simple-json"
    , "spec"
    , "strings"
    , "uuid"
    , "web-socket"
    , "web-uievents"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
