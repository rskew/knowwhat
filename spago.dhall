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
    , "formatters"
    , "group"
    , "halogen"
    , "halogen-contenteditable"
    , "halogen-css"
    , "halogen-svg"
    , "httpure"
    , "node-sqlite3"
    , "nullable"
    , "prelude"
    , "profunctor-lenses"
    , "psci-support"
    , "quickcheck"
    , "record-extra"
    , "run"
    , "simple-json"
    , "spec"
    , "strings"
    , "uuid"
    , "web-socket"
    , "web-uievents"
    , "functorial-data-migration-core"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
