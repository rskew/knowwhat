{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "halogen-svg"
, dependencies =
    [ "affjax"
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
    , "prelude"
    , "profunctor-lenses"
    , "psci-support"
    , "quickcheck"
    , "run"
    , "strings"
    , "uuid"
    , "web-uievents"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
