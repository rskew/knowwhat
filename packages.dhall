{-
Welcome to your new Dhall package-set!

Below are instructions for how to edit this file for most use
cases, so that you don't need to know Dhall to use it.

## Warning: Don't Move This Top-Level Comment!

Due to how `dhall format` currently works, this comment's
instructions cannot appear near corresponding sections below
because `dhall format` will delete the comment. However,
it will not delete a top-level comment like this one.

## Use Cases

Most will want to do one or both of these options:
1. Override/Patch a package's dependency
2. Add a package not already in the default package set

This file will continue to work whether you use one or both options.
Instructions for each option are explained below.

### Overriding/Patching a package

Purpose:
- Change a package's dependency to a newer/older release than the
    default package set's release
- Use your own modified version of some dependency that may
    include new API, changed API, removed API by
    using your custom git repo of the library rather than
    the package set's repo

Syntax:
Replace the overrides' "{=}" (an empty record) with the following idea
The "//" or "⫽" means "merge these two records and
  when they have the same value, use the one on the right:"
-------------------------------
let override =
  { packageName =
      upstream.packageName // { updateEntity1 = "new value", updateEntity2 = "new value" }
  , packageName =
      upstream.packageName // { version = "v4.0.0" }
  , packageName =
      upstream.packageName // { repo = "https://www.example.com/path/to/new/repo.git" }
  }
-------------------------------

Example:
-------------------------------
let overrides =
  { halogen =
      upstream.halogen // { version = "master" }
  , halogen-vdom =
      upstream.halogen-vdom // { version = "v4.0.0" }
  }
-------------------------------

### Additions

Purpose:
- Add packages that aren't already included in the default package set

Syntax:
Replace the additions' "{=}" (an empty record) with the following idea:
-------------------------------
let additions =
  { "package-name" =
       { dependencies =
           [ "dependency1"
           , "dependency2"
           ]
       , repo =
           "https://example.com/path/to/git/repo.git"
       , version =
           "tag ('v4.0.0') or branch ('master')"
       }
  , "package-name" =
       { dependencies =
           [ "dependency1"
           , "dependency2"
           ]
       , repo =
           "https://example.com/path/to/git/repo.git"
       , version =
           "tag ('v4.0.0') or branch ('master')"
       }
  , etc.
  }
-------------------------------

Example:
-------------------------------
let additions =
  { benchotron =
      { dependencies =
          [ "arrays"
          , "exists"
          , "profunctor"
          , "strings"
          , "quickcheck"
          , "lcg"
          , "transformers"
          , "foldable-traversable"
          , "exceptions"
          , "node-fs"
          , "node-buffer"
          , "node-readline"
          , "datetime"
          , "now"
          ],
      , repo =
          "https://github.com/hdgarrood/purescript-benchotron.git"
      , version =
          "v7.0.0"
      }
  }
-------------------------------
-}


let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.3-20190827/packages.dhall sha256:93f6b11068b42eac6632d56dab659a151c231381e53a16de621ae6d0dab475ce

let overrides = {=}

let additions =
  { undoable =
      { dependencies =
          [ "assert"
          , "console"
          , "effect"
          , "prelude"
          , "psci-support"
          , "maybe"
          , "tuples"
          , "profunctor-lenses"
          , "group"
          , "generics-rep"
          , "foreign-generic"
          ]
      , repo =
          "https://github.com/rskew/purescript-undoable.git"
      , version =
          "3e3074049766d921eea5de6a69418732a5d49b01"
      }
  , halogen-contenteditable =
      { dependencies =
          [ "console"
          , "effect"
          , "prelude"
          , "psci-support"
          , "halogen"
          , "halogen-svg"
          ]
      , repo =
          "https://github.com/rskew/purescript-halogen-contenteditable.git"
      , version =
          "23f9467a4307ea1f8615f08afc152103ade74ba3"
      }
  , halogen-svg =
      { dependencies =
          [ "console"
          , "effect"
          , "halogen"
          , "prelude"
          , "psci-support"
          , "strings"
          , "web-uievents"
          ]
      , repo =
          "https://github.com/rskew/purescript-halogen-svg.git"
      , version =
          "a9ad4422ee0fad84537558b46626b824f1883332"
      }
  , webaudio =
      { dependencies =
          [ "aff"
          , "affjax"
          , "arraybuffer"
          , "arraybuffer-types"
          , "arrays"
          , "assert"
          , "console"
          , "effect"
          , "foldable-traversable"
          , "js-timers"
          , "lists"
          , "math"
          , "maybe"
          , "psci-support"
          , "refs"
          , "strings"
          , "tuples"
          ]
      , repo =
          "https://github.com/rskew/purescript-webaudio.git"
      , version =
          "0bea1467032688d7925bdc8f7856d8e21747441c"
      }
  }

in  upstream // overrides // additions
