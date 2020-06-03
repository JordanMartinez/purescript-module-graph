{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "affjax"
  , "argonaut-core"
  , "codec"
  , "codec-argonaut"
  , "console"
  , "effect"
  , "halogen-hooks-extra"
  , "halogen-select"
  , "halogen-svg"
  , "http-methods"
  , "profunctor-lenses"
  , "psci-support"
  , "remotedata"
  , "routing-duplex"
  , "string-parsers"
  , "unordered-collections"
  ]
, packages = ./packages.dhall
, sources = [ "src/Client/**/*.purs", "src/Shared/**/*.purs" ]
}
