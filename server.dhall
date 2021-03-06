{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "argonaut-core"
  , "codec"
  , "codec-argonaut"
  , "console"
  , "effect"
  , "interpolate"
  , "node-fs-aff"
  , "psci-support"
  , "routing-duplex"
  , "string-parsers"
  , "stringutils"
  , "unordered-collections"
  , "warp"
  ]
, packages = ./packages.dhall
, sources = [ "src/Server/**/*.purs", "src/Shared/**/*.purs" ]
}
