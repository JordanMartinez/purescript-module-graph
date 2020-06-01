{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "codec"
  , "codec-argonaut"
  , "console"
  , "effect"
  , "node-fs-aff"
  , "psci-support"
  , "routing-duplex"
  , "string-parsers"
  , "warp"
  ]
, packages = ./packages.dhall
, sources = [ "src/Server/**/*.purs", "src/Shared/**/*.purs" ]
}
