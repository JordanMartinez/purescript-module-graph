{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "console"
  , "effect"
  , "node-fs-aff"
  , "psci-support"
  , "string-parsers"
  , "warp"
  ]
, packages = ./packages.dhall
, sources = [ "src/server/**/*.purs", "src/shared/**/*.purs" ]
}
