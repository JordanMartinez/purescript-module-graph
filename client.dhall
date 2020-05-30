{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "console"
  , "effect"
  , "halogen-hooks-extra"
  , "halogen-svg"
  , "psci-support"
  , "string-parsers"
  ]
, packages = ./packages.dhall
, sources = [ "src/client/**/*.purs", "src/shared/**/*.purs" ]
}
