{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "affjax"
  , "console"
  , "effect"
  , "halogen-hooks-extra"
  , "halogen-svg"
  , "http-methods"
  , "psci-support"
  , "routing-duplex"
  , "string-parsers"
  ]
, packages = ./packages.dhall
, sources = [ "src/Client/**/*.purs", "src/Shared/**/*.purs" ]
}
