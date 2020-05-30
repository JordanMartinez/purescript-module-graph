{-
Welcome to a Spago project!
You can edit this file as you like.
-}
let client = ./client.dhall
let server = ./server.dhall
in
{ name = "my-project"
, dependencies = client.dependencies # server.dependencies
, packages = ./packages.dhall
, sources = [ "src/client/**/*.purs", "src/shared/**/*.purs", "src/server/**/*.purs" ]
}
