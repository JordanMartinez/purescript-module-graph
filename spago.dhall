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
, sources = [ "src/Client/**/*.purs", "src/Shared/**/*.purs", "src/Server/**/*.purs" ]
}
