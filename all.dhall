let packages = ./packages.dhall
let Package = { dependencies : List Text, repo : Text, version : Text }
let PackageAssoc = { mapKey : Text, mapValue : Package }
let getPackageName = \(v : PackageAssoc) -> v.mapKey
let List/map = https://prelude.dhall-lang.org/List/map
in
  { name = "acme"
  , dependencies =
        List/map PackageAssoc Text getPackageName (toMap packages)
  , packages = packages
  , sources = [ ] : List Text
  }
