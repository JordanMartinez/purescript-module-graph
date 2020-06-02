let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.6-20200507/packages.dhall sha256:9c1e8951e721b79de1de551f31ecb5a339e82bbd43300eb5ccfb1bf8cf7bbd62

let overrides =
  { halogen-select = upstream.halogen-select // { version = "master", depends = [ "halogen", "halogen-hooks", "halogen-hooks-extra" ] }}

let additions =
  { halogen-svg =
    { dependencies = [ "strings", "halogen", "dom-indexed" ]
    , repo = "https://github.com/statebox/purescript-halogen-svg.git"
    , version = "master"
    }

  , wai =
    { dependencies = [ "http-types"
        , "node-buffer"
        , "node-http"
        , "node-net"
        , "node-streams"
        , "node-url"
        ]
    , repo = "https://github.com/Woody88/purescript-wai.git"
    , version = "master"
    }
  , warp =
    { dependencies = [ "node-fs-aff"
        , "node-net"
        , "node-url"
        , "wai"
        ]
    , repo = "https://github.com/Woody88/purescript-warp.git"
    , version = "master"
    }
  , http-types =
    { dependencies =
        [ "console"
        , "effect"
        , "psci-support"
        , "tuples"
        , "unicode"
        , "uri"
        ]
    , repo =
        "https://github.com/Woody88/purescript-http-types.git"
    , version =
        "master"
    }
  }

in  upstream // overrides // additions
