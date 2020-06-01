module Shared.Routes where

import Prelude

import Data.Generic.Rep (class Generic)
import Routing.Duplex (RouteDuplex', path, root)
import Routing.Duplex.Generic as G

data PageRoute
  = Home
  | HalogenFile

derive instance eqPageRoute :: Eq PageRoute
derive instance genericPageRoute :: Generic PageRoute _

pageRoutes :: RouteDuplex' PageRoute
pageRoutes = root $ G.sum
  { "Home": G.noArgs
  , "HalogenFile": path "app.js" G.noArgs
  }
