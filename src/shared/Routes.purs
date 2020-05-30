module Shared.Routes where

import Prelude

import Data.Generic.Rep (class Generic)
import Routing.Duplex (RouteDuplex', path, root)
import Routing.Duplex.Generic as G

data Route
  = Home
  | HalogenFile
  | GraphFile

derive instance genericRoute :: Generic Route _

serverRoutes :: RouteDuplex' Route
serverRoutes = root $ G.sum
  { "Home": G.noArgs
  , "HalogenFile": path "app.js" G.noArgs
  , "GraphFile": path "api" (path "file" G.noArgs)
  }
