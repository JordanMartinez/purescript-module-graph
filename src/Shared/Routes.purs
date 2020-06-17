module Shared.Routes where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Data.Lens.Iso.Newtype (_Newtype)
import Routing.Duplex (RouteDuplex', root, segment)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Shared.Types (Module, Package)

data PageRoute
  = Home
  | HalogenFile
  | MainCss
  -- | Gets a list of all modules in the package set. Mainly for
  -- | listing the modules one could query in a dropdown or something.
  | ModuleList
  -- | Gets the dependency and dependent graph of the module
  | ModuleGraph Module
  -- | Gets a list of all packages in the package set. Mainly for
  -- | listing the packages one could query in a dropdown or something.
  | PackageList
  -- | Gets the dependency and dependent graph of the package
  | PackageGraph Package

derive instance eqPageRoute :: Eq PageRoute
derive instance genericPageRoute :: Generic PageRoute _

pageRoutes :: RouteDuplex' PageRoute
pageRoutes = root $ sum
  { "Home": noArgs
  , "HalogenFile": "app.js" / noArgs
  , "MainCss" : "main.css" / noArgs
  , "ModuleList": "api" / "modules" / noArgs
  , "ModuleGraph": "api" / "modules" / module'
  , "PackageList": "api" / "packages" / noArgs
  , "PackageGraph": "api" / "packages" / package
  }

module' :: RouteDuplex' Module
module' = _Newtype segment

package :: RouteDuplex' Package
package = _Newtype segment
