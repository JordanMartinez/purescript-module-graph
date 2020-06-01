module Shared.Routes where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Data.Lens.Iso.Newtype (_Newtype)
import Routing.Duplex (RouteDuplex', root, segment)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Shared.Parser (Module, Package)

data PageRoute
  = Home
  | HalogenFile
  | ModuleList
  | ModuleDependency Module
  | PackageList
  | PackageDependency Package

derive instance eqPageRoute :: Eq PageRoute
derive instance genericPageRoute :: Generic PageRoute _

pageRoutes :: RouteDuplex' PageRoute
pageRoutes = root $ sum
  { "Home": noArgs
  , "HalogenFile": "app.js" / noArgs
  , "ModuleList": "api" / "module" / noArgs
  , "ModuleDependency": "api" / "module" / module'
  , "PackageList": "api" / "package" / noArgs
  , "PackageDependency": "api" / "package" / package
  }

module' :: RouteDuplex' Module
module' = _Newtype segment

package :: RouteDuplex' Package
package = _Newtype segment
