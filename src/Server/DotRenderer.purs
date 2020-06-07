module Server.DotRenderer where

import Prelude

import Control.Alt ((<|>))
import Data.Array (catMaybes, elemIndex, filter, foldl, nub, snoc)
import Data.Foldable (class Foldable)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.HashMap (HashMap, lookup)
import Data.HashMap as HashMap
import Data.Interpolate (i)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (un)
import Effect.Aff (Aff)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (writeTextFile)
import Shared.Types (Module, ModuleInfo(..), Package(..))

mkPackageGraph :: HashMap Module ModuleInfo -> HashMap Package (Array Package)
mkPackageGraph moduleMap = foldl buildMap HashMap.empty moduleMap
  where
  buildMap :: HashMap Package (Array Package) -> ModuleInfo -> HashMap Package (Array Package)
  buildMap acc (ModuleInfo rec) =
    let
      pList = nub $ catMaybes $ getPList <$> rec.dependencies
      packageList = filter (_ /= rec.package) pList
    in HashMap.upsert (\x -> nub (x <|> packageList)) rec.package packageList acc

  getPList :: Module -> Maybe Package
  getPList mod =
    (\(ModuleInfo rec) -> rec.package) <$> lookup mod moduleMap

-- | Renders something like this via `dot`:
-- | ```
-- | strict digraph root {
-- |   rankdir=LR
-- |   subgraph cluster_1 {
-- |     "my-package-name" [style=filled, fillcolor=lightblue]
-- |   }
-- |
-- |   subgraph cluster_2 {
-- |     label = "Dependencies"
-- |     "my-package-name" -> { "dependency1" "dependency2" "dependency3" "..." }
-- |   }
-- |
-- |   subgraph cluster_3 {
-- |     label = "Dependents"
-- |     { "dependent1" "dependent2" "dependent3" "..." } -> "my-package-name"
-- |   }
-- | }
-- | ```
renderPackageGraph :: Package -> HashMap Package (Array Package) -> String -> Aff Unit
renderPackageGraph p packageMap file = do
  let
    mainPkg = quotePackage p
    dependencies = fromMaybe [] $ lookup p packageMap
    dependents = foldlWithIndex extractDependents [] packageMap

    separateElemsBySpaces arrayOfValues =
      intercalate0 " " quotePackage arrayOfValues

  writeTextFile UTF8 file $ i
    "strict digraph root {\n\
    \rankdir = LR\
    \  subgraph cluster_1 {\n\
    \    "mainPkg" [style=filled, fillcolor=lightblue]\n\
    \  }\n\
    \  subgraph cluster_2 {\n\
    \    label = \"Dependencies\"\n\
    \    "mainPkg" -> {"(separateElemsBySpaces dependencies)"}\n\
    \  }\n\
    \  subgraph cluster_3 {\n\
    \    label = \"Dependents\"\n\
    \    {"(separateElemsBySpaces dependents)"} -> "mainPkg"\n\
    \  }\n\
    \}"

  where
    quotePackage :: Package -> String
    quotePackage pkg = "\"" <> (un Package pkg) <> "\""

    intercalate0 :: forall f a b. Foldable f => Monoid b => b -> (a -> b) -> f a -> b
    intercalate0 sep toMonoid xs =
      (foldl f {init: true, value: mempty } xs).value
      where
        f acc next =
          if acc.init then {init: false, value: toMonoid next }
          else acc { value = acc.value <> sep <> (toMonoid next) }

    extractDependents :: Package -> Array Package -> Array Package -> Array Package
    extractDependents keyPackage acc packDeps =
      if p `elemIndex` packDeps /= Nothing then
        acc `snoc` keyPackage
      else
        acc
