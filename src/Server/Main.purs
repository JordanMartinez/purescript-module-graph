module Server.Main where

import Prelude hiding (between)

import Control.Alt ((<|>))
import Data.Array (nub, null)
import Data.Array.NonEmpty (singleton, snoc, uncons)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.HashMap (HashMap, lookup)
import Data.HashMap as HashMap
import Data.HashSet as Set
import Data.Maybe (fromMaybe, maybe)
import Data.Newtype (un)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Network.Warp.Run (runSettings)
import Network.Warp.Settings (defaultSettings)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import Server.App (app)
import Shared.Config (baseUrl, port)
import Shared.Parser (pursGraphOutputParser)
import Shared.Types (AllInfo(..), ModuleInfo(..), Module(..), Package(..))
import Text.Parsing.StringParser (ParseError(..), runParser, unParser)

main :: Effect Unit
main = launchAff_ do
  fileContent <- readTextFile UTF8 "./module-graph.json"
  case unParser pursGraphOutputParser {pos: 0, str: fileContent} of
    Left e -> do
      liftEffect $ log $ show e.pos <> show e.error
    Right a -> do
      let
        modNames = HashMap.keys a.result
        graph = mkPackageGraph a.result
        quote s = "\"" <> s <> "\""
        renderToDotContent = \package acc deps ->
          if null deps then
            (quote (un Package package)) <> ";"
          else
            (quote (un Package package)) <> " -> " <>
            (foldl (\acc' n -> acc' <> ", " <> (quote (un Package n))) "" deps) <>
            ";"
        dotContent = foldlWithIndex renderToDotContent "" graph
      writeTextFile UTF8 "package-graph.dot" $
        "digraph G {\n" <> dotContent <> "\n}"
        -- settings = defaultSettings
        --       { beforeMainLoop = log $ "Open file via " <> baseUrl
        --       , port = port
        --       }
      -- liftEffect $ void $ runSettings settings (app { modNames, packages, resultArray })

  where
    mkPackageGraph :: HashMap Module ModuleInfo -> HashMap Package (Array Package)
    mkPackageGraph moduleMap =
      foldlWithIndex f HashMap.empty moduleMap
      where
        getPackageList :: Module -> Array Package
        getPackageList mod =
          maybe [] (\(ModuleInfo rec) -> join $ traverse getPackageList rec.dependencies) $ lookup mod moduleMap

        f :: Module -> HashMap Package (Array Package) -> ModuleInfo -> HashMap Package (Array Package)
        f mod acc (ModuleInfo rec) =
          let pList = join $ traverse getPackageList rec.dependencies
          in HashMap.upsert (\x -> nub (x <|> pList)) rec.package pList acc


mkErrorMessage :: ParseError -> String
mkErrorMessage (ParseError str) =
  "Error Message: " <> str
