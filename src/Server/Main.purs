module Server.Main where

import Prelude hiding (between)

import Control.Alt ((<|>))
import Data.Array (catMaybes, filter, foldM, nub, null, sort, sortBy)
import Data.Array.NonEmpty (singleton, snoc, uncons)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.FoldableWithIndex (foldWithIndexM, foldlWithIndex)
import Data.HashMap (HashMap, lookup, toArrayBy)
import Data.HashMap as HashMap
import Data.HashSet as Set
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.Newtype (un)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Network.Warp.Run (runSettings)
import Network.Warp.Settings (defaultSettings)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (appendTextFile, readTextFile, writeTextFile)
import Server.App (app)
import Server.DotRenderer (mkPackageGraph, renderPackageGraph)
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
      liftEffect $ log "Successful parse. Now making graph."
      renderPackageGraph (Package "halogen-hooks") (mkPackageGraph a.result) "halogen-hooks-graph.dot"
      let
        env =
          { allInfo: a.result
          , packageGraph: mkPackageGraph a.result
          }
        settings = defaultSettings
          { beforeMainLoop = log $ "Open file via" <> baseUrl
          , port = port
          }
      liftEffect $ void $ runSettings settings (app env)

mkErrorMessage :: ParseError -> String
mkErrorMessage (ParseError str) =
  "Error Message: " <> str
