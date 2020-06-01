module Server.Main where

import Prelude hiding (between)

import Data.Array.NonEmpty (singleton, snoc, uncons)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.HashSet as Set
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Network.Warp.Run (runSettings)
import Network.Warp.Settings (defaultSettings)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Server.App (app)
import Shared.Config (baseUrl, port)
import Shared.Parser (AllInfo(..), pursGraphOutputParser)
import Text.Parsing.StringParser (ParseError(..), runParser)

main :: Effect Unit
main = launchAff_ do
  fileContent <- readTextFile UTF8 "./module-graph.json"
  case runParser pursGraphOutputParser fileContent of
    Left parseError -> do
      liftEffect $ log $ mkErrorMessage parseError
    Right resultArray -> do
      let
        { head: (AllInfo a), tail } = uncons resultArray
        initialValue = { modNames: singleton a.modName, packages: Set.singleton a.package }
        extractInfo acc (AllInfo rec) =
          acc { modNames = acc.modNames `snoc` rec.modName
              , packages = Set.insert rec.package acc.packages
              }

        { modNames, packages } = foldl extractInfo initialValue tail
        settings = defaultSettings
              { beforeMainLoop = log $ "Open file via " <> baseUrl
              , port = port
              }
      liftEffect $ void $ runSettings settings app

mkErrorMessage :: ParseError -> String
mkErrorMessage (ParseError str) =
  "Error Message: " <> str
