module Server.Main where

import Prelude hiding (between)

import Data.Either (Either(..))
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
import Shared.Parser (pursGraphOutputParser)
import Text.Parsing.StringParser (ParseError(..), runParser)

main :: Effect Unit
main = launchAff_ do
  fileContent <- readTextFile UTF8 "./module-graph.json"
  case runParser pursGraphOutputParser fileContent of
    Left parseError -> do
      liftEffect $ log $ mkErrorMessage parseError
    Right result -> do
      let
        moduleList = result

        settings = defaultSettings
              { beforeMainLoop = log $ "Open file via " <> baseUrl
              , port = port
              }
      liftEffect $ void $ runSettings settings app

mkErrorMessage :: ParseError -> String
mkErrorMessage (ParseError str) =
  "Error Message: " <> str
