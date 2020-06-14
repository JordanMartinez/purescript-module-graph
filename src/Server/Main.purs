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
import Node.FS.Aff (exists, mkdir, readTextFile)
import Node.Path as Path
import Server.App (app)
import Server.DotRenderer (mkPackageGraph)
import Shared.Config (baseUrl, port)
import Shared.Parser (pursGraphOutputParser)
import Text.Parsing.StringParser (ParseError(..), unParser)

main :: Effect Unit
main = launchAff_ do
  let
    dotFilesFolder = Path.concat ["dist", "dotFiles"]
    imagesFolder = Path.concat ["dist", "images"]
  unlessM (exists dotFilesFolder) do
    mkdir dotFilesFolder
  unlessM (exists imagesFolder) do
    mkdir imagesFolder
  fileContent <- readTextFile UTF8 "./module-graph.json"
  case unParser pursGraphOutputParser {pos: 0, str: fileContent} of
    Left e -> do
      liftEffect $ log $ show e.pos <> show e.error
    Right a -> do
      liftEffect $ log "Successful parse. Now making graph."
      let
        env =
          { allInfo: a.result
          , packageGraph: mkPackageGraph a.result
          }
        settings = defaultSettings
          { beforeMainLoop = log $ "Open file via " <> baseUrl
          , port = port
          }
      liftEffect $ void $ runSettings settings (app env)

mkErrorMessage :: ParseError -> String
mkErrorMessage (ParseError str) =
  "Error Message: " <> str
