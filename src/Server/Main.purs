module Server.Main where

import Prelude hiding (between)

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (drop, take)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Network.HTTP.Types (status200, status404)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai (Application, Request(..), responseFile, responseStr)
import Network.Warp.Run (runSettings)
import Network.Warp.Settings (defaultSettings)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Routing.Duplex (parse)
import Shared.Config (baseUrl, port)
import Shared.Parser (pursGraphOutputParser)
import Shared.Routes (PageRoute(..), pageRoutes)
import Text.Parsing.StringParser (ParseError(..), Pos, unParser)

main :: Effect Unit
main = launchAff_ do
  file <- readTextFile UTF8 "./module-graph.json"
  case unParser pursGraphOutputParser { pos: 0, str: file } of
    Left parseError -> do
      liftEffect $ log $ mkErrorMessage parseError file
    Right parseResult -> do
      let
        moduleList = parseResult.result
        settings = defaultSettings
              { beforeMainLoop = log $ "Open file via " <> baseUrl
              , port = port
              }
      liftEffect $ void $ runSettings settings app

app :: Application
app (Request req) f = case parse pageRoutes req.rawPathInfo of
  Left _ -> do
    f $ responseStr status404 [(hContentType /\ "text/plain")] "File not found."
  Right route -> case route of
    Home -> do
      f $ responseFile status200 [(hContentType /\ "text/html")] "./dist/index.html" Nothing
    HalogenFile -> do
      f $ responseFile status200 [(hContentType /\ "text/javascript")] "./dist/app.js" Nothing

mkErrorMessage :: { error :: ParseError, pos :: Pos } -> String -> String
mkErrorMessage parseError text =
  "Text parsed so far: `" <> take parseError.pos text <> "`\n\n\
  \Parser error at position: " <> show parseError.pos <> "\n\
  \Error Message: " <> (case parseError.error of ParseError str -> str) <> "\n\
  \Prev 30 characters: `" <> (take 30 (drop (parseError.pos - 30) text)) <> "`\n\
  \nNext 30 characters: `" <> (take 30 (drop parseError.pos text)) <> "`"
