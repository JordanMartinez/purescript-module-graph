module Server.Main where

import Prelude hiding (between)

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (drop, take)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai (Application, responseFile, responseStr)
import Network.Warp.Run (runSettings)
import Network.Warp.Settings (defaultSettings)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Server.Parser (pursGraphOutputParser)
import Text.Parsing.StringParser (ParseError(..), unParser)

main :: Effect Unit
main = do
  let beforeMainLoop = do
        log $ "Listening on port " <> show defaultSettings.port
  void $ runSettings defaultSettings { beforeMainLoop = beforeMainLoop } app

app :: Application
app req f = do
    f $ responseFile status200 [(hContentType /\ "text/plain")] "./module-graph.json" Nothing
