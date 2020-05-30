module Server.Main where

import Prelude hiding (between)

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)
import Network.HTTP.Types (status200, status404)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai (Application, Request(..), responseFile, responseStr)
import Network.Warp.Run (runSettings)
import Network.Warp.Settings (defaultSettings)
import Routing.Duplex (parse)
import Shared.Routes (Route(..), serverRoutes)

main :: Effect Unit
main = do
  let beforeMainLoop = do
        log $ "Open file via http://localhost:" <> show defaultSettings.port
  void $ runSettings defaultSettings { beforeMainLoop = beforeMainLoop } app

app :: Application
app (Request req) f = case parse serverRoutes req.rawPathInfo of
  Left _ -> do
    f $ responseStr status404 [(hContentType /\ "text/plain")] "File not found."
  Right route -> case route of
    Home -> do
      f $ responseFile status200 [(hContentType /\ "text/html")] "./dist/client/index.html" Nothing
    HalogenFile -> do
      f $ responseFile status200 [(hContentType /\ "text/javascript")] "./dist/client/app.js" Nothing
    GraphFile -> do
      f $ responseFile status200 [(hContentType /\ "text/plain")] "./module-graph.json" Nothing
