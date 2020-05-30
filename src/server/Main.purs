module Server.Main where

import Prelude hiding (between)

import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)
import Effect.Unsafe (unsafePerformEffect)
import Network.HTTP.Types (status200, status404)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai (Application, Request(..), responseFile, responseStr)
import Network.Warp.Run (runSettings)
import Network.Warp.Settings (defaultSettings)

main :: Effect Unit
main = do
  let beforeMainLoop = do
        log $ "Listening on port " <> show defaultSettings.port
  void $ runSettings defaultSettings { beforeMainLoop = beforeMainLoop } app

app :: Application
app req f =
  let _ = unsafePerformEffect $ log $ show (un Request req).pathInfo
  in case (un Request req).pathInfo of
  ["", ""] -> do
    f $ responseFile status200 [(hContentType /\ "text/html")] "./dist/client/index.html" Nothing
  ["", "app.js"] -> do
    f $ responseFile status200 [(hContentType /\ "text/javascript")] "./dist/client/app.js" Nothing
  ["", "api", "graphFile" ] -> do
    f $ responseFile status200 [(hContentType /\ "text/plain")] "./module-graph.json" Nothing

  _ -> do
    f $ responseStr status404 [(hContentType /\ "text/plain")] "File not found."
