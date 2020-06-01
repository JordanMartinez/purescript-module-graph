module Server.App where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Network.HTTP.Types (status200, status404)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai (Application, Request(..), responseFile, responseStr)
import Routing.Duplex (parse)
import Shared.Routes (PageRoute(..), pageRoutes)

app :: Application
app (Request req) f = case parse pageRoutes req.rawPathInfo of
  Right route -> case route of
    Home -> do
      f $ responseFile status200 [(hContentType /\ "text/html")] "./dist/index.html" Nothing
    HalogenFile -> do
      f $ responseFile status200 [(hContentType /\ "text/javascript")] "./dist/app.js" Nothing
    ModuleList -> do
      f $ responseStr status200 [(hContentType /\ "application/json")] "Not yet implemented."
    ModuleDependency modName -> do
      f $ responseStr status200 [(hContentType /\ "application/json")] "Not yet implemented."
    PackageList -> do
      f $ responseStr status200 [(hContentType /\ "application/json")] "Not yet implemented."
    PackageDependency package -> do
      f $ responseStr status200 [(hContentType /\ "application/json")] "Not yet implemented."
  Left _ -> do
    f $ responseStr status404 [(hContentType /\ "text/plain")] "File not found."
