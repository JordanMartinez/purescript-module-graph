module Server.App where

import Prelude

import Data.Argonaut.Core (Json, stringify)
import Data.Codec as Codec
import Data.Codec.Argonaut as CA
import Data.Either (Either(..))
import Data.HashMap (HashMap, keys)
import Data.Interpolate (i)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Tuple.Nested ((/\))
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Network.HTTP.Types (status200, status404)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai (Application, Request(..), responseFile, responseStr)
import Node.FS.Aff (exists)
import Node.Path as Path
import Routing.Duplex (parse)
import Server.ChildProcess (execSync)
import Server.DotRenderer (renderPackageGraph)
import Shared.Codec (moduleCodec, packageCodec)
import Shared.Routes (PageRoute(..), pageRoutes)
import Shared.Types (Module, ModuleInfo, Package(..))

type Env =
  { allInfo :: HashMap Module ModuleInfo
  , packageGraph :: HashMap Package (Array Package)
  }

app :: Env -> Application
app env (Request req) f = case parse pageRoutes req.rawPathInfo of
  Right route -> case route of
    Home -> do
      f $ responseFile status200 [(hContentType /\ "text/html")] "./dist/index.html" Nothing
    HalogenFile -> do
      f $ responseFile status200 [(hContentType /\ "text/javascript")] "./dist/app.js" Nothing
    ModuleList -> do
      f $ responseStr status200 [(hContentType /\ "application/json")] $
        stringify encodedModuleList
    ModuleGraph modName -> do
      f $ responseStr status200 [(hContentType /\ "application/json")] $
        "Not yet implemented."
    PackageList -> do
      f $ responseStr status200 [(hContentType /\ "application/json")] $
        stringify encodePackageList
    PackageGraph package -> do
      let
        p = un Package package
        dotFile = Path.concat ["dist", "dotFiles", p <> ".dot"]
        svgFile = Path.concat ["dist", "images", p <> ".svg"]
      launchAff_ do
        unlessM (exists svgFile) do
          renderPackageGraph package env.packageGraph dotFile
          liftEffect $ execSync (i "dot -Tsvg -o "svgFile" "dotFile)

        liftEffect $ f $ responseFile status200 [(hContentType /\ "image/svg+xml")] svgFile Nothing
  Left _ -> do
    f $ responseStr status404 [(hContentType /\ "text/plain")] "File not found."
  where
    encodedModuleList :: Json
    encodedModuleList =
      Codec.encode (CA.array moduleCodec) $ keys env.allInfo

    encodePackageList :: Json
    encodePackageList =
      Codec.encode (CA.array packageCodec) $ keys env.packageGraph
