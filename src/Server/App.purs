module Server.App where

import Prelude

import Data.Argonaut (Json)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Codec as Codec
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Network.HTTP.Types (status200, status404)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai (Application, Request(..), responseFile, responseStr)
import Routing.Duplex (parse)
import Shared.Codec (moduleCodec, nonEmptyArrayCodec, packageCodec)
import Shared.Routes (PageRoute(..), pageRoutes)
import Shared.Types (AllInfo, Module, Package)
import Unsafe.Coerce (unsafeCoerce)

type Env =
  { modNames :: NonEmptyArray Module
  , packages :: NonEmptyArray Package
  , allInfo :: NonEmptyArray AllInfo
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
        jsonToString encodedModuleList
    ModuleDependency modName -> do
      f $ responseStr status200 [(hContentType /\ "application/json")] $
        "Not yet implemented."
    PackageList -> do
      f $ responseStr status200 [(hContentType /\ "application/json")] $
        jsonToString encodePackageList
    PackageDependency package -> do
      f $ responseStr status200 [(hContentType /\ "application/json")] "Not yet implemented."
  Left _ -> do
    f $ responseStr status404 [(hContentType /\ "text/plain")] "File not found."
  where
    jsonToString :: Json -> String
    jsonToString = unsafeCoerce

    encodedModuleList :: Json
    encodedModuleList =
      Codec.encode (nonEmptyArrayCodec moduleCodec) env.modNames

    encodePackageList :: Json
    encodePackageList =
      Codec.encode (nonEmptyArrayCodec packageCodec) env.packages
