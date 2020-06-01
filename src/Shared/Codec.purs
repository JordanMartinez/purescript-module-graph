module Shared.Codec where

import Prelude

import Data.Codec.Argonaut (JsonCodec, array, string)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Profunctor (wrapIso)
import Shared.Types (AllInfo(..), Module(..), Package(..), PathToFile(..), Version(..))

moduleCodec :: JsonCodec Module
moduleCodec = wrapIso Module string

packageCodec :: JsonCodec Package
packageCodec = wrapIso Package string

pathToFileCodec :: JsonCodec PathToFile
pathToFileCodec = wrapIso PathToFile string

versionCodec :: JsonCodec Version
versionCodec = wrapIso Version string

allInfoCodec :: JsonCodec AllInfo
allInfoCodec = wrapIso AllInfo $
  CA.object "AllInfo" $ CAR.record
    { modName: moduleCodec
    , package: packageCodec
    , path: pathToFileCodec
    , version: versionCodec
    , dependencies: array moduleCodec
    }
