module Shared.Codec where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray, fromNonEmpty, uncons)
import Data.Codec.Argonaut (JsonCodec, array, string)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.NonEmpty (NonEmpty(..))
import Data.Profunctor (dimap, wrapIso)
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

nonEmptyArrayCodec :: forall a. JsonCodec a -> JsonCodec (NonEmptyArray a)
nonEmptyArrayCodec codec =
    dimap uncons recons record
  where
    record = CA.object "NonEmptyArray" $ CAR.record
      { head: codec
      , tail: array codec
      }

    recons { head, tail } = fromNonEmpty (NonEmpty head tail)
