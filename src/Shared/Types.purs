module Shared.Types where

import Prelude
import Data.Hashable (class Hashable)
import Data.Newtype (class Newtype)

newtype AllInfo = AllInfo
  { modName :: Module
  , package :: Package
  , version :: Version
  , path :: PathToFile
  , dependencies :: Array Module
  }
derive instance eqAllInfo :: Eq AllInfo
derive instance ordAllInfo :: Ord AllInfo
derive newtype instance hashableAllInfo :: Hashable AllInfo
derive instance newtypeAllInfo :: Newtype AllInfo _

newtype ModuleInfo = ModuleInfo
  { package :: Package
  , version :: Version
  , path :: PathToFile
  , dependencies :: Array Module
  }
derive instance eqModuleInfo :: Eq ModuleInfo
derive instance ordModuleInfo :: Ord ModuleInfo
derive newtype instance hashableModuleInfo :: Hashable ModuleInfo
derive instance newtypeModuleInfo :: Newtype ModuleInfo _

newtype Module = Module String
derive instance eqModule :: Eq Module
derive instance ordModule :: Ord Module
derive newtype instance hashableModule :: Hashable Module
derive instance newtypeModule :: Newtype Module _

newtype Package = Package String
derive instance eqPackage :: Eq Package
derive instance ordPackage :: Ord Package
derive newtype instance hashablePackage :: Hashable Package
derive instance newtypePackage :: Newtype Package _

newtype PathToFile = PathToFile String
derive instance eqPathToFile :: Eq PathToFile
derive instance ordPathToFile :: Ord PathToFile
derive newtype instance hashablePathToFile :: Hashable PathToFile
derive instance newtypePathToFile :: Newtype PathToFile _

newtype Version = Version String
derive instance eqVersion :: Eq Version
derive instance ordVersion :: Ord Version
derive newtype instance hashableVersion :: Hashable Version
derive instance newtypeVersion :: Newtype Version _
