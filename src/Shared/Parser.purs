module Shared.Parser where

import Prelude hiding (between)

import Data.List (List)
import Data.List.Types (NonEmptyList)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Text.Parsing.StringParser (Parser, try)
import Text.Parsing.StringParser.CodeUnits (regex, string)
import Text.Parsing.StringParser.Combinators (between, sepBy, sepBy1, (<?>))

newtype AllInfo = AllInfo
  { modName :: Module
  , package :: Package
  , version :: Version
  , path :: PathToFile
  , dependencies :: List Module
  }

newtype Module = Module String
derive instance newtypeModule :: Newtype Module _

newtype Package = Package String
derive instance newtypePackage :: Newtype Package _

newtype PathToFile = PathToFile String
derive instance newtypePathToFile :: Newtype PathToFile _

newtype Version = Version String
derive instance newtypeVersion :: Newtype Version _

pursGraphOutputParser :: Parser (NonEmptyList AllInfo)
pursGraphOutputParser =
  between openCurlyBrace closeCurlyBrace $
    wholeModule `sepBy1` comma

-- | `{"<module>":<module info>}`
wholeModule :: Parser AllInfo
wholeModule = ado
  modName <- Module <$> quotedModulePath
  void $ colon *> openCurlyBrace *> quotedPath *> colon *> quoteChar
  Tuple package version <- try ado
    package <- string ".spago/" *> (Package <$> pathPiece)
    version <- string "/" *> (Version <$> pathPiece)
    in Tuple package version
  path <- PathToFile <$> quotedFilePath
  dependencies' <- comma *> dependencies
  void closeCurlyBrace
  in AllInfo { modName, package, version, path, dependencies: dependencies' }

-- | `"path":"<file path>"`
pathValue :: Parser PathToFile
pathValue = ado
  filePath <- quotedPath *> colon *> quotedFilePath
  in PathToFile filePath

-- | `"depends":["<module path", "<module path>"]`
dependencies :: Parser (List Module)
dependencies =
  quotedDepends *> colon *> (between openBracket closeBracket modules)

-- | `"<module path>","<module path>", ...,"<module path>"`
modules :: Parser (List Module)
modules =
  (Module <$> quotedModulePath) `sepBy` comma

-- | `src/Path/To/Name.purs`
filePath :: Parser String
filePath = regex "[a-zA-Z0-9-/.]+" <?> "Could not match file path"

-- | `Data.Foo.Bar.Baz`
modulePath :: Parser String
modulePath = regex "[a-zA-Z0-9.]+" <?> "Could not match module path"

pathPiece :: Parser String
pathPiece = regex "[^/]+"

quotedModulePath :: Parser String
quotedModulePath = (between quoteChar quoteChar modulePath)
                    <?> "Could not match `\"<module path>\"`"

quotedFilePath :: Parser String
quotedFilePath = (between quoteChar quoteChar filePath)
                    <?> "Could not match `\"<file path>\"`"

path :: Parser String
path = string "path" <?> "Could not match `path`"

quotedPath :: Parser String
quotedPath = (between quoteChar quoteChar path) <?> "Could not match \"path\""

depends :: Parser String
depends = string "depends" <?> "Could not match `depends`"

quotedDepends :: Parser String
quotedDepends = (between quoteChar quoteChar depends) <?> "Could not match `\"dependes\"`"

quoteChar :: Parser String
quoteChar = string "\"" <?> "Could not match double-quote character"

colon :: Parser String
colon = string ":" <?> "Could not match colon character"

comma :: Parser String
comma = string "," <?> "Could not match comma character"

openBracket :: Parser String
openBracket = string "[" <?> "Could not match opening bracket character"

closeBracket :: Parser String
closeBracket = string "]" <?> "Could not match closing bracket character"

openCurlyBrace :: Parser String
openCurlyBrace = string "{" <?> "Could not match opening bracket character"

closeCurlyBrace :: Parser String
closeCurlyBrace = string "}" <?> "Could not match closing bracket character"
