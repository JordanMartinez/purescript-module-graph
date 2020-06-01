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
derive instance eqModule :: Eq Module
derive instance newtypeModule :: Newtype Module _

newtype Package = Package String
derive instance eqPackage :: Eq Package
derive instance newtypePackage :: Newtype Package _

newtype PathToFile = PathToFile String
derive instance eqPathToFile :: Eq PathToFile
derive instance newtypePathToFile :: Newtype PathToFile _

newtype Version = Version String
derive instance eqVersion :: Eq Version
derive instance newtypeVersion :: Newtype Version _

pursGraphOutputParser :: Parser (NonEmptyList AllInfo)
pursGraphOutputParser =
  betweenCurlyBraces $ wholeModule `sepBy1` comma

-- | `{"<module>":<module info>}`
wholeModule :: Parser AllInfo
wholeModule = ado
  modName <- Module <$> (quoted modulePath)
  void $ colon *> openCurlyBrace *> (quoted pathWord) *> colon *> quoteChar
  Tuple package version <- try ado
    package <- string ".spago/" *> (Package <$> pathPiece)
    version <- string "/" *> (Version <$> pathPiece)
    in Tuple package version
  path <- PathToFile <$> (quoted filePath)
  dependencies' <- comma *> dependencies
  void closeCurlyBrace
  in AllInfo { modName, package, version, path, dependencies: dependencies' }

-- | `"path":"<file path>"`
pathValue :: Parser PathToFile
pathValue = ado
  filePath <- (quoted pathWord) *> colon *> (quoted filePath)
  in PathToFile filePath

-- | `"depends":["<module path", "<module path>"]`
dependencies :: Parser (List Module)
dependencies =
  (quoted dependsWord) *> colon *> (betweenBrackets modules)

-- | `"<module path>","<module path>", ...,"<module path>"`
modules :: Parser (List Module)
modules =
  (Module <$> (quoted modulePath)) `sepBy` comma

-- Single Pieces

-- | `src/Path/To/Name.purs`
filePath :: Parser String
filePath = regex "[a-zA-Z0-9-/.]+" <?> "Could not match file path"

-- | `Data.Foo.Bar.Baz`
modulePath :: Parser String
modulePath = regex "[a-zA-Z0-9.]+" <?> "Could not match module path"

pathPiece :: Parser String
pathPiece = regex "[^/]+"

pathWord :: Parser String
pathWord = string "path" <?> "Could not match `path`"

dependsWord :: Parser String
dependsWord = string "depends" <?> "Could not match `depends`"

-- Combinators

quoted :: forall a. Parser a -> Parser a
quoted = between quoteChar quoteChar

betweenCurlyBraces :: forall a. Parser a -> Parser a
betweenCurlyBraces = between openCurlyBrace closeCurlyBrace

betweenBrackets :: forall a. Parser a -> Parser a
betweenBrackets = between openBracket closeBracket

-- Single characters

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
