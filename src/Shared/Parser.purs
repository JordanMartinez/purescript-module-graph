module Shared.Parser where

import Prelude hiding (between)

import Data.Array (fromFoldable)
import Data.Foldable (foldl)
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.Tuple (Tuple(..))
import Shared.Types (AllInfo(..), ModuleInfo(..), Module(..), Package(..), PathToFile(..), Version(..))
import Text.Parsing.StringParser (Parser, try)
import Text.Parsing.StringParser.CodeUnits (regex, string)
import Text.Parsing.StringParser.Combinators (between, sepBy, sepBy1, (<?>))

pursGraphOutputParser :: Parser (HashMap Module ModuleInfo)
pursGraphOutputParser =
  convertToHashMap <$> (betweenCurlyBraces $ wholeModule `sepBy1` comma)
  where
    convertToHashMap = foldl mapify HashMap.empty
    mapify acc (AllInfo { modName, package, version, path, dependencies: dependencies' }) =
      HashMap.insert modName (ModuleInfo { package, version, path, dependencies: dependencies' }) acc

-- | `{"<module>":<module info>}`
wholeModule :: Parser AllInfo
wholeModule = ado
  modName <- Module <$> (quoted modulePath)
  void $ colon *> openCurlyBrace *> (quoted pathWord) *> colon
  Tuple package version <- try ado
    package <- quoteChar *> string ".spago/" *> (Package <$> pathPiece)
    version <- string "/" *> (Version <$> pathPiece)
    in Tuple package version
  path <- PathToFile <$> (quoted filePath)
  dependencies' <- comma *> dependencies
  void closeCurlyBrace
  in AllInfo { modName, package, version, path, dependencies: dependencies' }

-- | `"depends":["<module path", "<module path>"]`
dependencies :: Parser (Array Module)
dependencies =
  (quoted dependsWord) *> colon *> (betweenBrackets modules)

-- | `"<module path>","<module path>", ...,"<module path>"`
modules :: Parser (Array Module)
modules =
  fromFoldable <$> (Module <$> (quoted modulePath)) `sepBy` comma

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
quoted = between
  (quoteChar <?> "Opening quote not matched")
  (quoteChar <?> "Closing quote not matched")

betweenCurlyBraces :: forall a. Parser a -> Parser a
betweenCurlyBraces = between
  (openCurlyBrace <?> "Opening curly brace not matched")
  (closeCurlyBrace <?> "Closing curly brace not matched")

betweenBrackets :: forall a. Parser a -> Parser a
betweenBrackets = between
  (openBracket <?> "Opening bracket not matched")
  (closeBracket <?> "Closing bracket not matched")

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
