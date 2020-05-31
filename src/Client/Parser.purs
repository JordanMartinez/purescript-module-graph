module Client.Parser where

import Prelude hiding (between)

import Data.List (List)
import Data.List.Types (NonEmptyList)
import Data.Newtype (class Newtype)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodeUnits (regex, string)
import Text.Parsing.StringParser.Combinators (between, sepBy, sepBy1, (<?>))

newtype Module = Module { name :: String, info :: ModuleInfo }
derive instance newtypeModule :: Newtype Module _

newtype ModuleInfo = ModuleInfo { path :: Path, depends :: List Dependency }
derive instance newtypeModuleInfo :: Newtype ModuleInfo _

newtype Package = Package String
derive instance newtypePackage :: Newtype Package _

newtype Path = Path String
derive instance newtypePath :: Newtype Path _

newtype Dependency = Dependency String
derive instance newtypeDependency :: Newtype Dependency _

pursGraphOutputParser :: Parser (NonEmptyList Module)
pursGraphOutputParser =
  between openCurlyBrace closeCurlyBrace $
    wholeModule `sepBy1` comma

-- | `{"<module>":<module info>}`
wholeModule :: Parser Module
wholeModule = ado
  name <- quotedModulePath
  info <- colon *> moduleInfo
  in Module { name, info }

-- | `{"path":"<file path>", "depends":["<module path>", "<module path>"]}`
moduleInfo :: Parser ModuleInfo
moduleInfo =
  between openCurlyBrace closeCurlyBrace ado
    path <- pathValue
    void comma
    depends <- dependencies
    in ModuleInfo { path, depends }

-- | `"path":"<file path>"`
pathValue :: Parser Path
pathValue = ado
  filePath <- quotedPath *> colon *> quotedFilePath
  in Path filePath

-- | `"depends":["<module path", "<module path>"]`
dependencies :: Parser (List Dependency)
dependencies =
  quotedDepends *> colon *> (between openBracket closeBracket modules)

-- | `"<module path>","<module path>", ...,"<module path>"`
modules :: Parser (List Dependency)
modules =
  (Dependency <$> quotedModulePath) `sepBy` comma

-- | `src/Path/To/Name.purs`
filePath :: Parser String
filePath = regex "[a-zA-Z0-9-/.]+" <?> "Could not match file path"

-- | `Data.Foo.Bar.Baz`
modulePath :: Parser String
modulePath = regex "[a-zA-Z0-9.]+" <?> "Could not match module path"

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
