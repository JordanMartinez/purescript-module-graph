module Server.Main where

import Prelude hiding (between)

import Data.Either (Either(..))
import Data.String.CodeUnits (drop, take)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Server.Parser (pursGraphOutputParser)
import Text.Parsing.StringParser (ParseError(..), unParser)

main :: Effect Unit
main = launchAff_ do
  file <- readTextFile UTF8 "./module-graph.json"
  case unParser pursGraphOutputParser { pos: 0, str: file } of
    Left parseError -> liftEffect $ log $
      "Text parsed so far: `" <> take parseError.pos file <> "`\n\n\
      \Parser error at position: " <> show parseError.pos <> "\n\
      \Error Message: " <> (case parseError.error of ParseError str -> str) <>
      "\nPrev 30 characters: `" <> (take 30 (drop (parseError.pos - 30) file)) <> "`\n\
      \nNext 30 characters: `" <> (take 30 (drop parseError.pos file)) <> "`"
    Right a -> liftEffect $ log $
      "Succesfully parsed `module-graph.json` content."
