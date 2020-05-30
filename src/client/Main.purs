module Client.Main where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Client.Parser (Dependency(..), Module(..), ModuleInfo(..), Path(..), pursGraphOutputParser)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Monoid (power)
import Data.String.CodeUnits (drop, take)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Halogen (liftAff)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.Hooks as Hooks
import Halogen.VDom.Driver (runUI)
import Text.Parsing.StringParser (ParseError(..), unParser)

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  loadingIO <- runUI loading unit body

  reqResult <- AX.request (AX.defaultRequest { url = "http://localhost:3000/", method = Left GET, responseFormat = AXRF.string })
  let parseResults = case reqResult of
        Left err -> Left $
          "GET http://localhost:3000/ response failed to decode: " <> AX.printError err
        Right response -> do
          let text = response.body
          case unParser pursGraphOutputParser { pos: 0, str: response.body } of
            Left parseError -> Left $
              "Text parsed so far: `" <> take parseError.pos text <> "`\n\n\
              \Parser error at position: " <> show parseError.pos <> "\n\
              \Error Message: " <> (case parseError.error of ParseError str -> str) <>
              "\nPrev 30 characters: `" <> (take 30 (drop (parseError.pos - 30) text)) <> "`\n\
              \nNext 30 characters: `" <> (take 30 (drop parseError.pos text)) <> "`"
            Right parseResult -> Right parseResult.result

  loadingIO.dispose
  case parseResults of
    Left errorMessage -> do
      runUI displayError errorMessage body
    Right fileContent -> do
      runUI displayGraph fileContent body


loading :: forall q i o. H.Component HH.HTML q i o Aff
loading = Hooks.component \_ _ -> Hooks.do
  state /\ stateId <- Hooks.useState 0
  Hooks.useLifecycleEffect do
    id <- Hooks.fork do
      void $ liftAff do
        delay (Milliseconds 333.0)
      Hooks.modify_ stateId (\s -> if s == 3 then 0 else s + 1)
    pure $ Just do
      Hooks.kill id

  Hooks.pure $
    HH.h1_
      [ HH.text $ "Loading" <> (power "." state) ]
  Hooks.pure $
    HH.div_
