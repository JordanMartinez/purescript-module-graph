module Client.Main where

import Prelude

import Affjax (printError)
import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Affjax.StatusCode (StatusCode(..))
import Data.Array (cons, foldl)
import Data.Codec (decode)
import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Data.HTTP.Method (Method(..))
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Monoid (power)
import Data.Newtype (un)
import Data.String.CodeUnits (drop, take)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Halogen (liftAff)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.Hooks (useLifecycleEffect, useState)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Extra.Hooks (useEvent)
import Halogen.VDom.Driver (runUI)
import Network.RemoteData as RD
import Routing.Duplex (print)
import Select (SelectEvent(..), SelectReturn(..), selectInput, useSelect)
import Select as Select
import Shared.Codec (nonEmptyArrayCodec, packageCodec)
import Shared.Config (baseUrl)
import Shared.Routes (PageRoute(..), pageRoutes)
import Shared.Types (Module, Package)
import Text.Parsing.StringParser (ParseError(..), unParser)

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  runUI rootComponent unit body

rootComponent :: forall q i o. H.Component HH.HTML q i o Aff
rootComponent = Hooks.component \_ _ -> Hooks.do
  content /\ contentId <- useState $ RD.NotAsked
  packageEvents <- useEvent
  SelectReturn packageSelect <- useSelect $ selectInput
    { inputType = Select.Text
    , debounceTime = Just $ Milliseconds 300.0
    , pushNewSearch = packageEvents.push <<< NewSearch
    , pushSelectedIdxChanged = packageEvents.push <<< SelectedIndex
    }
  useLifecycleEffect do
    void $ packageEvents.setCallback $ Just \_ i -> case i of
      NewSearch str -> do
        H.put contentId RD.Loading
        reqResult <- liftAff $ AX.request $ AX.defaultRequest
              { url = baseUrl <> print pageRoutes $ Package str
              , method = Left GET
              , responseFormat = AXRF.json
              }
        case reqResult of
          Right response | response.statusCode == StatusCode 200 -> do
            let packageList = decode (nonEmptyArrayCodec packageCodec) respone.body
            H.put contentId $ Success packageList
          Right response -> do
            H.put contentID $ Falure $ "Status code: " <> show response.statusCode
          Left e -> do
            H.put contentId $ Failure $ "Error: " <> printError e

      SelectedIndex i -> do
        pure unit
      _ -> do
        pure unit

    pure Nothing

  Hooks.pure $
    HH.div_
      [ HH.text "To implement" ]

-- displayError :: forall q o. H.Component HH.HTML q String o Aff
-- displayError = Hooks.component \_ errorMessage -> Hooks.do
--   Hooks.pure $
--     HH.h1_
--       [ HH.text $ "Error: " <> errorMessage ]
--
-- displayGraph :: forall q o. H.Component HH.HTML q (NonEmptyList Module) o Aff
-- displayGraph = Hooks.component \_ moduleList -> Hooks.do
--   Hooks.pure $
--     HH.div_
--       [ HH.h1_ [ HH.text "Module List" ]
--       , HH.div_ $ mapToArray renderModule moduleList
--
--       ]
--   where
--     mapToArray :: forall f a b. Foldable f => (a -> b) -> f a -> Array b
--     mapToArray f =
--       foldl (\acc next -> f next `cons` acc) []
--
--     renderModule :: Module -> H.ComponentHTML _ _ Aff
--     renderModule (Module rec) = let info = un ModuleInfo rec.info in
--       HH.div_
--         [ HH.h2_
--           [ HH.text rec.name ]
--         , HH.p_ [ HH.text $ un Path info.path ]
--         , HH.ul_ $ mapToArray renderDependency info.depends
--         ]
--
--     renderDependency :: Dependency -> H.ComponentHTML _ _ Aff
--     renderDependency (Dependency dep) =
--       HH.li_ [ HH.text dep ]
