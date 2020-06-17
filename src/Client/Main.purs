module Client.Main where

import Prelude

import Affjax (printError)
import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Affjax.StatusCode (StatusCode(..))
import Client.Utils (whenElem)
import Data.Array (mapWithIndex, unsafeIndex)
import Data.Codec (decode)
import Data.Codec.Argonaut (array, printJsonDecodeError)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Interpolate (i)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (guard)
import Data.Newtype (un)
import Data.Symbol (SProxy(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen (ClassName(..), liftAff)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks (useLifecycleEffect, useState)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Extra.Hooks (useEvent)
import Halogen.VDom.Driver (runUI)
import Network.RemoteData (RemoteData(..))
import Network.RemoteData as RD
import Partial.Unsafe (unsafePartial)
import Routing.Duplex (print)
import Select (SelectReturn(..), Visibility(..), selectInput, useSelect)
import Select as Select
import Shared.Codec (packageCodec)
import Shared.Config (baseUrl)
import Shared.Routes (PageRoute(..), pageRoutes)
import Shared.Types (Package(..))

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  runUI rootComponent unit body

rootComponent :: forall q i o. H.Component HH.HTML q i o Aff
rootComponent = Hooks.component \_ _ -> Hooks.do
  content /\ contentId <- useState $ RD.NotAsked
  useLifecycleEffect do
    reqResult <- liftAff $ AX.request $ AX.defaultRequest
          { url = baseUrl <> (print pageRoutes PackageList)
          , method = Left GET
          , responseFormat = AXRF.json
          }
    Hooks.put contentId case reqResult of
      Right response | response.status == StatusCode 200 ->
        case decode (array packageCodec) response.body of
          Left e -> Failure $ printJsonDecodeError e
          Right a -> Success a
      Right response -> do
        Failure (i "Status code: "(show response.status)": "(show response.statusText))
      Left e -> do
        Failure $ printError e
    pure Nothing

  Hooks.pure case content of
    RD.NotAsked ->
      HH.div_
        [ HH.text "Loaded page. Not yet loaded package array." ]
    RD.Failure msg ->
      HH.div_
        [ HH.text $ "Error: " <> msg ]
    RD.Loading ->
      HH.div_
        [ HH.text "loaded package array. Please wait." ]
    RD.Success packages ->
      HH.slot (SProxy ::_"view") unit viewComponent packages (const Nothing)

viewComponent :: forall q o. H.Component HH.HTML q (Array Package) o Aff
viewComponent = Hooks.component \_ array -> Hooks.do
  currentPackage /\ currentPackageId <- useState Nothing
  indexChangeEvents <- useEvent
  SelectReturn packageSelect <- useSelect $ selectInput
    { inputType = Select.Toggle
    , pushSelectedIdxChanged = indexChangeEvents.push
    }
  useLifecycleEffect do
    void $ indexChangeEvents.setCallback $ Just \_ idx -> do
      let
        selectedPackage = unsafePartial (unsafeIndex array idx)
      Hooks.put currentPackageId $ Just selectedPackage
      packageSelect.setVisibility Off

    pure Nothing

  Hooks.pure $
      HH.div_
        [ HH.div
          [ HP.class_ $ ClassName "Typeahead-container" ]
          [ HH.button
            (packageSelect.setToggleProps
              [ HP.class_ $ ClassName "Typeahead-searchbar" ])
            [ HH.text (maybe "Select..." (un Package) currentPackage) ]
          , whenElem (packageSelect.visibility == On) \_ ->
            HH.div
              (packageSelect.setContainerProps
                [ HP.class_ $ ClassName "Typeahead-item-container"
                ])
              (array # mapWithIndex \i next ->
                HH.div
                  (packageSelect.setItemProps i
                    [ HP.classes $ map ClassName
                      [ "Typeahead-item"
                      , "Typeahead-item--selected"
                          # (guard (packageSelect.highlightedIndex == Just i))
                      ]
                    ])
                  [ HH.text $ un Package next ]
              )
          ]
        , case currentPackage of
            Just p ->
              HH.img
                [ HP.src $ print pageRoutes $ PackageGraph p ]
            Nothing ->
              HH.p
                [ HP.class_ (ClassName "invalid-search-label") ]
                [ HH.text "Selected package is not a valid package..."]
        ]
