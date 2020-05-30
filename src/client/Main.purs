module Client.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.Hooks as Hooks
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  runUI component unit body

component :: forall q i o. H.Component HH.HTML q i o Aff
component = Hooks.component \_ _ -> do
  Hooks.pure $
    HH.div_
      [ HH.text "it works" ]
