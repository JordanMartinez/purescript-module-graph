module Client.Utils where

import Prelude

import Data.Maybe (Maybe, maybe)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

class_ :: forall p i. String -> HH.IProp (class :: String | i) p
class_ = HP.class_ <<< HH.ClassName

classes_ :: forall p i. Array String -> HH.IProp (class :: String | i) p
classes_ = HP.classes <<< map HH.ClassName

whenElem :: forall p i. Boolean -> (Unit -> HH.HTML i p) -> HH.HTML i p
whenElem cond render = if cond then render unit else HH.text ""

mbElem :: forall a p i. Maybe a -> (a -> HH.HTML i p) -> HH.HTML i p
mbElem mb f = maybe (HH.text "") f mb
