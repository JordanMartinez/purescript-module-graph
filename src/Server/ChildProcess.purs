module Server.ChildProcess where

import Prelude
import Effect (Effect)

foreign import execSync :: String -> Effect Unit
