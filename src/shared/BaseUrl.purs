module Shared.Config where

import Data.Show (show)
import Data.Semigroup ((<>))

baseDomain :: String
baseDomain = "http://localhost"

port :: Int
port = 3000

baseUrl :: String
baseUrl = baseDomain <> ":" <> show port
