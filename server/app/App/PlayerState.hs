module App.PlayerState where

import Data.Aeson qualified as JS

data PlayerState = PlayerState
  deriving (Read, Show)

instance JS.ToJSON PlayerState where
  toJSON PlayerState = JS.object []
