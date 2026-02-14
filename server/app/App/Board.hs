module App.Board where

import Data.Aeson qualified as JS

data Board = Board
  deriving (Read, Show)

instance JS.ToJSON Board where
  toJSON Board = JS.object []
