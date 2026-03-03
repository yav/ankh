module App.PlayerState where

import Data.Aeson qualified as JS

data PlayerState = PlayerState
  { playerFollowers :: Int
  , playerSoldiers  :: Int
  }
  deriving (Read, Show)

instance JS.ToJSON PlayerState where
  toJSON ps = JS.object
    [ "followers" JS..= playerFollowers ps
    , "soldiers"  JS..= playerSoldiers ps
    ]
