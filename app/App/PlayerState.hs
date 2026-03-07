module App.PlayerState where

import Data.Aeson(ToJSON(..), (.=))
import Data.Aeson qualified as JS

data PlayerState = PlayerState {
  playerFollowers :: !Int,
  playerSoldiers  :: !Int,
  playerPoints    :: !Int,
  playerActions   :: !Int   -- ^ 1 or 2, depending on if merged
}
  deriving (Read, Show)

instance ToJSON PlayerState where
  toJSON ps = JS.object
    [ "followers" .= playerFollowers ps
    , "soldiers"  .= playerSoldiers ps
    , "points"    .= playerPoints ps
    , "actions"   .= playerActions ps
    ]
