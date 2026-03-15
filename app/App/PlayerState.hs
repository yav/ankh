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

spendSoldier :: PlayerState -> PlayerState
spendSoldier playerState =
  playerState { playerSoldiers = playerSoldiers playerState - 1 }

addFollowers :: Int -> PlayerState -> PlayerState
addFollowers n playerState =
  playerState { playerFollowers = playerFollowers playerState + n }

instance ToJSON PlayerState where
  toJSON ps = JS.object
    [ "followers" .= playerFollowers ps
    , "soldiers"  .= playerSoldiers ps
    , "points"    .= playerPoints ps
    , "actions"   .= playerActions ps
    ]
