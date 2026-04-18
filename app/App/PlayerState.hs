module App.PlayerState where

import Data.Aeson(ToJSON(..), (.=))
import Data.Aeson qualified as JS
import Data.Set (Set)

import App.Powers (Power)
import App.Cards (Card)

data PlayerState = PlayerState {
  playerFollowers :: !Int,
  playerSoldiers  :: !Int,
  playerPoints    :: !Int,
  playerActions   :: !Int,   -- ^ 1 or 2, depending on if merged
  playerPowers    :: !(Set Power),
  playerHand      :: ![Card],
  playerPlayed    :: ![Card],
  playerTeam      :: !Int
}
  deriving (Read, Show)

spendSoldier :: PlayerState -> PlayerState
spendSoldier playerState =
  playerState { playerSoldiers = playerSoldiers playerState - 1 }

addFollowers :: Int -> PlayerState -> PlayerState
addFollowers n playerState =
  playerState { playerFollowers = playerFollowers playerState + n }

spendFollowers :: Int -> PlayerState -> PlayerState
spendFollowers n playerState =
  playerState { playerFollowers = playerFollowers playerState - n }

playCard :: Card -> PlayerState -> PlayerState
playCard card playerState =
  playerState
    { playerHand = filter (/= card) (playerHand playerState)
    , playerPlayed = card : playerPlayed playerState
    }

instance ToJSON PlayerState where
  toJSON ps = JS.object
    [ "followers" .= playerFollowers ps
    , "soldiers"  .= playerSoldiers ps
    , "points"    .= playerPoints ps
    , "actions"   .= playerActions ps
    , "powers"    .= playerPowers ps
    , "hand"      .= playerHand ps
    , "played"    .= playerPlayed ps
    , "team"      .= playerTeam ps
    ]
