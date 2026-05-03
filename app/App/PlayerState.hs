module App.PlayerState where

import Data.Aeson(ToJSON(..), (.=))
import Data.Aeson qualified as JS
import Data.Set (Set)

import App.Powers (Power)
import App.Cards (Card)

data PlayerState = PlayerState {
  playerFollowers  :: !Int,
  playerSoldiers   :: !Int,
  playerBuildLimit :: !Int,
  playerDevotion   :: !(Int,Int), -- ^ (devotion, tiebreaker)
  playerActions    :: !Int,   -- ^ 1 or 2, depending on if merged
  playerPowers     :: !(Set Power),
  playerHand       :: ![Card],
  playerPlayed     :: ![Card]
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

adjustBuildLimit :: Int -> PlayerState -> PlayerState
adjustBuildLimit n ps = ps { playerBuildLimit = playerBuildLimit ps + n }

playCard :: Card -> PlayerState -> PlayerState
playCard card playerState =
  playerState
    { playerHand = filter (/= card) (playerHand playerState)
    , playerPlayed = card : playerPlayed playerState
    }

instance ToJSON PlayerState where
  toJSON ps = JS.object
    [ "followers"  .= playerFollowers ps
    , "soldiers"   .= playerSoldiers ps
    , "buildLimit" .= playerBuildLimit ps
    , "devotion"   .= let (x,y) = playerDevotion ps in fromIntegral x + fromIntegral y / 10 :: Double
    , "actions"   .= playerActions ps
    , "powers"    .= playerPowers ps
    , "hand"      .= playerHand ps
    , "played"    .= playerPlayed ps
    ]
