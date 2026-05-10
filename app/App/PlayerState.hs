module App.PlayerState where

import Data.Aeson(ToJSON(..), (.=))
import Data.Aeson qualified as JS
import Data.Set (Set)

import App.Piece (GuardianType, GuardianSize(..), guardianSize)
import App.Powers (Power)
import App.Cards (Card)

defaultSmallGuardianSlots :: Int
defaultSmallGuardianSlots = 2

defaultLargeGuardianSlots :: Int
defaultLargeGuardianSlots = 2

data PlayerState = PlayerState {
  playerFollowers  :: !Int,
  playerSoldiers   :: !Int,
  playerBuildLimit :: !Int,
  playerDevotion   :: !(Int,Int), -- ^ (devotion, tiebreaker)
  playerActions    :: !Int,   -- ^ 1 or 2, depending on if merged
  playerPowers     :: !(Set Power),
  playerHand       :: ![Card],
  playerPlayed     :: ![Card],
  playerGuardians  :: ![GuardianType],
  playerSmallGuardianSlots :: !Int,
  playerLargeGuardianSlots :: !Int
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

reclaimCards :: PlayerState -> PlayerState
reclaimCards ps =
  ps { playerHand = playerHand ps ++ playerPlayed ps
     , playerPlayed = []
     }


-- | Return an existing guardian to the supply
-- (does not affect limits)
addGuardian :: GuardianType -> PlayerState -> PlayerState
addGuardian gt ps =
  ps { playerGuardians = gt : playerGuardians ps }

-- | Add a new guardian to supply
gainGuardian :: GuardianType -> PlayerState -> PlayerState
gainGuardian gt ps0 =
  case guardianSize gt of
    Small -> ps { playerSmallGuardianSlots = playerSmallGuardianSlots ps0 - 1 }
    Large -> ps { playerLargeGuardianSlots = playerLargeGuardianSlots ps0 - 1 }
  where ps = addGuardian gt ps0

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
    , "guardians" .= playerGuardians ps
    , "smallGuardianSlots" .= playerSmallGuardianSlots ps
    , "largeGuardianSlots" .= playerLargeGuardianSlots ps
    ]
