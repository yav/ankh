module App.Action where

import Data.Aeson(ToJSON,(.=))
import Data.Aeson qualified as JS

data Action = MoveFigures | SummonFigure | GainFollowers | GainPower
    deriving (Eq,Ord,Bounded)

data ActionAmount = ActionAmount {
    actionMax :: !Int,
    actionAvailable :: !Int
}

initActionAmount :: Int -> Action -> ActionAmount
initActionAmount playerCount act =
  ActionAmount { actionMax = ma, actionAvailable = ma }
  where
  ma  = tot + playerCount - 5
  tot =
    case act of
      GainPower -> 5
      _         -> 6

instance ToJSON Action where
  toJSON act =
    case act of
      MoveFigures       -> "move"
      SummonFigure      -> "summon"
      GainFollowers     -> "follower"
      GainPower         -> "power"

instance ToJSON ActionAmount where
  toJSON x = JS.object [ "has" .= actionAvailable x, "max" .= actionMax x ]


