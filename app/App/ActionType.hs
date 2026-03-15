module App.ActionType where

import Data.Aeson(FromJSON(..), ToJSON(..), (.=))
import Data.Aeson qualified as JS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)

data Action = MoveFigures | SummonFigure | GainFollowers | GainPower
    deriving (Eq, Ord, Enum, Bounded, Read, Show)

data ActionAmount = ActionAmount {
    actionMax :: !Int,
    actionAvailable :: !Int
}
  deriving (Eq, Ord, Read, Show)

allActions :: [Action]
allActions = [minBound .. maxBound]

initActionAmount :: Int -> Action -> ActionAmount
initActionAmount playerCount act =
  ActionAmount { actionMax = ma, actionAvailable = ma }
  where
  ma  = tot + playerCount - 5
  tot =
    case act of
      GainPower -> 5
      _         -> 6

initActionSelector :: Int -> Map Action ActionAmount
initActionSelector playerCount =
  Map.fromList [ (act, initActionAmount playerCount act) | act <- allActions ]

-- | Human-readable label for presenting an action choice to the player.
actionLabel :: Action -> Text
actionLabel act =
  case act of
    MoveFigures -> "Move"
    SummonFigure -> "Summon"
    GainFollowers -> "Followers"
    GainPower -> "Power"

instance ToJSON Action where
  toJSON act =
    case act of
      MoveFigures       -> "move"
      SummonFigure      -> "summon"
      GainFollowers     -> "follower"
      GainPower         -> "power"

instance FromJSON Action where
  parseJSON = JS.withText "Action" $ \txt ->
    case txt of
      "move" -> pure MoveFigures
      "summon" -> pure SummonFigure
      "follower" -> pure GainFollowers
      "power" -> pure GainPower
      _ -> fail ("Unknown action: " ++ show txt)

instance ToJSON ActionAmount where
  toJSON x = JS.object [ "has" .= actionAvailable x, "max" .= actionMax x ]