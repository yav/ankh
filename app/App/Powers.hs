module App.Powers where

import Data.Aeson (ToJSON(..), FromJSON(..), withText)
import Data.Text (Text)

-- | All available powers in the game
data Power
  -- Level 1 Powers
  = Commanding
  | Inspiring
  | Omnipresent
  | Revered

  -- Level 2 Powers
  | Resplendent
  | ObeliskAttuned
  | TempleAttuned
  | PyramidAttuned

  -- Level 3 Powers
  | Glorious
  | Magnanimous
  | Bountiful
  | Worshipful
  deriving (Eq, Ord, Show, Read)

instance ToJSON Power where
  toJSON power = toJSON $ case power of
    Commanding      -> "commanding" :: Text
    Inspiring       -> "inspiring"
    Omnipresent     -> "omnipresent"
    Revered         -> "revered"
    Resplendent     -> "resplendent"
    ObeliskAttuned  -> "obeliskAttuned"
    TempleAttuned   -> "templeAttuned"
    PyramidAttuned  -> "pyramidAttuned"
    Glorious        -> "glorious"
    Magnanimous     -> "magnanimous"
    Bountiful       -> "bountiful"
    Worshipful      -> "worshipful"

instance FromJSON Power where
  parseJSON = withText "Power" $ \txt -> case txt of
    "commanding"      -> pure Commanding
    "inspiring"       -> pure Inspiring
    "omnipresent"     -> pure Omnipresent
    "revered"         -> pure Revered
    "resplendent"     -> pure Resplendent
    "obeliskAttuned"  -> pure ObeliskAttuned
    "templeAttuned"   -> pure TempleAttuned
    "pyramidAttuned"  -> pure PyramidAttuned
    "glorious"        -> pure Glorious
    "magnanimous"     -> pure Magnanimous
    "bountiful"       -> pure Bountiful
    "worshipful"      -> pure Worshipful
    _                 -> fail $ "Unknown power: " ++ show txt

-- | Get the level of a power (1, 2, or 3)
powerLevel :: Power -> Int
powerLevel power = case power of
  -- Level 1
  Commanding    -> 1
  Inspiring     -> 1
  Omnipresent   -> 1
  Revered       -> 1

  -- Level 2
  Resplendent     -> 2
  ObeliskAttuned  -> 2
  TempleAttuned   -> 2
  PyramidAttuned  -> 2

  -- Level 3
  Glorious      -> 3
  Magnanimous   -> 3
  Bountiful     -> 3
  Worshipful    -> 3

-- | Get the description of what a power does
powerDescription :: Power -> Text
powerDescription power = case power of
  Commanding ->
    "Each time this player wins a Battle Resolution, they gain 3 Followers \
    \from the supply. This power doesn't apply for Dominance."

  Inspiring ->
    "Each time this player resolves a Build Monument card during a Battle, \
    \the cost to build that Monument is free. They don't need to sacrifice \
    \any Followers."

  Omnipresent ->
    "At the start of each Conflict Event (before resolving any Battles), \
    \this player gains 1 Follower for each Region where they have at least \
    \1 figure."

  Revered ->
    "Each time this player performs a Gain Followers action, they gain 1 \
    \additional Follower."

  Resplendent ->
    "If this player controls a total of 3 or more Monuments of the same \
    \type anywhere on the board, their God figure has base strength 3 \
    \instead of 1."

  ObeliskAttuned ->
    "At the start of a Battle where this player has at least 1 figure, \
    \they may move any number of their figures from anywhere on the board \
    \to empty spaces adjacent to Obelisks they control in the Battle region. \
    \If multiple players have this power, they use it in reverse Devotion \
    \order, each moving 1 figure at a time until they can't anymore, or wish \
    \to stop moving figures."

  TempleAttuned ->
    "Each of this player's Temples in a region grants them +2 strength \
    \there as long as there is at least 1 of their figures adjacent to it. \
    \Having more figures adjacent to a Temple doesn't increase this bonus, \
    \and figures whose strength might be neutralized still count."

  PyramidAttuned ->
    "When this player performs a Summon Figures action, they may summon \
    \an additional figure adjacent to each Pyramid they control. Players \
    \may \"chain summon\" by first summoning these additional figures and \
    \then summoning their regular figure from the action adjacent to one \
    \of them."

  Glorious ->
    "Each time this player wins a Battle and their strength is 3 or more \
    \higher than the next strongest enemy, they gain 3 Devotion instead \
    \of 1. Players with no figures left during Battle Resolution count as \
    \having strength 0 (ignoring any bonuses)."

  Magnanimous ->
    "Each time this player loses a Battle where they have at least 2 \
    \figures during the Battle Resolution step, they gain 2 Devotion. \
    \Resolve this after the winner gains Devotion. If multiple players \
    \have this power, resolve it in reverse Devotion order."

  Bountiful ->
    "While this player is in the red section of the Devotion track, \
    \each time they gain any amount of Devotion, they gain 1 extra Devotion. \
    \If a player is at the last red space of the track and gains Devotion, \
    \they still gain 1 extra Devotion. Winning a Battle or Dominating a \
    \region counts as a single instance of Devotion gain, even if there are \
    \multiple effects changing the amount gained. Likewise, all Monument \
    \majorities won in a region count as a single instance of Devotion gain. \
    \In each of these instances, the player gains just +1 Devotion from \
    \Bountiful. So, normally, a player can gain up to 2 extra Devotion from \
    \Bountiful in each region: 1 for dominating or winning the Battle, and \
    \1 for winning any Monument majorities."

  Worshipful ->
    "After each Battle this player participated in (played a Battle card \
    \in), they may sacrifice 2 Followers to gain 1 Devotion. If multiple \
    \players use this power, resolve them in reverse Devotion order \
    \(starting with the one with the least Devotion). Any Miracle cards \
    \played should be resolved first."

-- | Get all powers of a specific level
powersAtLevel :: Int -> [Power]
powersAtLevel 1 = [Commanding, Inspiring, Omnipresent, Revered]
powersAtLevel 2 = [Resplendent, ObeliskAttuned, TempleAttuned, PyramidAttuned]
powersAtLevel 3 = [Glorious, Magnanimous, Bountiful, Worshipful]
powersAtLevel _ = []

-- | All powers in the game
allPowers :: [Power]
allPowers = powersAtLevel 1 ++ powersAtLevel 2 ++ powersAtLevel 3
