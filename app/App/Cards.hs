module App.Cards where

import Data.Aeson (ToJSON(..), FromJSON(..), withText)
import Data.Text (Text)

-- | All available Battle cards in the game
data Card
  = PlagueOfLocusts
  | BuildMonument
  | Chariots
  | CycleOfMaat
  | Drought
  | Flood
  | Miracle
  deriving (Eq, Ord, Show, Read)

instance ToJSON Card where
  toJSON card = toJSON $ case card of
    PlagueOfLocusts -> "plagueOfLocusts" :: Text
    BuildMonument   -> "buildMonument"
    Chariots        -> "chariots"
    CycleOfMaat     -> "cycleOfMaat"
    Drought         -> "drought"
    Flood           -> "flood"
    Miracle         -> "miracle"

instance FromJSON Card where
  parseJSON = withText "Card" $ \txt -> case txt of
    "plagueOfLocusts" -> pure PlagueOfLocusts
    "buildMonument"   -> pure BuildMonument
    "chariots"        -> pure Chariots
    "cycleOfMaat"     -> pure CycleOfMaat
    "drought"         -> pure Drought
    "flood"           -> pure Flood
    "miracle"         -> pure Miracle
    _                 -> fail $ "Unknown card: " ++ show txt

-- | Get the description of what a card does
cardDescription :: Card -> Text
cardDescription card = case card of
  PlagueOfLocusts ->
    "Each player with at least 1 figure in the region performs a blind bid \
    \of Followers to sacrifice. Each player secretly places any number of \
    \their Follower tokens in their closed fist, hiding their remaining pool \
    \with the other hand. Then, all players reveal their bid simultaneously. \
    \All Followers bid in this way are sacrificed and returned to the supply. \
    \All Warriors and Guardians in the region are killed, except for those \
    \belonging to the player who sacrificed the single most Followers. If 2 \
    \or more players are tied for the highest bid, then nobody's figures are \
    \spared. IMPORTANT: Note that if a player loses all their figures in the \
    \Battle region, they may still benefit from the effects of Worshipful or \
    \having played a Cycle of Ma'at or Miracle card, though they can no \
    \longer gain any Devotion when Monument Majority is determined, nor win \
    \the Battle (their strength ignores any bonuses and is considered 0)."

  BuildMonument ->
    "When this card is resolved, the player may sacrifice 3 of their Followers. \
    \If they do so, they build a Monument of their choice (Obelisk, Temple, or \
    \Pyramid, as long as there are still tokens left in the supply) in any \
    \empty non-Water space in the Battle region. The Monument is taken from \
    \the supply and placed on the board, attached to one of the player's Ankh \
    \tokens from their pool. If there's no empty space in the region, no \
    \Monument tokens in the supply, or no Ankh tokens left in the player's \
    \pool, they can't build a Monument."

  Chariots ->
    "The player gains +3 strength during the Battle Resolution (if they still \
    \have any figures left in the region). The card has no additional effect."

  CycleOfMaat ->
    "After Battle Resolution, the player reclaims all of their used Battle \
    \cards (including Cycle of Ma'at), placing them back in their hand. \
    \Playing this card is the only way for a player to retrieve their used \
    \Battle cards."

  Drought ->
    "If the player who revealed this card wins the Battle, their Devotion \
    \reward for winning is increased by 1 Devotion for each of their figures \
    \in a Desert (yellow) space in that region. Since this bonus is part of \
    \the \"winning a Battle\" Devotion reward, it does not trigger Bountiful \
    \an additional time."

  Flood ->
    "As soon as a player reveals this card, they gain 1 Follower for each of \
    \their figures in a Fertile (green) space in the region. Also, those \
    \figures cannot be killed during Battle Resolution (they can still die \
    \from the Plague of Locusts card)."

  Miracle ->
    "After Battle Resolution, the player gains 1 Devotion for each of their \
    \figures killed in the course of the Battle (including due to a Plague of \
    \Locusts card). If multiple players have played this card, they resolve \
    \it in reverse Devotion order (starting with the one with the least \
    \Devotion)."

cardStrength :: Card -> Int
cardStrength card =
  case card of
    PlagueOfLocusts -> 1
    Drought         -> 2
    Chariots        -> 3
    _               -> 0

-- | All cards in the game
allCards :: [Card]
allCards =
  [ PlagueOfLocusts
  , BuildMonument
  , Chariots
  , CycleOfMaat
  , Drought
  , Flood
  , Miracle
  ]
