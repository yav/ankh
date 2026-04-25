// Protocol types for Ankh game WebSocket communication
// These types match the server-side Haskell definitions

// Player identification
export type PlayerId = string

// App input values sent inside KOI ChoiceHelp.chChoice.
// This mirrors App.Input and its default Aeson encoding.
export type Input =
  | { tag: "ChooseHex", contents: HexPos }
  | { tag: "ChooseEdge", contents: EdgePos }
  | { tag: "ChoosePiece", contents: HexPos }
  | { tag: "ChooseAction", contents: Action }
  | { tag: "TextQuestion", contents: string }
  | { tag: "AskBid", contents: [number, number[]] }  // [bid amount, teammate bids]
  | { tag: "ChooseCard", contents: [Card, boolean] }  // [card, teammate selected this]

// Hexagonal coordinate system (FLoc in Haskell)
// Serialized as [x, y] array
export type HexPos = [number, number]

// Edge location (ELoc in Haskell)
// Serialized as [x, y, edge] array
export type EdgePos = [number, number, number]

// Terrain types
export type Terrain = "desert" | "grass" | "water"

// Player piece types (always represented as strings)
export type PlayerPieceType = "god" | "soldier" | "guardian"

// Structure types (always represented as strings)
export type StructureType = "temple" | "obelisk" | "pyramid"

// Game pieces
// Both player pieces and structures use the same shape: { player, kind }
// player is PlayerId for player pieces, null for structures
export type Piece = {
  player: PlayerId | null,
  kind: PlayerPieceType | StructureType
}

// Individual hex on the board
export type Hex = {
  terrain: Terrain,
  pieces: Piece[]
}

// Edge type for borders
export type EdgeType = "water" | "camels"

// Edge definition
export type Edge = {
  location: EdgePos,
  type: EdgeType
}

// Hexagon with location
export type HexagonWithLocation = {
  location: HexPos,  // Uses array representation [x, y]
  terrain: Terrain,
  pieces: Piece[]
}

// Game board with hexagonal grid
export type Board = {
  hexes: HexagonWithLocation[],
  regions: { [key: string]: HexPos[] }, // Key is region ID, values are arrays of [x, y]
  edges: Edge[]
}

// Player state
export type PlayerState = {
  followers: number,
  soldiers: number,
  points: number,
  actions: number,
  powers: Power[],
  hand: Card[],
  played: Card[]
}

export type ActionAmount = {
  has: number,
  max: number
}

export type Action = "move" | "summon" | "follower" | "power" | "testSplitRegion" | "testBid" | "testPlayCards" | "testGainPoints"

// Powers (matches App.Powers)
export type Power =
  // Level 1
  | "commanding"
  | "inspiring"
  | "omnipresent"
  | "revered"
  // Level 2
  | "resplendent"
  | "obeliskAttuned"
  | "templeAttuned"
  | "pyramidAttuned"
  // Level 3
  | "glorious"
  | "magnanimous"
  | "bountiful"
  | "worshipful"

// Battle Cards (matches App.Cards)
export type Card =
  | "plagueOfLocusts"
  | "buildMonument"
  | "chariots"
  | "cycleOfMaat"
  | "drought"
  | "flood"
  | "miracle"

export type SplitSelectionState = {
  edges: EdgePos[],
  invalid: boolean
}

export const emptySplitSelectionState: SplitSelectionState = {
  edges: [],
  invalid: false
}

// Log word type - inline elements
export type LogWord =
  | { tag: "text", contents: string }
  | { tag: "player", contents: PlayerId }
  | { tag: "card", contents: Card }
  | { tag: "followers", contents: number }
  | { tag: "points", contents: number }

// Log item type - block elements (recursive)
export type LogItem =
  | { tag: "entry", contents: LogWord[] }
  | { tag: "group", contents: LogItem[] }

// State view of the game
export type StateView = {
  board: Board,
  players: [PlayerId, PlayerState][],
  actions: [Action, ActionAmount][],
  splitSelection: SplitSelectionState,
  log: LogItem[]
}

// Get the description of a power
export function powerDescription(power: Power): string {
  switch (power) {
    case "commanding":
      return "Each time this player wins a Battle Resolution, they gain 3 Followers from the supply. This power doesn't apply for Dominance."
    case "inspiring":
      return "Each time this player resolves a Build Monument card during a Battle, the cost to build that Monument is free. They don't need to sacrifice any Followers."
    case "omnipresent":
      return "At the start of each Conflict Event (before resolving any Battles), this player gains 1 Follower for each Region where they have at least 1 figure."
    case "revered":
      return "Each time this player performs a Gain Followers action, they gain 1 additional Follower."
    case "resplendent":
      return "If this player controls a total of 3 or more Monuments of the same type anywhere on the board, their God figure has base strength 3 instead of 1."
    case "obeliskAttuned":
      return "At the start of a Battle where this player has at least 1 figure, they may move any number of their figures from anywhere on the board to empty spaces adjacent to Obelisks they control in the Battle region. If multiple players have this power, they use it in reverse Devotion order, each moving 1 figure at a time until they can't anymore, or wish to stop moving figures."
    case "templeAttuned":
      return "Each of this player's Temples in a region grants them +2 strength there as long as there is at least 1 of their figures adjacent to it. Having more figures adjacent to a Temple doesn't increase this bonus, and figures whose strength might be neutralized still count."
    case "pyramidAttuned":
      return "When this player performs a Summon Figures action, they may summon an additional figure adjacent to each Pyramid they control. Players may \"chain summon\" by first summoning these additional figures and then summoning their regular figure from the action adjacent to one of them."
    case "glorious":
      return "Each time this player wins a Battle and their strength is 3 or more higher than the next strongest enemy, they gain 3 Devotion instead of 1. Players with no figures left during Battle Resolution count as having strength 0 (ignoring any bonuses)."
    case "magnanimous":
      return "Each time this player loses a Battle where they have at least 2 figures during the Battle Resolution step, they gain 2 Devotion. Resolve this after the winner gains Devotion. If multiple players have this power, resolve it in reverse Devotion order."
    case "bountiful":
      return "While this player is in the red section of the Devotion track, each time they gain any amount of Devotion, they gain 1 extra Devotion. If a player is at the last red space of the track and gains Devotion, they still gain 1 extra Devotion. Winning a Battle or Dominating a region counts as a single instance of Devotion gain, even if there are multiple effects changing the amount gained. Likewise, all Monument majorities won in a region count as a single instance of Devotion gain. In each of these instances, the player gains just +1 Devotion from Bountiful. So, normally, a player can gain up to 2 extra Devotion from Bountiful in each region: 1 for dominating or winning the Battle, and 1 for winning any Monument majorities."
    case "worshipful":
      return "After each Battle this player participated in (played a Battle card in), they may sacrifice 2 Followers to gain 1 Devotion. If multiple players use this power, resolve them in reverse Devotion order (starting with the one with the least Devotion). Any Miracle cards played should be resolved first."
  }
}

// Get the description of a card
export function cardDescription(card: Card): string {
  switch (card) {
    case "plagueOfLocusts":
      return "Each player with at least 1 figure in the region performs a blind bid of Followers to sacrifice. Each player secretly places any number of their Follower tokens in their closed fist, hiding their remaining pool with the other hand. Then, all players reveal their bid simultaneously. All Followers bid in this way are sacrificed and returned to the supply. All Warriors and Guardians in the region are killed, except for those belonging to the player who sacrificed the single most Followers. If 2 or more players are tied for the highest bid, then nobody's figures are spared. IMPORTANT: Note that if a player loses all their figures in the Battle region, they may still benefit from the effects of Worshipful or having played a Cycle of Ma'at or Miracle card, though they can no longer gain any Devotion when Monument Majority is determined, nor win the Battle (their strength ignores any bonuses and is considered 0)."
    case "buildMonument":
      return "When this card is resolved, the player may sacrifice 3 of their Followers. If they do so, they build a Monument of their choice (Obelisk, Temple, or Pyramid, as long as there are still tokens left in the supply) in any empty non-Water space in the Battle region. The Monument is taken from the supply and placed on the board, attached to one of the player's Ankh tokens from their pool. If there's no empty space in the region, no Monument tokens in the supply, or no Ankh tokens left in the player's pool, they can't build a Monument."
    case "chariots":
      return "The player gains +3 strength during the Battle Resolution (if they still have any figures left in the region). The card has no additional effect."
    case "cycleOfMaat":
      return "After Battle Resolution, the player reclaims all of their used Battle cards (including Cycle of Ma'at), placing them back in their hand. Playing this card is the only way for a player to retrieve their used Battle cards."
    case "drought":
      return "If the player who revealed this card wins the Battle, their Devotion reward for winning is increased by 1 Devotion for each of their figures in a Desert (yellow) space in that region. Since this bonus is part of the \"winning a Battle\" Devotion reward, it does not trigger Bountiful an additional time."
    case "flood":
      return "As soon as a player reveals this card, they gain 1 Follower for each of their figures in a Fertile (green) space in the region. Also, those figures cannot be killed during Battle Resolution (they can still die from the Plague of Locusts card)."
    case "miracle":
      return "After Battle Resolution, the player gains 1 Devotion for each of their figures killed in the course of the Battle (including due to a Plague of Locusts card). If multiple players have played this card, they resolve it in reverse Devotion order (starting with the one with the least Devotion)."
  }
}
