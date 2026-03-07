// Protocol types for Ankh game WebSocket communication
// These types match the server-side Haskell definitions

// Player identification
export type PlayerId = string

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
  actions: number
}

// State view of the game
export type StateView = {
  board: Board,
  players: [PlayerId, PlayerState][]
}
