import {
  Grid,
  FLoc,
  newHexShape,
  FLocMap,
  Region
} from "../../hex-grid/src/index.ts"

// Terrain types matching the Haskell definition
export type Terrain = "desert" | "grass" | "water"

// Hexagonal coordinate system
export type HexPos = {
  q: number,  // column
  r: number   // row
}

// Guardian type (currently empty in server)
type GuardianType = {}

// Player piece types
type PlayerPieceType =
  | "god"
  | "soldier"
  | { tag: "guardian", contents: GuardianType }

// Structure types
type StructureType = "temple" | "obelisk" | "pyramid"

// Game pieces
type Piece =
  | { tag: "PlayerPiece", player: string, pieceType: PlayerPieceType }
  | { tag: "Structure", structureType: StructureType }

// Individual hex on the board
type Hex = {
  terrain: Terrain,
  pieces: Piece[]
}

// Game board with hexagonal grid
export type Board = {
  hexes: { [key: string]: Hex },      // Key format: "q,r"
  regions: { [key: string]: HexPos[] } // Key is region ID
}

// Terrain color mapping
const TERRAIN_COLORS: Record<Terrain, string> = {
  desert: "#F4E4C1",  // Sandy beige
  grass: "#90C695",   // Grass green
  water: "#6BA3D9"    // Water blue
}

/**
 * A custom region representing the board's hexes
 * Uses FLocMap for efficient membership testing
 */
class BoardRegion extends Region {
  private faceSet: FLocMap<boolean>
  private faceList: FLoc[]

  constructor(positions: HexPos[]) {
    super()
    this.faceSet = new FLocMap<boolean>()
    this.faceList = []

    for (const pos of positions) {
      const loc = new FLoc(pos.q, pos.r)
      this.faceSet.setLoc(loc, true)
      this.faceList.push(loc)
    }
  }

  containsFace(face: FLoc): boolean {
    return this.faceSet.getLoc(face) !== null
  }

  *faces(): Generator<FLoc> {
    yield* this.faceList
  }
}

/**
 * Data for rendering the board
 * Uses FLocMap to efficiently store hex data
 */
interface BoardData {
  region: BoardRegion
  hexData: FLocMap<Hex>
}

/**
 * Parses the board JSON into hex-grid data structures
 */
function parseBoardData(board: Board): BoardData {
  const positions: HexPos[] = []
  const hexData = new FLocMap<Hex>()

  for (const [key, hex] of Object.entries(board.hexes)) {
    const [qStr, rStr] = key.split(",")
    const q = parseInt(qStr, 10)
    const r = parseInt(rStr, 10)
    const pos = { q, r }
    positions.push(pos)

    const loc = new FLoc(q, r)
    hexData.setLoc(loc, hex)
  }

  return {
    region: new BoardRegion(positions),
    hexData
  }
}

/**
 * Calculates bounding box for a region
 * Returns [minX, minY, maxX, maxY]
 */
function calculateBoundingBox(
  grid: Grid,
  region: Region
): [number, number, number, number] {
  let minX = Infinity
  let minY = Infinity
  let maxX = -Infinity
  let maxY = -Infinity

  const [width, height] = grid.faceBoundingBox()

  for (const loc of region.faces()) {
    const [x, y] = grid.faceLoc(loc)
    minX = Math.min(minX, x - width / 2)
    minY = Math.min(minY, y - height / 2)
    maxX = Math.max(maxX, x + width / 2)
    maxY = Math.max(maxY, y + height / 2)
  }

  return [minX, minY, maxX, maxY]
}

// Piece rendering helpers
function getPieceColor(piece: Piece): string {
  if (piece.tag === "PlayerPiece") {
    // Different colors for different players
    // For now, use a simple color scheme
    return "#FF6B6B" // Red for player pieces
  } else {
    // Structure colors
    switch (piece.structureType) {
      case "temple": return "#9B59B6"   // Purple
      case "obelisk": return "#E67E22"  // Orange
      case "pyramid": return "#F1C40F"  // Yellow
    }
  }
}

function renderPiece(
  container: HTMLElement,
  piece: Piece,
  x: number,
  y: number,
  index: number,
  total: number
): void {
  const pieceElement = document.createElement("div")
  pieceElement.style.position = "absolute"
  pieceElement.style.width = "12px"
  pieceElement.style.height = "12px"
  pieceElement.style.borderRadius = piece.tag === "Structure" ? "2px" : "50%"
  pieceElement.style.backgroundColor = getPieceColor(piece)
  pieceElement.style.border = "2px solid white"
  pieceElement.style.zIndex = "10"

  // Position multiple pieces in a grid within the hex
  const positions = [
    [0, 0],           // 1 piece: center
    [-8, 0], [8, 0],  // 2 pieces: left, right
  ]

  let offsetX = 0
  let offsetY = 0

  if (total === 1) {
    [offsetX, offsetY] = positions[0]
  } else if (total === 2) {
    [offsetX, offsetY] = positions[index + 1]
  } else {
    // For more pieces, arrange in a circle
    const angle = (index / total) * 2 * Math.PI
    offsetX = Math.cos(angle) * 10
    offsetY = Math.sin(angle) * 10
  }

  pieceElement.style.left = `${x + offsetX}px`
  pieceElement.style.top = `${y + offsetY}px`
  pieceElement.style.transform = "translate(-50%, -50%)"

  // Add tooltip with piece info
  const tooltip = document.createElement("div")
  tooltip.className = "piece-tooltip"
  tooltip.style.display = "none"
  tooltip.style.position = "absolute"
  tooltip.style.backgroundColor = "rgba(0, 0, 0, 0.8)"
  tooltip.style.color = "white"
  tooltip.style.padding = "4px 8px"
  tooltip.style.borderRadius = "4px"
  tooltip.style.fontSize = "12px"
  tooltip.style.zIndex = "100"
  tooltip.style.pointerEvents = "none"

  if (piece.tag === "PlayerPiece") {
    const pieceType = typeof piece.pieceType === "string"
      ? piece.pieceType
      : piece.pieceType.tag
    tooltip.textContent = `${piece.player}'s ${pieceType}`
  } else {
    tooltip.textContent = piece.structureType
  }

  pieceElement.addEventListener("mouseenter", () => {
    tooltip.style.display = "block"
  })

  pieceElement.addEventListener("mousemove", (e) => {
    tooltip.style.left = `${e.pageX + 10}px`
    tooltip.style.top = `${e.pageY + 10}px`
  })

  pieceElement.addEventListener("mouseleave", () => {
    tooltip.style.display = "none"
  })

  container.appendChild(pieceElement)
  container.appendChild(tooltip)
}

/**
 * Renders a game board using the hex-grid library
 */
export function renderBoard(container: HTMLElement, board: Board): void {
  // Clear the container
  container.innerHTML = ""
  container.style.position = "relative"

  // Parse board data using hex-grid structures
  const { region, hexData } = parseBoardData(board)

  // Check if board has hexes
  let hasHexes = false
  for (const _ of region.faces()) {
    hasHexes = true
    break
  }

  if (!hasHexes) {
    container.textContent = "No hexes on board"
    return
  }

  // Create and configure the grid
  const grid = new Grid()
  grid.setOrientation("edge_up")
  grid.setOuterDiameter(80)
  grid.setSpacing(4)

  // Calculate bounding box using hex-grid utilities
  const [minX, minY, maxX, maxY] = calculateBoundingBox(grid, region)

  // Add padding
  const padding = 40
  const offsetX = -minX + padding
  const offsetY = -minY + padding

  // Set container dimensions
  container.style.width = `${maxX - minX + 2 * padding}px`
  container.style.height = `${maxY - minY + 2 * padding}px`
  container.style.backgroundColor = "#2C3E50"

  // Render each hex using the region iterator
  for (const loc of region.faces()) {
    const hex = hexData.getLoc(loc)
    if (!hex) continue

    // Create hex shape using hex-grid utilities
    const shape = newHexShape(grid, loc)

    // Apply terrain color
    shape.style.backgroundColor = TERRAIN_COLORS[hex.terrain]
    shape.style.border = "1px solid #34495E"
    shape.style.zIndex = "0"

    // Apply offset
    const currentLeft = parseFloat(shape.style.left)
    const currentTop = parseFloat(shape.style.top)
    shape.style.left = `${currentLeft + offsetX}px`
    shape.style.top = `${currentTop + offsetY}px`

    // Add coordinate label
    const label = document.createElement("div")
    label.style.position = "absolute"
    label.style.left = shape.style.left
    label.style.top = shape.style.top
    label.style.transform = "translate(-50%, -50%)"
    label.style.fontSize = "10px"
    label.style.color = "#34495E"
    label.style.pointerEvents = "none"
    label.style.zIndex = "5"
    label.style.fontFamily = "monospace"
    label.textContent = `${loc.x},${loc.y}`

    container.appendChild(shape)
    container.appendChild(label)

    // Render pieces on this hex
    if (hex.pieces && hex.pieces.length > 0) {
      const [centerX, centerY] = grid.faceLoc(loc)
      hex.pieces.forEach((piece, index) => {
        renderPiece(
          container,
          piece,
          centerX + offsetX,
          centerY + offsetY,
          index,
          hex.pieces.length
        )
      })
    }
  }
}
