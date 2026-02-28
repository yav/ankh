import {
  Grid,
  FLoc,
  Dir,
  newHexShape,
  newEdgeShape,
  FLocMap,
  Region,
  SetRegion
} from "../../hex-grid/src/index.ts"
import type {
  Terrain,
  HexPos,
  EdgePos,
  Board,
  Hex,
  Piece,
  Edge
} from "./protocol.ts"
import {
  Component,
  List,
  MapComponent
} from "./common-js/combinators.ts"

// Re-export for backwards compatibility
export type { Terrain, HexPos, EdgePos, Board }

/**
 * Data for rendering the board
 * Uses FLocMap to efficiently store hex data
 */
interface BoardData {
  region: SetRegion
  hexData: FLocMap<Hex>
}

/**
 * Parses the board JSON into hex-grid data structures
 */
function parseBoardData(board: Board): BoardData {
  const faces: FLoc[] = []
  const hexData = new FLocMap<Hex>()

  for (const hexWithLoc of board.hexes) {
    const [x, y] = hexWithLoc.location
    const loc = new FLoc(x, y)
    faces.push(loc)

    const hex: Hex = {
      terrain: hexWithLoc.terrain,
      pieces: hexWithLoc.pieces
    }
    hexData.setLoc(loc, hex)
  }

  return {
    region: new SetRegion(faces),
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
function getPieceClasses(piece: Piece): string[] {
  const classes = ["piece"]

  if (piece.tag === "PlayerPiece") {
    classes.push("piece-player")
  } else {
    classes.push("piece-structure", `piece-structure-${piece.structureType}`)
  }

  return classes
}

/**
 * Component for rendering a single piece
 */
class PieceComponent implements Component<Piece> {
  private container: HTMLElement
  private x: number
  private y: number
  private index: number
  private pieceElement: HTMLElement
  private tooltip: HTMLElement
  private currentPiece: Piece | null

  constructor(container: HTMLElement, x: number, y: number, index: number) {
    this.container = container
    this.x = x
    this.y = y
    this.index = index
    this.currentPiece = null

    this.pieceElement = document.createElement("div")
    this.tooltip = document.createElement("div")
    this.tooltip.className = "piece-tooltip"

    this.pieceElement.addEventListener("mouseenter", () => {
      this.tooltip.style.display = "block"
    })

    this.pieceElement.addEventListener("mousemove", (e) => {
      this.tooltip.style.left = `${e.pageX + 10}px`
      this.tooltip.style.top = `${e.pageY + 10}px`
    })

    this.pieceElement.addEventListener("mouseleave", () => {
      this.tooltip.style.display = "none"
    })

    this.container.appendChild(this.pieceElement)
    this.container.appendChild(this.tooltip)
  }

  set(piece: Piece): boolean {
    // Update classes
    this.pieceElement.className = getPieceClasses(piece).join(" ")

    // Update tooltip text
    if (piece.tag === "PlayerPiece") {
      const pieceType = typeof piece.pieceType === "string"
        ? piece.pieceType
        : piece.pieceType.tag
      this.tooltip.textContent = `${piece.player}'s ${pieceType}`
    } else {
      this.tooltip.textContent = piece.structureType
    }

    this.currentPiece = piece
    return true
  }

  updatePosition(x: number, y: number, index: number, total: number): void {
    this.x = x
    this.y = y
    this.index = index

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

    this.pieceElement.style.left = `${x + offsetX}px`
    this.pieceElement.style.top = `${y + offsetY}px`
  }

  destroy(): void {
    this.pieceElement.remove()
    this.tooltip.remove()
  }
}

/**
 * Component for rendering a hex with its pieces
 */
class HexComponent implements Component<Hex> {
  private container: HTMLElement
  private grid: Grid
  private loc: FLoc
  private offsetX: number
  private offsetY: number
  private shape: HTMLElement
  private label: HTMLElement
  private pieces: List<Piece>
  private centerX: number
  private centerY: number

  constructor(container: HTMLElement, grid: Grid, key: string, offsetX: number, offsetY: number) {
    this.container = container
    this.grid = grid
    this.offsetX = offsetX
    this.offsetY = offsetY

    // Parse key as "x,y"
    const [x, y] = key.split(",").map(Number)
    this.loc = new FLoc(x, y)

    // Create hex shape
    this.shape = newHexShape(grid, this.loc)
    this.shape.classList.add("hex-shape")

    // Apply offset
    const currentLeft = parseFloat(this.shape.style.left)
    const currentTop = parseFloat(this.shape.style.top)
    this.shape.style.left = `${currentLeft + offsetX}px`
    this.shape.style.top = `${currentTop + offsetY}px`

    // Create coordinate label
    this.label = document.createElement("div")
    this.label.className = "coordinate-label"
    this.label.style.left = this.shape.style.left
    this.label.style.top = this.shape.style.top
    this.label.textContent = `${x},${y}`

    // Get center position for pieces
    const [cx, cy] = grid.faceLoc(this.loc)
    this.centerX = cx + offsetX
    this.centerY = cy + offsetY

    // Create list component for pieces
    this.pieces = new List<Piece>((index) => {
      const piece = new PieceComponent(this.container, this.centerX, this.centerY, index)
      return piece
    })

    this.container.appendChild(this.shape)
    this.container.appendChild(this.label)
  }

  set(hex: Hex): boolean {
    // Update terrain class
    this.shape.className = `hex-shape terrain-${hex.terrain}`

    // Update pieces
    const piecesChanged = this.pieces.set(hex.pieces)

    // Update piece positions
    const pieceElements = this.pieces.getElements()
    pieceElements.forEach((pieceComp, index) => {
      if (pieceComp instanceof PieceComponent) {
        pieceComp.updatePosition(this.centerX, this.centerY, index, pieceElements.length)
      }
    })

    return true
  }

  destroy(): void {
    this.pieces.destroy()
    this.shape.remove()
    this.label.remove()
  }
}

/**
 * Component for rendering an edge
 */
class EdgeComponent implements Component<Edge> {
  private container: HTMLElement
  private grid: Grid
  private offsetX: number
  private offsetY: number
  private edgeShape: HTMLElement | null

  constructor(container: HTMLElement, grid: Grid, _key: string, offsetX: number, offsetY: number) {
    this.container = container
    this.grid = grid
    this.offsetX = offsetX
    this.offsetY = offsetY
    this.edgeShape = null
  }

  set(edge: Edge): boolean {
    // Remove old edge if it exists
    if (this.edgeShape) {
      this.edgeShape.remove()
    }

    const [x, y, edgeNum] = edge.location
    const loc = new FLoc(x, y)
    const dir = new Dir(edgeNum)
    const eloc = loc.edge(dir)

    this.edgeShape = newEdgeShape(this.grid, eloc)
    this.edgeShape.classList.add(`edge-${edge.type}`)

    // Apply offset
    const currentLeft = parseFloat(this.edgeShape.style.left)
    const currentTop = parseFloat(this.edgeShape.style.top)
    this.edgeShape.style.left = `${currentLeft + this.offsetX}px`
    this.edgeShape.style.top = `${currentTop + this.offsetY}px`
    this.edgeShape.style.zIndex = "2"

    this.container.appendChild(this.edgeShape)
    return true
  }

  destroy(): void {
    if (this.edgeShape) {
      this.edgeShape.remove()
    }
  }
}

/**
 * Component for rendering the entire board
 */
export class BoardComponent implements Component<Board> {
  private container: HTMLElement
  private grid: Grid
  private hexes: MapComponent<Hex>
  private edges: MapComponent<Edge>
  private offsetX: number
  private offsetY: number

  constructor(container: HTMLElement) {
    this.container = container
    this.container.style.position = "relative"
    this.container.classList.add("board-container")

    // Create and configure the grid
    this.grid = new Grid()
    this.grid.setOrientation("edge_up")
    this.grid.setOuterDiameter(80)
    this.grid.setSpacing(4)

    this.offsetX = 0
    this.offsetY = 0

    // Create map components for hexes and edges
    this.hexes = new MapComponent<Hex>((key) => {
      return new HexComponent(this.container, this.grid, key, this.offsetX, this.offsetY)
    })

    this.edges = new MapComponent<Edge>((key) => {
      return new EdgeComponent(this.container, this.grid, key, this.offsetX, this.offsetY)
    })
  }

  set(board: Board): boolean {
    // Parse board data
    const { region, hexData } = parseBoardData(board)

    // Check if board has hexes
    let hasHexes = false
    for (const _ of region.faces()) {
      hasHexes = true
      break
    }

    if (!hasHexes) {
      this.container.textContent = "No hexes on board"
      return false
    }

    // Calculate bounding box
    const [minX, minY, maxX, maxY] = calculateBoundingBox(this.grid, region)

    // Add padding
    const padding = 40
    this.offsetX = -minX + padding
    this.offsetY = -minY + padding

    // Set container dimensions
    this.container.style.width = `${maxX - minX + 2 * padding}px`
    this.container.style.height = `${maxY - minY + 2 * padding}px`

    // Convert hexes to a map keyed by "x,y"
    const hexMap: { [key: string]: Hex } = {}
    for (const loc of region.faces()) {
      const hex = hexData.getLoc(loc)
      if (hex) {
        const key = `${loc.x},${loc.y}`
        hexMap[key] = hex
      }
    }

    // Convert edges to a map keyed by "x,y,dir"
    const edgeMap: { [key: string]: Edge } = {}
    for (const edge of board.edges) {
      const [x, y, dir] = edge.location
      const key = `${x},${y},${dir}`
      edgeMap[key] = edge
    }

    // Update components
    this.hexes.set(hexMap)
    this.edges.set(edgeMap)

    return true
  }

  destroy(): void {
    this.hexes.destroy()
    this.edges.destroy()
    this.container.innerHTML = ""
  }
}

