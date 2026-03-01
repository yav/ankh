import {
  Grid,
  FLoc
} from "../../hex-grid/src/index.ts"
import type {
  Terrain,
  HexPos,
  EdgePos,
  Board,
  Hex,
  EdgeType
} from "./protocol.ts"
import {
  Component,
  MapComponent
} from "./common-js/combinators.ts"
import { HexComponent } from "./hexComponent.ts"
import { EdgeComponent } from "./edgeComponent.ts"

// Re-export for backwards compatibility
export type { Terrain, HexPos, EdgePos, Board }

/**
 * Calculates bounding box for a set of faces
 * Returns [minX, minY, maxX, maxY]
 * Returns [0, 0, 0, 0] for empty face list
 */
function calculateBoundingBox(
  grid: Grid,
  faces: FLoc[]
): [number, number, number, number] {
  if (faces.length === 0) {
    return [0, 0, 0, 0]
  }

  let minX = Infinity
  let minY = Infinity
  let maxX = -Infinity
  let maxY = -Infinity

  const [width, height] = grid.faceBoundingBox()

  for (const loc of faces) {
    const [x, y] = grid.faceLoc(loc)
    minX = Math.min(minX, x - width / 2)
    minY = Math.min(minY, y - height / 2)
    maxX = Math.max(maxX, x + width / 2)
    maxY = Math.max(maxY, y + height / 2)
  }

  return [minX, minY, maxX, maxY]
}

/**
 * Component for rendering the entire board
 */
export class BoardComponent implements Component<Board> {
  private container: HTMLElement
  private grid: Grid
  private hexes: MapComponent<Hex>
  private edges: MapComponent<EdgeType>
  private offsetX: number
  private offsetY: number
  private initialized: boolean

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
    this.initialized = false

    // Create map components for hexes and edges
    // The factory functions read this.offsetX/offsetY when called
    this.hexes = new MapComponent<Hex>((key) => {
      return new HexComponent(this.container, this.grid, key, this.offsetX, this.offsetY)
    })

    this.edges = new MapComponent<EdgeType>((key) => {
      return new EdgeComponent(this.container, this.grid, key, this.offsetX, this.offsetY)
    })
  }

  /**
   * Parses the board JSON into maps for rendering
   */
  private parseBoardData(board: Board) {
    const faces: FLoc[] = []
    const hexMap: { [key: string]: Hex } = {}

    for (const hexWithLoc of board.hexes) {
      const [x, y] = hexWithLoc.location
      const loc = new FLoc(x, y)
      faces.push(loc)

      const hex: Hex = {
        terrain: hexWithLoc.terrain,
        pieces: hexWithLoc.pieces
      }
      const key = `${x},${y}`
      hexMap[key] = hex
    }

    const edgeMap: { [key: string]: EdgeType } = {}
    for (const edge of board.edges) {
      const [x, y, dir] = edge.location
      const key = `${x},${y},${dir}`
      edgeMap[key] = edge.type
    }

    return {
      faces,
      hexMap,
      edgeMap
    }
  }

  set(board: Board): boolean {
    // Parse board data
    const { faces, hexMap, edgeMap } = this.parseBoardData(board)

    // Calculate offsets only once on first call (board shape won't change)
    let justInitialized = false
    if (!this.initialized) {
      const [minX, minY, maxX, maxY] = calculateBoundingBox(this.grid, faces)

      // Add padding
      const padding = 40
      this.offsetX = -minX + padding
      this.offsetY = -minY + padding

      // Set container dimensions
      this.container.style.width = `${maxX - minX + 2 * padding}px`
      this.container.style.height = `${maxY - minY + 2 * padding}px`

      this.initialized = true
      justInitialized = true
    }

    // Update components
    const hexesChanged = this.hexes.set(hexMap)
    const edgesChanged = this.edges.set(edgeMap)

    return justInitialized || hexesChanged || edgesChanged
  }

  destroy(): void {
    this.hexes.destroy()
    this.edges.destroy()
    this.container.innerHTML = ""
  }
}

