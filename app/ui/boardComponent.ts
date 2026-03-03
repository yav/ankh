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
  private grid: Grid
  private hexes: MapComponent<Hex>
  private edges: MapComponent<EdgeType>
  private offsetX: number
  private offsetY: number
  private initialized: boolean
  private resizeObserver: ResizeObserver | null
  private baseWidth: number
  private baseHeight: number

  constructor() {

    // Create grid with fixed base size
    this.grid = new Grid()
    this.grid.setOrientation("edge_up")
    this.grid.setOuterDiameter(80) // Fixed base size
    this.grid.setSpacing(4)

    this.offsetX = 0
    this.offsetY = 0
    this.initialized = false
    this.resizeObserver = null
    this.baseWidth = 0
    this.baseHeight = 0

    // Create map components for hexes and edges
    this.hexes = new MapComponent<Hex>((key) => {
      return new HexComponent(this.grid, key, this.offsetX, this.offsetY)
    })

    this.edges = new MapComponent<EdgeType>((key) => {
      return new EdgeComponent(this.grid, key, this.offsetX, this.offsetY)
    })
  }

  /**
   * Update board scaling based on available space
   */
  private updateScale(): void {
    if (!this.initialized || this.baseWidth === 0 || this.baseHeight === 0) {
      return
    }

    const leftPane = document.querySelector(".left-pane") as HTMLElement
    const boardContainer = document.getElementById("board-container")!

    const containerWidth = leftPane.clientWidth || 800

    // Calculate scale based on width to use all horizontal space
    // Minimum scale of 0.8 (64px hexes)
    const scale = Math.max(0.8, containerWidth / this.baseWidth)

    boardContainer.style.transform = `scale(${scale})`
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

    // Calculate dimensions with fixed base hex size only once on first call
    let justInitialized = false
    if (!this.initialized) {
      // Calculate board extents with fixed 80px hex size
      const [minX, minY, maxX, maxY] = calculateBoundingBox(this.grid, faces)

      const padding = 10
      this.offsetX = -minX + padding
      this.offsetY = -minY + padding

      // Store base dimensions (unscaled)
      this.baseWidth = maxX - minX + 2 * padding
      this.baseHeight = maxY - minY + 2 * padding

      // Set wrapper to base size
      const boardContainer = document.getElementById("board-container")!
      boardContainer.style.width = `${this.baseWidth}px`
      boardContainer.style.height = `${this.baseHeight}px`

      this.initialized = true
      justInitialized = true

      // Set up resize observer if not already done
      if (!this.resizeObserver) {
        const leftPane = document.querySelector(".left-pane") as HTMLElement
        this.resizeObserver = new ResizeObserver(() => {
          this.updateScale()
        })
        this.resizeObserver.observe(leftPane)
      }

      // Initial scale calculation
      setTimeout(() => this.updateScale(), 0)
    }

    // Update components
    const hexesChanged = this.hexes.set(hexMap)
    const edgesChanged = this.edges.set(edgeMap)

    return justInitialized || hexesChanged || edgesChanged
  }

  destroy(): void {
    if (this.resizeObserver) {
      this.resizeObserver.disconnect()
      this.resizeObserver = null
    }
    this.hexes.destroy()
    this.edges.destroy()

    const boardContainer = document.getElementById("board-container")!
    boardContainer.innerHTML = ""
  }
}

