import { Grid, FLoc, newHexShape } from "../../hex-grid/src/index.ts"
import type { Hex, Piece } from "./protocol.ts"
import { Component, List } from "./common-js/combinators.ts"
import { PieceComponent } from "./pieceComponent.ts"

/**
 * Component for rendering a hex with its pieces
 */
export class HexComponent implements Component<Hex> {
  private shape: HTMLElement
  private label: HTMLElement
  private pieces: List<Piece>
  private centerX: number
  private centerY: number
  private currentTerrain: string | undefined

  constructor(container: HTMLElement, grid: Grid, key: string, offsetX: number, offsetY: number) {
    // Parse key as "x,y"
    const [x, y] = key.split(",").map(Number)
    const loc = new FLoc(x, y)

    // Create hex shape
    this.shape = newHexShape(grid, loc)
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
    const [cx, cy] = grid.faceLoc(loc)
    this.centerX = cx + offsetX
    this.centerY = cy + offsetY

    // Create list component for pieces
    this.pieces = new List((index) => {
      const piece = new PieceComponent(container, this.centerX, this.centerY, index)
      return piece
    })

    container.appendChild(this.shape)
    container.appendChild(this.label)
    this.currentTerrain = undefined
  }

  set(hex: Hex): boolean {
    let changed = false

    // Update terrain class if changed
    if (this.currentTerrain !== hex.terrain) {
      this.shape.className = `hex-shape terrain-${hex.terrain}`
      this.currentTerrain = hex.terrain
      changed = true
    }

    // Update pieces
    const piecesChanged = this.pieces.set(hex.pieces)
    if (piecesChanged) {
      changed = true

      // Update piece positions only when pieces changed
      const pieceElements = this.pieces.getElements() as PieceComponent[]
      pieceElements.forEach((pieceComp, index) => {
        pieceComp.updatePosition(this.centerX, this.centerY, index, pieceElements.length)
      })
    }

    return changed
  }

  destroy(): void {
    this.pieces.destroy()
    this.shape.remove()
    this.label.remove()
  }
}
