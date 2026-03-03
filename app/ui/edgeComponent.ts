import { Grid, FLoc, Dir, newEdgeShape } from "../../hex-grid/src/index.ts"
import type { EdgeType } from "./protocol.ts"
import { Component } from "./common-js/combinators.ts"

/**
 * Component for rendering an edge type at a specific location.
 * The location is determined by the key passed to the constructor,
 * and only the edge type can change via set().
 */
export class EdgeComponent implements Component<EdgeType> {
  private edgeShape: HTMLElement
  private currentType: EdgeType | undefined

  constructor(grid: Grid, key: string, offsetX: number, offsetY: number) {
    // Get container from DOM
    const container = document.getElementById("board-container")
    if (!container) {
      throw new Error("Board container not found in DOM")
    }

    // Parse key as "x,y,dir"
    const [x, y, edgeNum] = key.split(",").map(Number)
    const loc = new FLoc(x, y)
    const dir = new Dir(edgeNum)
    const eloc = loc.edge(dir)

    // Create edge shape once (location is fixed)
    this.edgeShape = newEdgeShape(grid, eloc)
    this.edgeShape.style.zIndex = "2"

    // Apply offset
    const currentLeft = parseFloat(this.edgeShape.style.left)
    const currentTop = parseFloat(this.edgeShape.style.top)
    this.edgeShape.style.left = `${currentLeft + offsetX}px`
    this.edgeShape.style.top = `${currentTop + offsetY}px`

    container.appendChild(this.edgeShape)
    this.currentType = undefined
  }

  set(edgeType: EdgeType): boolean {
    // Only update if type changed
    if (this.currentType !== edgeType) {
      // Remove old class if it exists
      if (this.currentType !== undefined) {
        this.edgeShape.classList.remove(`edge-${this.currentType}`)
      }

      // Add new class
      this.edgeShape.classList.add(`edge-${edgeType}`)
      this.currentType = edgeType
      return true
    }

    return false
  }

  destroy(): void {
    this.edgeShape.remove()
  }
}
