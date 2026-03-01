import type { Piece } from "./protocol.ts"
import { Component } from "./common-js/combinators.ts"

// Access global playerColors variable from dynamic.js
declare global {
  interface Window {
    playerColors: Record<string, string>
  }
}

// Piece rendering helpers
function getPieceClasses(piece: Piece): string[] {
  const classes = ["piece", `piece-${piece.kind}`]

  if (piece.player !== null) {
    // Player piece - use color from global playerColors variable
    const color = window.playerColors?.[piece.player] || "red"
    classes.push("piece-player", `piece-color-${color}`)
  } else {
    // Neutral structure
    classes.push("piece-neutral")
  }

  return classes
}

/**
 * Component for rendering a single piece
 */
export class PieceComponent implements Component<Piece> {
  private pieceElement: HTMLElement
  private tooltip: HTMLElement

  constructor(container: HTMLElement, x: number, y: number, index: number) {
    this.pieceElement = document.createElement("div")
    this.tooltip = document.createElement("div")
    this.tooltip.className = "piece-tooltip"
    this.tooltip.style.display = "none"

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

    container.appendChild(this.pieceElement)
    document.body.appendChild(this.tooltip)

    // Set initial position
    this.updatePosition(x, y, index, 1)
  }

  set(piece: Piece): boolean {
    // Update classes
    this.pieceElement.className = getPieceClasses(piece).join(" ")

    // Update tooltip text
    let tooltipText = ""
    if (piece.player !== null) {
      // Player piece
      tooltipText = `${piece.player}'s ${piece.kind}`
    } else {
      // Structure (neutral)
      tooltipText = `neutral ${piece.kind}`
    }

    this.tooltip.textContent = tooltipText
    return true
  }

  updatePosition(x: number, y: number, index: number, total: number): void {
    let offsetX = 0
    let offsetY = 0

    if (total === 1) {
      // Single piece: center
      offsetX = 0
      offsetY = 0
    } else if (total === 2) {
      // Two pieces: side by side
      offsetX = index === 0 ? -8 : 8
      offsetY = 0
    } else {
      // Three or more pieces: arrange in a circle
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
