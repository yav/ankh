import type { Piece, Input, Merged } from "./protocol.ts"
import { playerColorHex } from "./protocol.ts"
import type { Question } from "./common-js/connect.ts"
import { Component } from "./common-js/combinators.ts"
import { registerQuestionCleanup, respondToQuestion } from "./questionActions"


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
    classes.push("piece-player")
  } else {
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
  private currentPiece: Piece | null
  private questionClickHandler: (() => void) | null
  private merged: Merged | null

  constructor(x: number, y: number, index: number) {
    // Get container from DOM
    const container = document.getElementById("board-container")
    if (!container) {
      throw new Error("Board container not found in DOM")
    }

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
    this.currentPiece = null
    this.questionClickHandler = null
    this.merged = null

    // Set initial position
    this.updatePosition(x, y, index, 1)
  }

  private updateClasses(): void {
    if (this.currentPiece === null) {
      this.pieceElement.className = "piece"
      this.pieceElement.style.background = ""
      return
    }

    const classes = getPieceClasses(this.currentPiece)
    if (this.questionClickHandler !== null) {
      classes.push("piece-question-choice")
    }
    this.pieceElement.className = classes.join(" ")
    this.applyMergedStyle()
  }

  private applyMergedStyle(): void {
    const piece = this.currentPiece
    if (piece === null || piece.player === null) {
      this.pieceElement.style.background = ""
      return
    }
    const color = window.playerColors?.[piece.player] || "red"
    const myColor = playerColorHex[color] || "#888"

    if (this.merged !== null &&
        (piece.player === this.merged.lead || piece.player === this.merged.follow)) {
      const teammate = piece.player === this.merged.lead ? this.merged.follow : this.merged.lead
      const teammateColor = playerColorHex[window.playerColors?.[teammate] || "red"] || "#888"
      this.pieceElement.style.background =
        `linear-gradient(to right, ${myColor}, ${teammateColor})`
    } else {
      this.pieceElement.style.background = myColor
    }
  }

  private clearQuestionState(): void {
    if (this.questionClickHandler !== null) {
      this.pieceElement.removeEventListener("click", this.questionClickHandler)
      this.questionClickHandler = null
    }
    this.pieceElement.removeAttribute("title")
    this.pieceElement.classList.remove("teammate-selected")
    this.updateClasses()
  }

  handleChoosePieceQuestion(question: Question<Input>, teammateSelected: boolean = false): void {
    this.clearQuestionState()
    this.pieceElement.title = question.chHelp
    if (teammateSelected) {
      this.pieceElement.classList.add("teammate-selected")
    }
    this.questionClickHandler = () => respondToQuestion(question)
    this.pieceElement.addEventListener("click", this.questionClickHandler)
    this.updateClasses()
    registerQuestionCleanup(() => this.clearQuestionState())
  }

  handleChooseHexQuestion(question: Question<Input>, teammateSelected: boolean = false): void {
    this.handleChoosePieceQuestion(question, teammateSelected)
  }

  setMerged(merged: Merged | null): void {
    this.merged = merged
    this.updateClasses()
  }

  set(piece: Piece): boolean {
    this.currentPiece = piece
    this.updateClasses()

    // Update tooltip text
    let tooltipText = ""
    if (piece.player !== null) {
      // Player piece
      tooltipText = `${piece.player}'s ${piece.kind}`
    } else {
      // Structure (neutral)
      tooltipText = `Neutral ${piece.kind}`
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

  setVisible(visible: boolean): void {
    this.pieceElement.style.display = visible ? "" : "none"
    if (!visible) {
      this.tooltip.style.display = "none"
    }
  }

  setPassThrough(passThrough: boolean): void {
    this.pieceElement.style.pointerEvents = passThrough ? "none" : "auto"
    if (passThrough) {
      this.tooltip.style.display = "none"
    }
  }

  destroy(): void {
    this.clearQuestionState()
    this.pieceElement.remove()
    this.tooltip.remove()
  }
}
