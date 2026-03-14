import { Grid, FLoc, newHexShape } from "../../hex-grid/src/index.ts"
import type { Hex, Piece, Input } from "./protocol.ts"
import type { Question } from "./common-js/connect.ts"
import { Component, List } from "./common-js/combinators.ts"
import { PieceComponent } from "./pieceComponent.ts"
import { registerQuestionCleanup, respondToQuestion } from "./questionActions"

/**
 * Component for rendering a hex with its pieces
 */
export class HexComponent implements Component<Hex> {
  private shape: HTMLElement
  private pieces: List<Piece>
  private centerX: number
  private centerY: number
  private currentTerrain: string | undefined
  private currentQuestion: Question<Input> | null
  private questionClickHandler: (() => void) | null

  constructor(grid: Grid, key: string, offsetX: number, offsetY: number) {
    // Get container from DOM
    const container = document.getElementById("board-container")
    if (!container) {
      throw new Error("Board container not found in DOM")
    }

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

    // Get center position for pieces
    const [cx, cy] = grid.faceLoc(loc)
    this.centerX = cx + offsetX
    this.centerY = cy + offsetY

    // Create list component for pieces
    this.pieces = new List((index) => {
      const piece = new PieceComponent(this.centerX, this.centerY, index)
      return piece
    })

    container.appendChild(this.shape)
    this.currentTerrain = undefined
    this.currentQuestion = null
    this.questionClickHandler = null
  }

  private updateClasses(): void {
    const classes = ["hex-shape"]
    if (this.currentTerrain !== undefined) {
      classes.push(`terrain-${this.currentTerrain}`)
    }
    if (this.currentQuestion !== null) {
      classes.push("hex-question-choice")
    }
    this.shape.className = classes.join(" ")
  }

  private clearQuestionState(): void {
    if (this.questionClickHandler !== null) {
      this.shape.removeEventListener("click", this.questionClickHandler)
      this.questionClickHandler = null
    }
    this.currentQuestion = null
    this.shape.removeAttribute("title")
    this.updateClasses()
  }

  handleChooseHexQuestion(question: Question<Input>): void {
    const pieceElements = this.pieces.getElements() as PieceComponent[]
    if (pieceElements.length > 0) {
      this.clearQuestionState()
      pieceElements.forEach((piece) => piece.handleChooseHexQuestion(question))
      return
    }

    this.clearQuestionState()
    this.currentQuestion = question
    this.shape.title = question.chHelp
    this.questionClickHandler = () => respondToQuestion(question)
    this.shape.addEventListener("click", this.questionClickHandler)
    this.updateClasses()
    registerQuestionCleanup(() => this.clearQuestionState())
  }

  set(hex: Hex): boolean {
    let changed = false

    // Update terrain class if changed
    if (this.currentTerrain !== hex.terrain) {
      this.currentTerrain = hex.terrain
      this.updateClasses()
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
    this.clearQuestionState()
    this.pieces.destroy()
    this.shape.remove()
  }
}
