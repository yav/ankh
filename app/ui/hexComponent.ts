import { Grid, FLoc, newHexShape } from "../../hex-grid/src/index.ts"
import type { Hex, Piece, Input } from "./protocol.ts"
import type { Question } from "./common-js/connect.ts"
import { Component, List, Text } from "./common-js/combinators.ts"
import { PieceComponent } from "./pieceComponent.ts"
import { registerQuestionCleanup, respondToQuestion } from "./questionActions"

export type HexDisplayData = {
  hex: Hex,
  regionId: string | null
}

/**
 * Component for rendering a hex with its pieces
 */
export class HexComponent implements Component<HexDisplayData> {
  private shape: HTMLElement
  private regionLabel: HTMLElement
  private regionLabelText: Text
  private pieces: List<Piece>
  private centerX: number
  private centerY: number
  private currentTerrain: string | undefined
  private currentQuestion: Question<Input> | null
  private questionClickHandler: (() => void) | null
  private showRegion: boolean
  private currentRegionId: string | null
  private currentRegionClass: string

  constructor(grid: Grid, key: string, offsetX: number, offsetY: number, showRegion: boolean = false) {
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

    this.regionLabel = document.createElement("div")
    this.regionLabel.className = "hex-region-label hex-region-color-none"
    this.regionLabel.style.left = `${this.centerX}px`
    this.regionLabel.style.top = `${this.centerY}px`
    this.regionLabel.style.display = showRegion ? "" : "none"
    this.regionLabelText = new Text(this.regionLabel, false)

    // Create list component for pieces
    this.pieces = new List((index) => {
      const piece = new PieceComponent(this.centerX, this.centerY, index)
      return piece
    })

    container.appendChild(this.shape)
    container.appendChild(this.regionLabel)
    this.currentTerrain = undefined
    this.currentQuestion = null
    this.questionClickHandler = null
    this.showRegion = showRegion
    this.currentRegionId = null
    this.currentRegionClass = "hex-region-color-none"
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

  private updateRegionLabel(): void {
    const nextClass = this.currentRegionId === null
      ? "hex-region-color-none"
      : `hex-region-color-${Math.abs(Math.trunc(Number(this.currentRegionId))) % 10}`

    if (this.currentRegionClass !== nextClass) {
      this.regionLabel.classList.remove(this.currentRegionClass)
      this.regionLabel.classList.add(nextClass)
      this.currentRegionClass = nextClass
    }

    if (this.currentRegionId !== null) {
      this.regionLabelText.set(this.currentRegionId)
    } else {
      this.regionLabelText.set("")
    }
  }

  setRegionDisplay(show: boolean): boolean {
    const changed = this.showRegion !== show
    this.showRegion = show
    this.regionLabel.style.display = this.showRegion ? "" : "none"
    return changed
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

  set(data: HexDisplayData): boolean {
    let changed = false
    const { hex, regionId } = data

    if (this.currentRegionId !== regionId) {
      this.currentRegionId = regionId
      this.updateRegionLabel()
      changed = true
    }

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
    this.regionLabelText.destroy()
    this.regionLabel.remove()
    this.shape.remove()
  }
}
