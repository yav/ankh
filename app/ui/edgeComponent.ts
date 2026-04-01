import { Grid, FLoc, Dir, newEdgeShape } from "../../hex-grid/src/index.ts"
import type { EdgeType, Input } from "./protocol.ts"
import type { Question } from "./common-js/connect.ts"
import { Component } from "./common-js/combinators.ts"
import { registerQuestionCleanup, respondToQuestion } from "./questionActions"

/**
 * Component for rendering an edge type at a specific location.
 * The location is determined by the key passed to the constructor,
 * and only the edge type can change via set().
 */
export type EdgeDisplayData = {
  edgeType: EdgeType | null,
  splitSelected: boolean,
  splitInvalid: boolean
}

export class EdgeComponent implements Component<EdgeDisplayData> {
  private edgeShape: HTMLElement
  private currentType: EdgeType | null
  private currentQuestion: Question<Input> | null
  private questionClickHandler: (() => void) | null
  private currentSplitSelected: boolean
  private currentSplitInvalid: boolean

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
    this.currentType = null
    this.currentQuestion = null
    this.questionClickHandler = null
    this.currentSplitSelected = false
    this.currentSplitInvalid = false
  }

  private updateClasses(): void {
    const classes = ["edge-shape"]
    if (this.currentType !== null) {
      classes.push(`edge-${this.currentType}`)
    }
    if (this.currentQuestion !== null) {
      classes.push("edge-question-choice")
    }
    if (this.currentSplitSelected) {
      classes.push(this.currentSplitInvalid ? "edge-split-invalid" : "edge-split-selected")
    }
    this.edgeShape.className = classes.join(" ")
  }

  private clearQuestionState(): void {
    if (this.questionClickHandler !== null) {
      this.edgeShape.removeEventListener("click", this.questionClickHandler)
      this.questionClickHandler = null
    }
    this.currentQuestion = null
    this.edgeShape.removeAttribute("title")
    this.updateClasses()
  }

  handleChooseEdgeQuestion(question: Question<Input>): void {
    this.clearQuestionState()
    this.currentQuestion = question
    this.edgeShape.title = question.chHelp
    this.questionClickHandler = () => respondToQuestion(question)
    this.edgeShape.addEventListener("click", this.questionClickHandler)
    this.updateClasses()
    registerQuestionCleanup(() => this.clearQuestionState())
  }

  set(data: EdgeDisplayData): boolean {
    let changed = false

    if (this.currentType !== data.edgeType) {
      this.currentType = data.edgeType
      changed = true
    }

    if (this.currentSplitSelected !== data.splitSelected) {
      this.currentSplitSelected = data.splitSelected
      changed = true
    }

    if (this.currentSplitInvalid !== data.splitInvalid) {
      this.currentSplitInvalid = data.splitInvalid
      changed = true
    }

    if (changed) {
      this.updateClasses()
      return true
    }

    return false
  }

  destroy(): void {
    this.clearQuestionState()
    this.edgeShape.remove()
  }
}
