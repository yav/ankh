import { Component, Text } from "./common-js/combinators.ts"
import type { Question } from "./common-js/connect.ts"
import type { Action, ActionAmount, Input } from "./protocol.ts"
import { registerQuestionCleanup, respondToQuestion } from "./questionActions"

const actionLabels: Record<Action, string> = {
  move: "Move",
  summon: "Summon",
  follower: "Followers",
  power: "Power"
}

export class ActionComponent implements Component<[Action, ActionAmount]> {
  private actionDiv: HTMLElement
  private nameText: Text
  private amountText: Text
  private questionClickHandler: (() => void) | null

  constructor() {
    const container = document.getElementById("actions-container")!

    this.actionDiv = document.createElement("div")
    this.actionDiv.className = "action-card"

    const nameDiv = document.createElement("div")
    nameDiv.className = "action-card-name"
    this.nameText = new Text(nameDiv, false)

    const amountDiv = document.createElement("div")
    amountDiv.className = "action-card-amount"
    this.amountText = new Text(amountDiv, false)

    this.actionDiv.appendChild(nameDiv)
    this.actionDiv.appendChild(amountDiv)
    container.appendChild(this.actionDiv)
    this.questionClickHandler = null
  }

  private updateClasses(): void {
    const classes = ["action-card"]
    if (this.questionClickHandler !== null) {
      classes.push("action-question-choice")
    }
    this.actionDiv.className = classes.join(" ")
  }

  private clearQuestionState(): void {
    if (this.questionClickHandler !== null) {
      this.actionDiv.removeEventListener("click", this.questionClickHandler)
      this.questionClickHandler = null
    }
    this.actionDiv.removeAttribute("title")
    this.updateClasses()
  }

  handleChooseActionQuestion(question: Question<Input>): void {
    this.actionDiv.title = question.chHelp
    registerQuestionCleanup(() => this.clearQuestionState())
    this.questionClickHandler = () => respondToQuestion(question)
    this.actionDiv.addEventListener("click", this.questionClickHandler)
    this.updateClasses()
  }

  set([action, amount]: [Action, ActionAmount]): boolean {
    const nameChanged = this.nameText.set(actionLabels[action])
    const amountChanged = this.amountText.set(`${amount.has}/${amount.max}`)
    return nameChanged || amountChanged
  }

  destroy(): void {
    this.clearQuestionState()
    this.nameText.destroy()
    this.amountText.destroy()
    this.actionDiv.remove()
  }
}