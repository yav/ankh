import { Component, Text } from "./common-js/combinators.ts"
import type { Card, Input } from "./protocol.ts"
import { cardDescription } from "./protocol.ts"
import type { Question } from "./common-js/connect.ts"
import { registerQuestionCleanup, respondToQuestion } from "./questionActions"

function cardName(card: Card): string {
  switch (card) {
    case "plagueOfLocusts": return "Plague of Locusts"
    case "buildMonument":   return "Build Monument"
    case "chariots":        return "Chariots"
    case "cycleOfMaat":     return "Cycle of Ma'at"
    case "drought":         return "Drought"
    case "flood":           return "Flood"
    case "miracle":         return "Miracle"
  }
}

function cardStrength(card: Card): number {
  switch (card) {
    case "plagueOfLocusts": return 1
    case "drought":         return 2
    default:                return 0
  }
}

export class CardComponent implements Component<Card> {
  private cardSpan: HTMLElement
  private strengthBadge: HTMLElement
  private nameSpan: HTMLElement
  private cardText: Text
  private currentCard: Card | null

  constructor(parent?: HTMLElement) {
    this.cardSpan = document.createElement("span")
    this.cardSpan.className = "card-badge"

    this.strengthBadge = document.createElement("span")
    this.strengthBadge.className = "card-strength-badge"

    this.nameSpan = document.createElement("span")
    this.nameSpan.className = "card-name"

    this.cardSpan.appendChild(this.strengthBadge)
    this.cardSpan.appendChild(this.nameSpan)
    this.cardText = new Text(this.nameSpan, false)
    this.currentCard = null
    if (parent) parent.appendChild(this.cardSpan)
  }

  set(card: Card): boolean {
    this.currentCard = card
    this.cardSpan.title = cardDescription(card)
    const strength = cardStrength(card)
    this.strengthBadge.textContent = `${strength}`
    return this.cardText.set(cardName(card))
  }

  ask(question: Question<Input>): void {
    if (this.currentCard === null) return

    this.cardSpan.classList.add("clickable")
    this.cardSpan.style.cursor = "pointer"

    if (question.chChoice.tag === "ChooseCard") {
      const [_card, teammateSelected] = question.chChoice.contents
      if (teammateSelected) {
        this.cardSpan.classList.add("teammate-selected")
        this.cardSpan.style.border = "3px solid gold"
        this.cardSpan.style.backgroundColor = "rgba(255, 215, 0, 0.2)"
      }
    }

    const clickHandler = () => { respondToQuestion(question) }

    this.cardSpan.addEventListener("click", clickHandler)
    registerQuestionCleanup(() => {
      this.cardSpan.removeEventListener("click", clickHandler)
      this.cardSpan.classList.remove("clickable")
      this.cardSpan.classList.remove("teammate-selected")
      this.cardSpan.style.cursor = ""
      this.cardSpan.style.border = ""
      this.cardSpan.style.backgroundColor = ""
    })
  }

  destroy(): void {
    this.cardText.destroy()
    this.cardSpan.remove()
  }

  getDom(): HTMLElement {
    return this.cardSpan
  }
}
