import { Component, Text, List } from "./common-js/combinators.ts"
import type { PlayerId, PlayerState, Power, Card, Input } from "./protocol.ts"
import { powerDescription, cardDescription } from "./protocol.ts"
import type { Question } from "./common-js/connect.ts"
import { registerQuestionCleanup, respondToQuestion } from "./questionActions"
import { conn } from "./main.ts"
import cardsInHandIcon from "./images/cards-in-hand.svg"
import cardsPlayedIcon from "./images/cards-played.svg"
import followersIconSrc from "./images/followers.svg"
import soldiersIconSrc from "./images/soldier.svg"
import pointsIconSrc from "./images/points.svg"

/**
 * Get the display name of a card
 */
function cardName(card: Card): string {
  switch (card) {
    case "plagueOfLocusts":
      return "Plague of Locusts"
    case "buildMonument":
      return "Build Monument"
    case "chariots":
      return "Chariots"
    case "cycleOfMaat":
      return "Cycle of Ma'at"
    case "drought":
      return "Drought"
    case "flood":
      return "Flood"
    case "miracle":
      return "Miracle"
  }
}

/**
 * Get the strength modifier of a card
 */
function cardStrength(card: Card): number {
  switch (card) {
    case "plagueOfLocusts":
      return 1
    case "drought":
      return 2
    default:
      return 0
  }
}

/**
 * Component for rendering a power name
 */
class PowerComponent implements Component<Power> {
  private powerSpan: HTMLElement
  private powerText: Text
  private currentPower: Power | null

  constructor() {
    this.powerSpan = document.createElement("span")
    this.powerSpan.className = "power-badge"
    this.powerText = new Text(this.powerSpan, false)
    this.currentPower = null
  }

  set(power: Power): boolean {
    this.currentPower = power
    this.powerSpan.title = powerDescription(power)
    return this.powerText.set(power)
  }

  destroy(): void {
    this.powerText.destroy()
    this.powerSpan.remove()
  }

  getDom(): HTMLElement {
    return this.powerSpan
  }
}

/**
 * Component for rendering a card name
 */
class CardComponent implements Component<Card> {
  private cardSpan: HTMLElement
  private strengthBadge: HTMLElement
  private nameSpan: HTMLElement
  private cardText: Text
  private currentCard: Card | null

  constructor() {
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

    const clickHandler = () => {
      respondToQuestion(question)
    }

    this.cardSpan.addEventListener("click", clickHandler)
    registerQuestionCleanup(() => {
      this.cardSpan.removeEventListener("click", clickHandler)
      this.cardSpan.classList.remove("clickable")
      this.cardSpan.style.cursor = ""
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

/**
 * Component for rendering a single player
 */
export class PlayerComponent implements Component<[PlayerId, PlayerState]> {
  private playerDiv: HTMLElement
  private playerId: PlayerId
  private idText: Text
  private followersIcon: HTMLImageElement
  private followersText: Text
  private soldiersIcon: HTMLImageElement
  private soldiersText: Text
  private pointsIcon: HTMLImageElement
  private pointsText: Text
  private powersContainer: HTMLElement
  private powersList: List<Power>
  private handIcon: HTMLImageElement
  private handContainer: HTMLElement
  private handList: List<Card>
  private playedIcon: HTMLImageElement
  private playedContainer: HTMLElement
  private playedList: List<Card>

  constructor() {
    // Get container from DOM
    const container = document.getElementById("players-container")!

    this.playerDiv = document.createElement("div")
    this.playerDiv.className = "player-card"

    // First line: ID and basic stats
    const statsLine = document.createElement("div")
    statsLine.className = "player-stats"

    const idSpan = document.createElement("span")
    idSpan.className = "player-id"
    this.idText = new Text(idSpan, false)

    // Followers with icon
    this.followersIcon = document.createElement("img")
    this.followersIcon.src = followersIconSrc
    this.followersIcon.title = "Followers"
    this.followersIcon.alt = "Followers"
    this.followersIcon.className = "stat-icon"
    const followersSpan = document.createElement("span")
    followersSpan.className = "stat-value"
    this.followersText = new Text(followersSpan, false)

    // Soldiers with icon
    this.soldiersIcon = document.createElement("img")
    this.soldiersIcon.src = soldiersIconSrc
    this.soldiersIcon.title = "Soldiers"
    this.soldiersIcon.alt = "Soldiers"
    this.soldiersIcon.className = "stat-icon"
    const soldiersSpan = document.createElement("span")
    soldiersSpan.className = "stat-value"
    this.soldiersText = new Text(soldiersSpan, false)

    // Points with icon
    this.pointsIcon = document.createElement("img")
    this.pointsIcon.src = pointsIconSrc
    this.pointsIcon.title = "Points"
    this.pointsIcon.alt = "Points"
    this.pointsIcon.className = "stat-icon"
    const pointsSpan = document.createElement("span")
    pointsSpan.className = "stat-value"
    this.pointsText = new Text(pointsSpan, false)

    // Hand cards icon
    this.handIcon = document.createElement("img")
    this.handIcon.src = cardsInHandIcon
    this.handIcon.title = "Cards in hand"
    this.handIcon.alt = "Cards in hand"
    this.handIcon.className = "card-toggle-icon"

    // Played cards icon
    this.playedIcon = document.createElement("img")
    this.playedIcon.src = cardsPlayedIcon
    this.playedIcon.title = "Played cards"
    this.playedIcon.alt = "Played cards"
    this.playedIcon.className = "card-toggle-icon"

    statsLine.appendChild(idSpan)
    statsLine.appendChild(this.followersIcon)
    statsLine.appendChild(followersSpan)
    statsLine.appendChild(this.soldiersIcon)
    statsLine.appendChild(soldiersSpan)
    statsLine.appendChild(this.pointsIcon)
    statsLine.appendChild(pointsSpan)
    statsLine.appendChild(this.handIcon)
    statsLine.appendChild(this.playedIcon)

    // Powers line
    const powersLine = document.createElement("div")
    powersLine.className = "player-powers"
    this.powersContainer = document.createElement("span")
    powersLine.appendChild(this.powersContainer)

    this.powersList = new List<Power>(() => {
      const comp = new PowerComponent()
      this.powersContainer.appendChild(comp.getDom())
      return comp
    })

    // Hand cards container
    this.handContainer = document.createElement("div")
    this.handContainer.className = "card-container card-container-hand"

    this.handList = new List<Card>(() => {
      const comp = new CardComponent()
      this.handContainer.appendChild(comp.getDom())
      return comp
    })

    this.handIcon.addEventListener("click", () => {
      this.handContainer.classList.toggle("visible")
      this.handIcon.classList.toggle("active")
    })

    // Played cards container
    this.playedContainer = document.createElement("div")
    this.playedContainer.className = "card-container card-container-played"

    this.playedList = new List<Card>(() => {
      const comp = new CardComponent()
      this.playedContainer.appendChild(comp.getDom())
      return comp
    })

    this.playedIcon.addEventListener("click", () => {
      this.playedContainer.classList.toggle("visible")
      this.playedIcon.classList.toggle("active")
    })

    // Assemble the player div
    this.playerDiv.appendChild(statsLine)
    this.playerDiv.appendChild(powersLine)
    this.playerDiv.appendChild(this.handContainer)
    this.playerDiv.appendChild(this.playedContainer)
    container.appendChild(this.playerDiv)
  }

  set([id, state]: [PlayerId, PlayerState]): boolean {
    this.playerId = id
    const idChanged = this.idText.set(`${id}:`)
    const followersChanged = this.followersText.set(`${state.followers}`)
    const soldiersChanged = this.soldiersText.set(`${state.soldiers}`)
    const pointsChanged = this.pointsText.set(`${state.points}`)
    const powersChanged = this.powersList.set(state.powers)
    const handChanged = this.handList.set(state.hand)
    const playedChanged = this.playedList.set(state.played)

    return idChanged || followersChanged || soldiersChanged ||
           pointsChanged || powersChanged || handChanged || playedChanged
  }

  handleChooseCardQuestion(card: Card, question: Question<Input>): void {
    // Only handle this question if this component represents the current player
    if (conn.playerId !== this.playerId) {
      return
    }

    // Remember if hand was already visible
    const wasVisible = this.handContainer.classList.contains("visible")

    // Make hand visible if it's not already
    if (!wasVisible) {
      this.handContainer.classList.add("visible")
      this.handIcon.classList.add("active")

      // Restore visibility state when question is done
      registerQuestionCleanup(() => {
        this.handContainer.classList.remove("visible")
        this.handIcon.classList.remove("active")
      })
    }

    // Find the card component that matches and make it clickable
    const cardComponents = this.handList.getElements()
    for (let i = 0; i < cardComponents.length; i++) {
      const cardComp = cardComponents[i] as CardComponent
      // Check if this component is displaying the target card
      if (cardComp['currentCard'] === card) {
        cardComp.ask(question)
        break
      }
    }
  }

  destroy(): void {
    this.idText.destroy()
    this.followersText.destroy()
    this.soldiersText.destroy()
    this.pointsText.destroy()
    this.powersList.destroy()
    this.handList.destroy()
    this.playedList.destroy()
    this.playerDiv.remove()
  }
}
