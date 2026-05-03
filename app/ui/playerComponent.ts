import { Component, Text, List } from "./common-js/combinators.ts"
import type { PlayerId, PlayerState, Power, Card, Input } from "./protocol.ts"
import { powerDescription } from "./protocol.ts"
import type { Question } from "./common-js/connect.ts"
import { registerQuestionCleanup } from "./questionActions"
import { CardComponent } from "./cardComponent.ts"
import { IconWithNumber } from "./iconWithNumber.ts"
import { conn } from "./main.ts"
import cardsInHandIcon from "./images/cards-in-hand.svg"
import cardsPlayedIcon from "./images/cards-played.svg"
import followersIconSrc from "./images/followers.svg"
import soldiersIconSrc from "./images/soldier.svg"
import devotionIconSrc from "./images/devotion.svg"
import ankhIconSrc from "./images/ankh.svg"

// Access global playerColors variable from dynamic.js
declare global {
  interface Window {
    playerColors: Record<string, string>
  }
}

export class PlayerBadgeComponent implements Component<PlayerId> {
  private dom: HTMLElement
  private textComponent: Text
  private currentPlayer: PlayerId | null

  constructor(parent: HTMLElement) {
    this.dom = document.createElement("span")
    this.dom.className = "log-player-badge"
    this.textComponent = new Text(this.dom, false)
    this.currentPlayer = null
    parent.appendChild(this.dom)
  }

  set(playerId: PlayerId): boolean {
    if (this.currentPlayer === playerId) return false

    const color = window.playerColors?.[playerId] || "red"
    if (this.currentPlayer !== null) {
      const oldColor = window.playerColors?.[this.currentPlayer] || "red"
      this.dom.classList.remove("player-color-" + oldColor)
    }
    this.dom.classList.add("player-color-" + color)

    this.currentPlayer = playerId
    return this.textComponent.set(playerId)
  }

  destroy(): void {
    this.textComponent.destroy()
    this.dom.remove()
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
 * Component for rendering a single player
 */
export class PlayerComponent implements Component<[PlayerId, PlayerState]> {
  private playerDiv: HTMLElement
  private playerId: PlayerId
  private playerBadge: PlayerBadgeComponent
  private followersStat: IconWithNumber
  private soldiersStat: IconWithNumber
  private buildLimitStat: IconWithNumber
  private devotionStat: IconWithNumber
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

    this.playerBadge = new PlayerBadgeComponent(statsLine)

    this.followersStat = new IconWithNumber(statsLine, followersIconSrc, "Followers")
    this.soldiersStat = new IconWithNumber(statsLine, soldiersIconSrc, "Soldiers")
    this.buildLimitStat = new IconWithNumber(statsLine, ankhIconSrc, "Remaining buildings")
    this.devotionStat = new IconWithNumber(statsLine, devotionIconSrc, "Devotion")

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
    const idChanged = this.playerBadge.set(id)
    const followersChanged = this.followersStat.set(state.followers)
    const soldiersChanged = this.soldiersStat.set(state.soldiers)
    const buildLimitChanged = this.buildLimitStat.set(state.buildLimit)
    const devotionChanged = this.devotionStat.set(state.devotion)
    const powersChanged = this.powersList.set(state.powers)
    const handChanged = this.handList.set(state.hand)
    const playedChanged = this.playedList.set(state.played)

    return idChanged || followersChanged || soldiersChanged ||
           buildLimitChanged || devotionChanged || powersChanged ||
           handChanged || playedChanged
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
    this.playerBadge.destroy()
    this.followersStat.destroy()
    this.soldiersStat.destroy()
    this.buildLimitStat.destroy()
    this.devotionStat.destroy()
    this.powersList.destroy()
    this.handList.destroy()
    this.playedList.destroy()
    this.playerDiv.remove()
  }
}
