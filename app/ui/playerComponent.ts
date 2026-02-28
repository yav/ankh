import { Component, Text } from "./common-js/combinators.ts"
import type { PlayerId, PlayerState } from "./protocol.ts"

// Player data type for components
export type PlayerData = {
  id: PlayerId,
  state: PlayerState
}

/**
 * Component for rendering a single player
 */
export class PlayerComponent implements Component<PlayerData> {
  private container: HTMLElement
  private playerDiv: HTMLElement
  private idText: Text
  private stateText: Text

  constructor(container: HTMLElement) {
    this.container = container
    this.playerDiv = document.createElement("div")
    this.playerDiv.style.marginBottom = "8px"

    const idSpan = document.createElement("span")
    idSpan.style.fontWeight = "bold"
    idSpan.style.marginRight = "8px"
    this.idText = new Text(idSpan, false)

    const stateSpan = document.createElement("span")
    this.stateText = new Text(stateSpan, false)

    this.playerDiv.appendChild(idSpan)
    this.playerDiv.appendChild(stateSpan)
    this.container.appendChild(this.playerDiv)
  }

  set(player: PlayerData): boolean {
    this.idText.set(`${player.id}:`)
    this.stateText.set(JSON.stringify(player.state))
    return true
  }

  destroy(): void {
    this.idText.destroy()
    this.stateText.destroy()
    this.playerDiv.remove()
  }
}
