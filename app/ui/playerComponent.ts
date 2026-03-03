import { Component, Text } from "./common-js/combinators.ts"
import type { PlayerId, PlayerState } from "./protocol.ts"

/**
 * Component for rendering a single player
 */
export class PlayerComponent implements Component<[PlayerId, PlayerState]> {
  private playerDiv: HTMLElement
  private idText: Text
  private followersText: Text
  private soldiersText: Text

  constructor() {
    // Get container from DOM
    const container = document.getElementById("players-container")!

    this.playerDiv = document.createElement("div")
    this.playerDiv.style.marginBottom = "8px"

    const idSpan = document.createElement("span")
    idSpan.style.fontWeight = "bold"
    idSpan.style.marginRight = "8px"
    this.idText = new Text(idSpan, false)

    // Followers label
    const followersLabel = document.createElement("span")
    followersLabel.textContent = "Followers: "
    followersLabel.style.marginRight = "4px"

    // Followers value
    const followersSpan = document.createElement("span")
    followersSpan.style.marginRight = "12px"
    this.followersText = new Text(followersSpan, false)

    // Soldiers label
    const soldiersLabel = document.createElement("span")
    soldiersLabel.textContent = "Soldiers: "
    soldiersLabel.style.marginRight = "4px"

    // Soldiers value
    const soldiersSpan = document.createElement("span")
    this.soldiersText = new Text(soldiersSpan, false)

    this.playerDiv.appendChild(idSpan)
    this.playerDiv.appendChild(followersLabel)
    this.playerDiv.appendChild(followersSpan)
    this.playerDiv.appendChild(soldiersLabel)
    this.playerDiv.appendChild(soldiersSpan)
    container.appendChild(this.playerDiv)
  }

  set([id, state]: [PlayerId, PlayerState]): boolean {
    const idChanged = this.idText.set(`${id}:`)
    const followersChanged = this.followersText.set(`${state.followers}`)
    const soldiersChanged = this.soldiersText.set(`${state.soldiers}`)
    return idChanged || followersChanged || soldiersChanged
  }

  destroy(): void {
    this.idText.destroy()
    this.followersText.destroy()
    this.soldiersText.destroy()
    this.playerDiv.remove()
  }
}
