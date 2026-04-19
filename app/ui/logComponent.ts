import type { Component } from "./common-js/combinators.ts"
import { Text, Tagged, List } from "./common-js/combinators.ts"
import type { LogItem, LogWord } from "./protocol.ts"
import { CardComponent } from "./cardComponent.ts"
import { PlayerBadgeComponent } from "./playerComponent.ts"
import followersIconSrc from "./images/followers.svg"


class LogTextComponent implements Component<string> {
  private dom: HTMLElement
  private textComponent: Text

  constructor(parent: HTMLElement) {
    this.dom = document.createElement("span")
    this.dom.className = "log-text"
    this.textComponent = new Text(this.dom, false)
    parent.appendChild(this.dom)
  }

  set(text: string): boolean {
    return this.textComponent.set(text)
  }

  destroy(): void {
    this.textComponent.destroy()
    this.dom.remove()
  }
}


class LogFollowersComponent implements Component<number> {
  private dom: HTMLElement
  private textNode: Text

  constructor(parent: HTMLElement) {
    this.dom = document.createElement("span")
    this.dom.className = "log-followers"
    const numSpan = document.createElement("span")
    this.textNode = new Text(numSpan, false)
    const img = document.createElement("img")
    img.src = followersIconSrc
    img.alt = "followers"
    img.className = "log-followers-icon"
    this.dom.appendChild(numSpan)
    this.dom.appendChild(img)
    parent.appendChild(this.dom)
  }

  set(n: number): boolean {
    return this.textNode.set(`${n}`)
  }

  destroy(): void {
    this.textNode.destroy()
    this.dom.remove()
  }
}

class LogCardComponent implements Component<string> {
  private card: CardComponent

  constructor(parent: HTMLElement) {
    this.card = new CardComponent(parent)
  }

  set(cardId: string): boolean {
    return this.card.set(cardId as any)
  }

  destroy(): void {
    this.card.destroy()
  }
}

class LogEntryComponent implements Component<LogWord[]> {
  private dom: HTMLElement
  private wordsList: List<LogWord>

  constructor(parentContainer: HTMLElement) {
    this.dom = document.createElement("div")
    this.dom.className = "log-entry"
    parentContainer.appendChild(this.dom)

    this.wordsList = new List<LogWord>(() => new Tagged({
      text: () => new LogTextComponent(this.dom),
      player: () => new PlayerBadgeComponent(this.dom),
      card: () => new LogCardComponent(this.dom),
      followers: () => new LogFollowersComponent(this.dom)
    }))
  }

  set(words: LogWord[]): boolean {
    return this.wordsList.set(words)
  }

  destroy(): void {
    this.wordsList.destroy()
    this.dom.remove()
  }
}

class LogGroupComponent implements Component<LogItem[]> {
  private dom: HTMLElement
  private itemsList: List<LogItem>

  constructor(parentContainer: HTMLElement) {
    this.dom = document.createElement("div")
    this.dom.className = "log-group"
    parentContainer.appendChild(this.dom)

    this.itemsList = new List<LogItem>(() => new LogItemComponent(this.dom))
  }

  set(items: LogItem[]): boolean {
    return this.itemsList.set(items)
  }

  destroy(): void {
    this.itemsList.destroy()
    this.dom.remove()
  }
}

export class LogItemComponent implements Component<LogItem> {
  private tagged: Tagged

  constructor(parentContainer?: HTMLElement) {
    const parent = parentContainer || document.getElementById("log-container")!
    this.tagged = new Tagged({
      entry: () => new LogEntryComponent(parent),
      group: () => new LogGroupComponent(parent)
    })
  }

  set(item: LogItem): boolean {
    return this.tagged.set(item as { tag: string, contents: unknown })
  }

  destroy(): void {
    this.tagged.destroy()
  }
}
