import type { Component } from "./common-js/combinators.ts"
import { Text } from "./common-js/combinators.ts"

export class IconWithNumber implements Component<number> {
  private dom: HTMLElement
  private textNode: Text

  constructor(parent: HTMLElement, iconSrc: string, tooltip: string) {
    this.dom = document.createElement("span")
    this.dom.className = "icon-with-number"
    this.dom.title = tooltip
    const numSpan = document.createElement("span")
    this.textNode = new Text(numSpan, false)
    const img = document.createElement("img")
    img.src = iconSrc
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

  getDom(): HTMLElement {
    return this.dom
  }
}
