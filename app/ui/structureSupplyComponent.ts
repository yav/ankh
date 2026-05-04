import type { Component } from "./common-js/combinators.ts"
import type { StructureType, Input } from "./protocol.ts"
import type { Question } from "./common-js/connect.ts"
import { IconWithNumber } from "./iconWithNumber.ts"
import { registerQuestionCleanup, respondToQuestion } from "./questionActions"
import templeIconSrc from "./images/temple.svg"
import obeliskIconSrc from "./images/obelisk.svg"
import pyramidIconSrc from "./images/pyramid.svg"

const structureIcons: Record<StructureType, string> = {
  temple: templeIconSrc,
  obelisk: obeliskIconSrc,
  pyramid: pyramidIconSrc
}

class StructureCountComponent implements Component<number> {
  private inner: IconWithNumber
  private stype: StructureType
  private clickHandler: (() => void) | null

  constructor(parent: HTMLElement, stype: StructureType) {
    this.stype = stype
    this.inner = new IconWithNumber(parent, structureIcons[stype], "Available " + stype + "s")
    this.clickHandler = null
  }

  set(count: number): boolean {
    return this.inner.set(count)
  }

  ask(question: Question<Input>, teammateSelected: boolean): void {
    const dom = this.inner.getDom()
    dom.classList.add("structure-question-choice")
    if (teammateSelected) {
      dom.classList.add("teammate-selected")
    }

    this.clickHandler = () => respondToQuestion(question)
    dom.addEventListener("click", this.clickHandler)
    dom.title = question.chHelp

    registerQuestionCleanup(() => this.clearQuestionState())
  }

  private clearQuestionState(): void {
    const dom = this.inner.getDom()
    if (this.clickHandler) {
      dom.removeEventListener("click", this.clickHandler)
      this.clickHandler = null
    }
    dom.classList.remove("structure-question-choice")
    dom.classList.remove("teammate-selected")
    dom.title = "Available " + this.stype + "s"
  }

  destroy(): void {
    this.clearQuestionState()
    this.inner.destroy()
  }
}

export class StructureSupplyComponent implements Component<[StructureType, number][]> {
  private components: Map<StructureType, StructureCountComponent>

  constructor(parent: HTMLElement) {
    this.components = new Map()
    const types: StructureType[] = ["temple", "obelisk", "pyramid"]
    for (const stype of types) {
      this.components.set(stype, new StructureCountComponent(parent, stype))
    }
  }

  set(structures: [StructureType, number][]): boolean {
    let changed = false
    for (const [stype, count] of structures) {
      const comp = this.components.get(stype)
      if (comp && comp.set(count)) changed = true
    }
    return changed
  }

  handleChooseMonumentTypeQuestion(stype: StructureType, teammateSelected: boolean, question: Question<Input>): void {
    const comp = this.components.get(stype)
    if (comp) comp.ask(question, teammateSelected)
  }

  destroy(): void {
    for (const comp of this.components.values()) {
      comp.destroy()
    }
  }
}
