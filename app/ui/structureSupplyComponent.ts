import type { Component } from "./common-js/combinators.ts"
import { List } from "./common-js/combinators.ts"
import type { StructureType } from "./protocol.ts"
import { IconWithNumber } from "./iconWithNumber.ts"
import templeIconSrc from "./images/temple.svg"
import obeliskIconSrc from "./images/obelisk.svg"
import pyramidIconSrc from "./images/pyramid.svg"

const structureIcons: Record<StructureType, string> = {
  temple: templeIconSrc,
  obelisk: obeliskIconSrc,
  pyramid: pyramidIconSrc
}

class StructureCountComponent implements Component<[StructureType, number]> {
  private inner: IconWithNumber | null
  private parent: HTMLElement
  private currentType: StructureType | null

  constructor(parent: HTMLElement) {
    this.parent = parent
    this.inner = null
    this.currentType = null
  }

  set([stype, count]: [StructureType, number]): boolean {
    let changed = false
    if (this.currentType !== stype) {
      if (this.inner) this.inner.destroy()
      this.inner = new IconWithNumber(this.parent, structureIcons[stype], "Available " + stype + "s")
      this.currentType = stype
      changed = true
    }
    if (this.inner!.set(count)) changed = true
    return changed
  }

  destroy(): void {
    if (this.inner) this.inner.destroy()
  }
}

export class StructureSupplyComponent implements Component<[StructureType, number][]> {
  private list: List<[StructureType, number]>

  constructor(parent: HTMLElement) {
    this.list = new List(() => new StructureCountComponent(parent))
  }

  set(structures: [StructureType, number][]): boolean {
    return this.list.set(structures)
  }

  destroy(): void {
    this.list.destroy()
  }
}
