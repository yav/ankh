// Common interfaces for UI combinators

/**
 * Interface for objects that can be cleaned up and released.
 */
export interface Destroyable {
  destroy(): void
}

/**
 * A component that can be updated with new values.
 * The `set` method returns true if the value changed, false otherwise.
 */
export interface Component<T> extends Destroyable {
  set(value: T): boolean
}

/**
 * Interface for containers that support iteration over their elements.
 */
export interface Mappable<T> {
  map(f: (element: T) => void): void
}

/**
 * Interface for containers that provide access to their elements as an array.
 */
export interface Container<T> {
  getElements(): T[]
}

/**
 * A dynamic list of components that automatically creates, updates, and destroys
 * elements as the array grows and shrinks.
 *
 * When the array gets longer, new elements are created using the factory function.
 * When it gets shorter, excess elements are destroyed.
 */
export class List<T> implements Component<T[]>, Mappable<Component<T>>, Container<Component<T>> {
  private mk: (index: number) => Component<T>
  private els: Component<T>[]

  constructor(mk: (index: number) => Component<T>) {
    this.mk = mk
    this.els = []
  }

  getElements(): Component<T>[] {
    return this.els
  }

  map(f: (element: Component<T>) => void): void {
    for (const el of this.els) f(el)
  }

  set(xs: T[]): boolean {
    const oldLen = this.els.length
    const newLen = xs.length
    const smaller = Math.min(oldLen, newLen)

    let changed = false
    for (let i = 0; i < smaller; ++i) {
      if (this.els[i].set(xs[i])) changed = true
    }
    for (let i = smaller; i < newLen; ++i) {
      const v = this.mk(i)
      v.set(xs[i])
      this.els.push(v)
      changed = true
    }
    for (let i = smaller; i < oldLen; ++i) {
      this.els.pop()!.destroy()
      changed = true
    }
    return changed
  }

  destroy(): void {
    for (const i of this.els) i.destroy()
    this.els = []
    this.mk = () => { throw new Error("List has been destroyed") }
  }
}

/**
 * An optional component that may or may not exist.
 *
 * When set to null, the component is destroyed.
 * When set to a non-null value, a component is created (if needed) and updated.
 */
export class Optional<T> implements Component<T | null>, Mappable<Component<T>>, Container<Component<T>> {
  private mk: () => Component<T>
  private el: Component<T> | null

  constructor(mk: () => Component<T>) {
    this.mk = mk
    this.el = null
  }

  map(f: (element: Component<T>) => void): void {
    if (this.el !== null) f(this.el)
  }

  getElements(): Component<T>[] {
    return this.el === null ? [] : [this.el]
  }

  destroy(): void {
    if (this.el !== null) this.el.destroy()
    this.el = null
    this.mk = () => { throw new Error("Optional has been destroyed") }
  }

  set(x: T | null): boolean {
    if (this.el === null) {
      if (x !== null) {
        this.el = this.mk()
        this.el.set(x)
        return true
      } else {
        return false
      }
    } else {
      if (x === null) {
        this.el.destroy()
        this.el = null
        return true
      } else {
        return this.el.set(x)
      }
    }
  }
}

/**
 * A constant component that wraps a fixed value.
 *
 * The `set` method always returns false (the value never changes).
 * The wrapped value is destroyed when the Const is destroyed.
 */
export class Const<T extends Destroyable> implements Component<void>, Mappable<T> {
  private thing: T

  constructor(thing: T) {
    this.thing = thing
  }

  destroy(): void {
    this.thing.destroy()
  }

  set(_value: void): boolean {
    return false
  }

  map(f: (element: T) => void): void {
    f(this.thing)
  }
}

/**
 * A record (object) of components with fixed fields.
 *
 * The set of fields is established at construction time and does not change.
 * When setting a new value, each field is updated with its corresponding value.
 */
export class Record implements Component<{ [key: string]: unknown }>, Mappable<Component<unknown>> {
  private obj: { [key: string]: Component<unknown> }
  private ord: string[]

  constructor(obj: { [key: string]: Component<unknown> }, ord?: string[]) {
    this.obj = obj
    this.ord = ord === undefined ? Object.keys(obj) : ord
  }

  set(obj: { [key: string]: unknown }): boolean {
    let changed = false
    for (const i of this.ord) {
      const el = obj[i]
      if (el === undefined) {
        console.log("Field " + i + " is undefined")
      }
      if (this.obj[i].set(el)) changed = true
    }
    return changed
  }

  destroy(): void {
    for (const i of this.ord) {
      this.obj[i].destroy()
    }
  }

  map(f: (element: Component<unknown>) => void): void {
    for (const i of this.ord) f(this.obj[i])
  }

  getElement(i: string): Component<unknown> {
    return this.obj[i]
  }
}

/**
 * A fixed-length tuple of components.
 *
 * The number of elements is established at construction time and does not change.
 * When setting a new value, each element is updated with its corresponding value.
 */
export class Tuple implements Component<unknown[]>, Mappable<Component<unknown>> {
  private arr: Component<unknown>[]

  constructor(arr: Component<unknown>[]) {
    this.arr = arr
  }

  set(arr: unknown[]): boolean {
    let changed = false
    for (let i = 0; i < this.arr.length; ++i) {
      if (this.arr[i].set(arr[i])) changed = true
    }
    return changed
  }

  destroy(): void {
    for (const o of this.arr) o.destroy()
  }

  map(f: (element: Component<unknown>) => void): void {
    for (const o of this.arr) f(o)
  }

  getElement(i: number): Component<unknown> {
    return this.arr[i]
  }
}

/**
 * A value with a tag and contents, used for tagged unions.
 */
type TaggedValue = { tag: string, contents: unknown }

/**
 * A tagged union component that can hold one of several different component types.
 *
 * The active variant is determined by the tag.
 * When the tag changes, the old component is destroyed and a new one is created.
 * When the tag stays the same, the existing component is updated.
 */
export class Tagged implements Component<TaggedValue>, Mappable<Component<unknown>> {
  private mk: { [key: string]: () => Component<unknown> }
  private tag: string | null
  private val: Component<unknown> | null

  constructor(mk: { [key: string]: () => Component<unknown> }) {
    this.mk = mk
    this.tag = null
    this.val = null
  }

  destroy(): void {
    if (this.val !== null) this.val.destroy()
    this.mk = {}
    this.tag = null
    this.val = null
  }

  set(v: TaggedValue): boolean {
    const tag = v.tag
    if (this.tag === tag && this.val !== null) {
      return this.val.set(v.contents)
    } else {
      if (this.val !== null) this.val.destroy()
      const el = this.mk[tag]()
      el.set(v.contents)
      this.tag = tag
      this.val = el
      return true
    }
  }

  map(f: (element: Component<unknown>) => void): void {
    if (this.val !== null) f(this.val)
  }

  getElement(tag: string): Component<unknown> | null {
    if (this.tag === tag) return this.val
    return null
  }
}

/**
 * A text component that updates the text content of a DOM element.
 *
 * If `own` is true, the element is removed from the DOM when destroyed.
 * If `own` is false, only the text content is cleared.
 */
export class Text implements Component<string> {
  private own: boolean
  private dom: HTMLElement
  private val: string | null

  constructor(dom: HTMLElement, own: boolean) {
    this.own = own
    this.dom = dom
    this.val = null
  }

  destroy(): void {
    if (this.own) {
      this.dom.remove()
    } else {
      this.dom.textContent = ""
    }
  }

  set(x: string): boolean {
    if (this.val === x) return false
    this.val = x
    this.dom.textContent = this.val
    return true
  }

  get(): string | null {
    return this.val
  }
}

/**
 * A component that manages a single CSS class on a DOM element.
 *
 * When set to a new class name, the old class is removed and the new one is added.
 * When set to null, the current class is removed.
 */
export class DomClass implements Component<string | null> {
  private tag: string | null
  private dom: HTMLElement

  constructor(dom: HTMLElement) {
    this.tag = null
    this.dom = dom
  }

  set(newTag: string | null): boolean {
    if (this.tag === newTag) return false
    if (this.tag !== null) {
      this.dom.classList.remove(this.tag)
    }
    this.tag = newTag
    if (this.tag !== null) {
      this.dom.classList.add(this.tag)
    }
    return true
  }

  destroy(): void {
    if (this.tag !== null) {
      this.dom.classList.remove(this.tag)
      this.tag = null
    }
  }

  get(): string | null {
    return this.tag
  }
}

/**
 * A component that toggles the visibility of a DOM element using a CSS class.
 *
 * When set to true, the element is visible (class is removed).
 * When set to false, the element is hidden (class is added).
 */
export class Toggle implements Component<boolean> {
  private domClass: DomClass
  private hidden: string

  constructor(dom: HTMLElement, hidden: string) {
    this.domClass = new DomClass(dom)
    this.hidden = hidden
    this.domClass.set(hidden)
  }

  set(vis: boolean): boolean {
    return this.domClass.set(vis ? null : this.hidden)
  }

  destroy(): void {
    this.domClass.destroy()
  }

  toggle(): void {
    this.set(!this.isVisible())
  }

  isVisible(): boolean {
    return this.domClass.get() === null
  }
}
