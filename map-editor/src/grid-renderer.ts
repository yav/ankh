import { Grid } from "hex"
import { FLoc, ELoc } from "hex"
import { newHexShape, newEdgeShape } from "hex"
import { RectangularRegion } from "hex"
import { FLocMap, ELocMap } from "hex"

// Terrain types for hexagons
export type TerrainType = "plains" | "desert" | "water" | "deleted"

// Terrain types for edges
export type EdgeTerrainType = "deleted" | "water" | "camels"

// Item kind represents the actual game entity type
export type ItemKind = "god" | "soldier" | "temple" | "obelisk" | "pyramid"

// Counter for generating unique item IDs
let nextItemId = 0

// Item class represents a game piece on the board
export class Item {
  player: number  // Player number (1-5)
  kind: ItemKind
  id: number  // Unique identifier for this item

  constructor(player: number, kind: ItemKind) {
    this.player = player
    this.kind = kind
    this.id = nextItemId++
  }
}

// Class to track hexagon information (terrain and items)
export class LocInfo {
  terrain: TerrainType
  items: Item[]

  constructor(terrain: TerrainType, items: Item[] = []) {
    this.terrain = terrain
    this.items = items
  }
}


export interface GridConfig {
  rectWidth: number
  rectHeight: number
  rectStartsWide: boolean
  debugHover: boolean
  hexInfo: FLocMap<LocInfo> // Maps hexagon locations to hex information (terrain and items)
  edgeInfo: ELocMap<EdgeTerrainType> // Maps edge locations to terrain type
  editMode: "none" | "add" | "remove" | "terrain"
  selectedPlayer: number // Currently selected player (1-5) for add mode
  selectedItemKind: ItemKind // Currently selected item kind for add mode
  selectedTerrainType: TerrainType // Currently selected hex terrain type for terrain mode
  selectedEdgeTerrainType: EdgeTerrainType // Currently selected edge terrain type for terrain mode
}

export function renderGrid(leftPane: HTMLElement, config: GridConfig) {
  // Clear the left pane and set up for absolute positioning
  leftPane.innerHTML = ""
  leftPane.style.position = "relative"

  // Create debug info tooltip if debug mode is enabled
  let debugTooltip: HTMLDivElement | null = null
  if (config.debugHover) {
    debugTooltip = document.createElement("div")
    debugTooltip.className = "debug-tooltip"
    leftPane.appendChild(debugTooltip)
  }

  // Create and configure the grid
  const grid = new Grid()
  grid.setOrientation("edge_up")
  grid.setInnerDiameter(64)
  grid.setSpacing(16)

  // Create rectangular region
  const region = new RectangularRegion(
    new FLoc(),
    config.rectWidth,
    config.rectHeight,
    config.rectStartsWide,
    "edge_up"
  )

  // Calculate the bounding box of the entire region to find the offset needed
  let minX = Infinity
  let minY = Infinity
  let maxX = -Infinity
  let maxY = -Infinity

  // Check all faces
  for (const loc of region.faces()) {
    const [x, y] = grid.faceLoc(loc)
    const [width, height] = grid.faceBoundingBox()
    minX = Math.min(minX, x - width / 2)
    minY = Math.min(minY, y - height / 2)
    maxX = Math.max(maxX, x + width / 2)
    maxY = Math.max(maxY, y + height / 2)
  }

  // Add padding for the offset
  const padding = 20
  const offsetX = -minX + padding
  const offsetY = -minY + padding

  // Create a container div for the grid with explicit dimensions
  const gridContainer = document.createElement("div")
  gridContainer.className = "grid-container"
  gridContainer.style.width = `${maxX - minX + 2 * padding}px`
  gridContainer.style.height = `${maxY - minY + 2 * padding}px`
  leftPane.append(gridContainer)

  // Create render context
  const ctx: RenderContext = {
    grid,
    config,
    offsetX,
    offsetY,
    gridContainer,
    leftPane,
    debugTooltip
  }

  // Render all hexagons (faces)
  for (const loc of region.faces()) {
    renderHexagon(ctx, loc)
  }

  // Render all edges
  for (const edge of region.edges()) {
    renderEdge(ctx, edge)
  }
}

/**
 * Adds debug hover functionality to an element, displaying the location's string
 * representation in a tooltip when the user hovers over the element.
 */
function addDebugHover<T extends { toString(): string }>(
  element: HTMLElement,
  location: T,
  debugTooltip: HTMLDivElement,
  leftPane: HTMLElement
) {
  element.addEventListener("mouseenter", () => {
    debugTooltip.textContent = location.toString()
    debugTooltip.style.display = "block"
  })

  element.addEventListener("mousemove", (e: MouseEvent) => {
    debugTooltip.style.left = `${e.pageX - leftPane.offsetLeft + 10}px`
    debugTooltip.style.top = `${e.pageY - leftPane.offsetTop + 10}px`
  })

  element.addEventListener("mouseleave", () => {
    debugTooltip.style.display = "none"
  })
}

/**
 * Shared rendering context passed to all location rendering calls.
 * Groups together all the common parameters needed for rendering to avoid
 * passing many individual parameters.
 */
interface RenderContext {
  grid: Grid
  config: GridConfig
  offsetX: number
  offsetY: number
  gridContainer: HTMLElement
  leftPane: HTMLElement
  debugTooltip: HTMLDivElement | null
}

/**
 * Renders items placed on a hexagon location.
 */
function renderHexagonItems(
  ctx: RenderContext,
  loc: FLoc,
  data: LocInfo
) {
  const { grid, config, offsetX, offsetY, gridContainer, leftPane } = ctx

  if (data.items.length === 0) return

  // Position patterns for different hexagon element counts
  const positions: [number, number][][] = [
    [], // 0 elements
    [[0, 0]], // 1 element: center
    [[-6, 0], [6, 0]], // 2 elements: horizontal line, 12px spacing
    [[-12, 0], [0, 0], [12, 0]], // 3 elements: horizontal line, 12px spacing
    [[-6, -6], [6, -6], [-6, 6], [6, 6]], // 4 elements: 2x2 grid, 12px spacing
    [[-12, -6], [0, -6], [12, -6], [-6, 6], [6, 6]], // 5 elements: 3 top, 2 bottom, 12px spacing
    [[-12, -6], [0, -6], [12, -6], [-12, 6], [0, 6], [12, 6]], // 6 elements: 2x3 grid, 12px spacing
  ]
  const itemPositions = positions[data.items.length] || []
  const [centerX, centerY] = grid.faceLoc(loc)
  const elementSize = 8
  const borderWidth = 1
  const totalSize = elementSize + 2 * borderWidth

  data.items.forEach((item: Item, i: number) => {
    const [dx, dy] = itemPositions[i]
    const x = centerX + dx + offsetX
    const y = centerY + dy + offsetY

    const element = document.createElement("div")
    element.className = "hex-element"
    element.classList.add(`player-${item.player}-color`)
    element.style.left = `${x - totalSize / 2}px`
    element.style.top = `${y - totalSize / 2}px`
    element.textContent = item.kind.charAt(0).toUpperCase()

    // Add cursor style and click handler for remove mode
    if (config.editMode === "remove") {
      element.style.cursor = "not-allowed"
      element.style.pointerEvents = "auto"
      // Capture the item's unique ID
      const itemId = item.id
      element.addEventListener("click", (e) => {
        e.stopPropagation()
        // Remove the item with the matching ID
        const filtered = data.items.filter((it: Item) => it.id !== itemId)
        data.items.length = 0
        data.items.push(...filtered)
        renderGrid(leftPane, config)
      })
    }

    gridContainer.append(element)
  })
}

/**
 * Renders a single hexagon (face) location along with any game elements
 * placed at that location. Supports terrain changes, adding, and removing items.
 */
function renderHexagon(
  ctx: RenderContext,
  loc: FLoc
) {
  const { grid, config, offsetX, offsetY, gridContainer, leftPane, debugTooltip } = ctx
  const hexInfo = config.hexInfo

  // Get or create data for this location
  let data = hexInfo.getLoc(loc)
  if (!data) {
    data = new LocInfo("deleted")
    hexInfo.setLoc(loc, data)
  }

  const shape = newHexShape(grid, loc)
  shape.classList.add(`terrain-${data.terrain}`)
  shape.style.zIndex = "0"

  // Apply offset to position
  const currentLeft = parseFloat(shape.style.left)
  const currentTop = parseFloat(shape.style.top)
  shape.style.left = `${currentLeft + offsetX}px`
  shape.style.top = `${currentTop + offsetY}px`

  // Add cursor style based on edit mode
  if (config.editMode === "add") {
    shape.style.cursor = "copy"
  } else if (config.editMode === "remove") {
    shape.style.cursor = "default"
  } else if (config.editMode === "terrain") {
    shape.style.cursor = "crosshair"
  } else {
    shape.style.cursor = "default"
  }

  // Add click handler for terrain mode
  if (config.editMode === "terrain") {
    shape.addEventListener("click", () => {
      data.terrain = config.selectedTerrainType
      renderGrid(leftPane, config)
    })
  }

  // Add click handler for adding items
  if (config.editMode === "add") {
    shape.addEventListener("click", () => {
      if (data.items.length < 6) {
        data.items.push(new Item(config.selectedPlayer, config.selectedItemKind))
        renderGrid(leftPane, config)
      }
    })
  }

  // Add hover debug info
  if (config.debugHover && debugTooltip) {
    addDebugHover(shape, loc, debugTooltip, leftPane)
  }

  gridContainer.append(shape)

  // Render items at this location
  renderHexagonItems(ctx, loc, data)
}

/**
 * Renders a single edge location. Only supports terrain changes (no items).
 */
function renderEdge(
  ctx: RenderContext,
  loc: ELoc
) {
  const { grid, config, offsetX, offsetY, gridContainer, leftPane, debugTooltip } = ctx
  const edgeInfo = config.edgeInfo

  // Get or create terrain for this location
  let terrain = edgeInfo.getLoc(loc)
  if (!terrain) {
    terrain = "deleted"
    edgeInfo.setLoc(loc, terrain)
  }

  const shape = newEdgeShape(grid, loc)
  shape.classList.add(`edge-terrain-${terrain}`)
  shape.style.zIndex = "1"

  // Apply offset to position
  const currentLeft = parseFloat(shape.style.left)
  const currentTop = parseFloat(shape.style.top)
  shape.style.left = `${currentLeft + offsetX}px`
  shape.style.top = `${currentTop + offsetY}px`

  // Add cursor style based on edit mode
  if (config.editMode === "terrain") {
    shape.style.cursor = "crosshair"
  } else {
    shape.style.cursor = "default"
  }

  // Add click handler for terrain mode
  if (config.editMode === "terrain") {
    shape.addEventListener("click", () => {
      edgeInfo.setLoc(loc, config.selectedEdgeTerrainType)
      renderGrid(leftPane, config)
    })
  }

  // Add hover debug info
  if (config.debugHover && debugTooltip) {
    addDebugHover(shape, loc, debugTooltip, leftPane)
  }

  gridContainer.append(shape)
}
