import { Grid } from "hex"
import { FLoc, ELoc } from "hex"
import { newHexShape, newEdgeShape } from "hex"
import { RectangularRegion } from "hex"
import { FLocMap, ELocMap, type LocMap } from "hex"

// Terrain types for hexagons
export type TerrainType = "plains" | "desert" | "water" | "deleted"

// Terrain types for edges
export type EdgeTerrainType = "deleted" | "water" | "camels"

// Color palette system for theming
export interface ColorPalette {
  name: string
  edgeBackground: string
  terrainColors: {
    plains: string
    desert: string
    water: string
    deleted: string
  }
  edgeTerrainColors: {
    deleted: string
    water: string
    camels: string
  }
  itemTypes: {
    alpha: { color: string, displayName: string }
    beta: { color: string, displayName: string }
    gamma: { color: string, displayName: string }
    delta: { color: string, displayName: string }
    epsilon: { color: string, displayName: string }
  }
}

// Warm color palette (warm grid, cool items for contrast)
export const WARM_PALETTE: ColorPalette = {
  name: "Warm",
  edgeBackground: "#5D4037",      // Rich brown
  terrainColors: {
    plains: "#A5D6A7",    // Light green
    desert: "#FFE082",    // Light amber/yellow
    water: "#64B5F6",     // Light blue
    deleted: "#BDBDBD"    // Gray
  },
  edgeTerrainColors: {
    deleted: "#BDBDBD",   // Gray
    water: "#64B5F6",     // Light blue
    camels: "#D4A574"     // Sandy/camel brown
  },
  itemTypes: {
    alpha: { color: "#01579B", displayName: "Ocean" },      // Very dark blue
    beta: { color: "#0288D1", displayName: "Cyan" },        // Medium blue
    gamma: { color: "#26A69A", displayName: "Teal" },       // Medium teal
    delta: { color: "#81C784", displayName: "Green" },      // Light green
    epsilon: { color: "#C5E1A5", displayName: "Lime" },     // Very light lime
  }
}

// Cool color palette (cool grid, warm items for contrast)
export const COOL_PALETTE: ColorPalette = {
  name: "Cool",
  edgeBackground: "#1565C0",      // Deep blue
  terrainColors: {
    plains: "#C5E1A5",    // Light lime green
    desert: "#FFCC80",    // Light orange
    water: "#81D4FA",     // Bright light blue
    deleted: "#CFD8DC"    // Blue gray
  },
  edgeTerrainColors: {
    deleted: "#CFD8DC",   // Blue gray
    water: "#81D4FA",     // Bright light blue
    camels: "#FFAB91"     // Warm peach/coral
  },
  itemTypes: {
    alpha: { color: "#BF360C", displayName: "Terracotta" },      // Very dark red-orange
    beta: { color: "#E65100", displayName: "Golden Honey" },     // Dark orange
    gamma: { color: "#FF6F00", displayName: "Coral" },           // Medium amber
    delta: { color: "#FFA726", displayName: "Warm Taupe" },      // Light orange
    epsilon: { color: "#FFD54F", displayName: "Burnt Sienna" },  // Very light yellow
  }
}

// Item type system for supporting different colored boxes on the grid
export interface ItemType {
  id: string
  color: string
  displayName: string
}

export interface Item {
  type: ItemType
  id: number  // Unique identifier for this item
}

// Counter for generating unique item IDs
let nextItemId = 0

export function createItem(type: ItemType): Item {
  return {
    type,
    id: nextItemId++
  }
}

// Class to track location information (terrain and items)
export class LocInfo<T> {
  terrain: T
  items: Item[]

  constructor(terrain: T, items: Item[] = []) {
    this.terrain = terrain
    this.items = items
  }
}

// Create item types from a color palette
export function createItemTypesFromPalette(palette: ColorPalette): ItemType[] {
  return [
    { id: "alpha", color: palette.itemTypes.alpha.color, displayName: palette.itemTypes.alpha.displayName },
    { id: "beta", color: palette.itemTypes.beta.color, displayName: palette.itemTypes.beta.displayName },
    { id: "gamma", color: palette.itemTypes.gamma.color, displayName: palette.itemTypes.gamma.displayName },
    { id: "delta", color: palette.itemTypes.delta.color, displayName: palette.itemTypes.delta.displayName },
    { id: "epsilon", color: palette.itemTypes.epsilon.color, displayName: palette.itemTypes.epsilon.displayName },
  ]
}

export const ITEM_TYPES: ItemType[] = createItemTypesFromPalette(WARM_PALETTE)

export interface GridConfig {
  rectWidth: number
  rectHeight: number
  rectStartsWide: boolean
  debugHover: boolean
  hexInfo: FLocMap<LocInfo<TerrainType>> // Maps hexagon locations to hex information (terrain and items)
  edgeInfo: ELocMap<LocInfo<EdgeTerrainType>> // Maps edge locations to edge information (terrain and items)
  editMode: "none" | "add" | "remove" | "terrain"
  selectedItemType: ItemType // Currently selected item type for add mode
  selectedTerrainType: TerrainType // Currently selected hex terrain type for terrain mode
  selectedEdgeTerrainType: EdgeTerrainType // Currently selected edge terrain type for terrain mode
  palette: ColorPalette // Color palette for theming
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
    renderLocation(ctx, loc, hexRenderer, new LocInfo<TerrainType>("deleted"))
  }

  // Render all edges
  for (const edge of region.edges()) {
    renderLocation(ctx, edge, edgeRenderer, new LocInfo<EdgeTerrainType>("deleted"))
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
 * Interface for rendering different types of grid locations (hexes, edges, vertices).
 * Encapsulates all the type-specific rendering behavior and data access needed to
 * render a single location and its associated game elements.
 */
export interface LocationRenderer<T extends { toString(): string }, V, M extends LocMap<T, V>> {
  /** Creates the visual shape element for this location type */
  createShape(grid: Grid, loc: T): HTMLElement
  /** Gets the background color for this location type from the palette and location data */
  getBackgroundColor(config: GridConfig, data: V): string
  /** Z-index for layering (hexes < edges < vertices) */
  zIndex: string
  /** Maximum number of game elements that can be placed at this location */
  maxElements: number
  /** Gets the element map for tracking game elements at locations of this type */
  getElementMap(config: GridConfig): M
  /** Gets the items from the location data */
  getItems(data: V): Item[]
  /** Gets the relative positions for arranging multiple game elements */
  getElementPositions(elementCount: number): [number, number][]
  /** Gets the function to calculate the screen position of this location */
  getPositionFn(): (g: Grid, l: T) => [number, number]
}

/** Renderer for hexagon (face) locations - hexagons that can hold up to 6 elements */
const hexRenderer: LocationRenderer<FLoc, LocInfo<TerrainType>, FLocMap<LocInfo<TerrainType>>> = {
  createShape: (grid: Grid, loc: FLoc) => newHexShape(grid, loc),
  getBackgroundColor: (config: GridConfig, data: LocInfo<TerrainType>) => {
    return config.palette.terrainColors[data.terrain]
  },
  zIndex: "0",
  maxElements: 6,
  getElementMap: (config: GridConfig) => config.hexInfo,
  getItems: (data: LocInfo<TerrainType>) => data.items,
  getElementPositions: (elementCount: number) => {
    // Position patterns for different hexagon element counts
    // Positions are relative offsets from center
    // Consistent spacing: 12px between element centers (4px gap between edges)
    const positions: [number, number][][] = [
      [], // 0 elements
      [[0, 0]], // 1 element: center
      [[-6, 0], [6, 0]], // 2 elements: horizontal line, 12px spacing
      [[-12, 0], [0, 0], [12, 0]], // 3 elements: horizontal line, 12px spacing
      [[-6, -6], [6, -6], [-6, 6], [6, 6]], // 4 elements: 2x2 grid, 12px spacing
      [[-12, -6], [0, -6], [12, -6], [-6, 6], [6, 6]], // 5 elements: 3 top, 2 bottom, 12px spacing
      [[-12, -6], [0, -6], [12, -6], [-12, 6], [0, 6], [12, 6]], // 6 elements: 2x3 grid, 12px spacing
    ]
    return positions[elementCount] || []
  },
  getPositionFn: () => (g: Grid, l: FLoc) => g.faceLoc(l)
}

/** Renderer for edge locations - edges that can hold 1 element */
const edgeRenderer: LocationRenderer<ELoc, LocInfo<EdgeTerrainType>, ELocMap<LocInfo<EdgeTerrainType>>> = {
  createShape: (grid: Grid, loc: ELoc) => newEdgeShape(grid, loc),
  getBackgroundColor: (config: GridConfig, data: LocInfo<EdgeTerrainType>) => {
    return config.palette.edgeTerrainColors[data.terrain]
  },
  zIndex: "1",
  maxElements: 1,
  getElementMap: (config: GridConfig) => config.edgeInfo,
  getItems: (data: LocInfo<EdgeTerrainType>) => data.items,
  getElementPositions: () => [[0, 0]],
  getPositionFn: () => (g: Grid, l: ELoc) => g.edgeLoc(l)
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
 * Renders a single grid location (hex, edge, or vertex) along with any game elements
 * placed at that location. Uses the provided renderer to handle type-specific behavior.
 *
 * This is the main rendering function that combines:
 * - Creating and styling the location shape
 * - Positioning it with offsets
 * - Adding interaction handlers (click, hover)
 * - Rendering game elements at the location
 */
function renderLocation<T extends { toString(): string }, V, M extends LocMap<T, V>>(
  ctx: RenderContext,
  loc: T,
  renderer: LocationRenderer<T, V, M>,
  defaultData: V
) {
  const { grid, config, offsetX, offsetY, gridContainer, leftPane, debugTooltip } = ctx
  const elementMap = renderer.getElementMap(config)

  // Get or create data for this location
  let data = elementMap.getLoc(loc)
  if (!data) {
    data = defaultData
    elementMap.setLoc(loc, data)
  }

  const shape = renderer.createShape(grid, loc)
  shape.style.backgroundColor = renderer.getBackgroundColor(config, data)
  shape.style.zIndex = renderer.zIndex

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
  if (config.editMode === "terrain" && data instanceof LocInfo) {
    shape.addEventListener("click", () => {
      // Determine which terrain type to use based on location type
      if (loc instanceof FLoc) {
        (data as LocInfo<TerrainType>).terrain = config.selectedTerrainType
      } else if (loc instanceof ELoc) {
        (data as LocInfo<EdgeTerrainType>).terrain = config.selectedEdgeTerrainType
      }
      renderGrid(leftPane, config)
    })
  }

  // Add click handler for adding entities
  if (config.editMode === "add") {
    shape.addEventListener("click", () => {
      const items = renderer.getItems(data)
      if (items.length < renderer.maxElements) {
        items.push(createItem(config.selectedItemType))
        renderGrid(leftPane, config)
      }
    })
  }

  // Add hover debug info
  if (config.debugHover && debugTooltip) {
    addDebugHover(shape, loc, debugTooltip, leftPane)
  }

  gridContainer.append(shape)

  // Render elements at this location
  const items = renderer.getItems(data)
  if (items.length > 0) {
    const positions = renderer.getElementPositions(items.length)
    const getPosition = renderer.getPositionFn()
    const [centerX, centerY] = getPosition(grid, loc)
    const elementSize = 8
    const borderWidth = 1
    const totalSize = elementSize + 2 * borderWidth

    items.forEach((item: Item, i: number) => {
      const [dx, dy] = positions[i]
      const x = centerX + dx + offsetX
      const y = centerY + dy + offsetY

      const element = document.createElement("div")
      element.className = "hex-element"
      element.style.left = `${x - totalSize / 2}px`
      element.style.top = `${y - totalSize / 2}px`
      // Look up color from current palette based on item type id
      const currentItemTypes = createItemTypesFromPalette(config.palette)
      const currentType = currentItemTypes.find(t => t.id === item.type.id)
      element.style.backgroundColor = currentType ? currentType.color : item.type.color
      element.style.border = "1px solid #333"

      // Add cursor style and click handler for remove mode
      if (config.editMode === "remove") {
        element.style.cursor = "not-allowed"
        element.style.pointerEvents = "auto"
        // Capture the item's unique ID
        const itemId = item.id
        element.addEventListener("click", (e) => {
          e.stopPropagation()
          const currentItems = renderer.getItems(data)
          // Remove the item with the matching ID
          const filtered = currentItems.filter((it: Item) => it.id !== itemId)
          currentItems.length = 0
          currentItems.push(...filtered)
          renderGrid(leftPane, config)
        })
      }

      gridContainer.append(element)
    })
  }
}
