import { renderGrid, type GridConfig, LocInfo, type TerrainType, type EdgeType, type ItemKind, exportGridData, importGridData, type BoardData } from "./grid-renderer.ts"
import { FLocMap, ELocMap } from "hex"

function setupControls(leftPane: HTMLElement, config: GridConfig): void {
  // Helper function to re-render grid
  const updateGrid = () => {
    renderGrid(leftPane, config)
  }

  // Get references to HTML elements
  const debugHoverCheckbox = document.getElementById("debug-hover-checkbox") as HTMLInputElement
  const rectWidthInput = document.getElementById("rect-width") as HTMLInputElement
  const rectWidthLabel = document.getElementById("rect-width-label") as HTMLLabelElement
  const rectHeightInput = document.getElementById("rect-height") as HTMLInputElement
  const rectHeightLabel = document.getElementById("rect-height-label") as HTMLLabelElement
  const rectStartsWideCheckbox = document.getElementById("rect-starts-wide") as HTMLInputElement
  const editModeRadios = document.querySelectorAll<HTMLInputElement>('input[name="edit-mode"]')
  const itemSelectorsContainer = document.getElementById("item-selectors") as HTMLElement
  const terrainSelectorsContainer = document.getElementById("terrain-selectors") as HTMLElement
  const itemKindRadios = document.querySelectorAll<HTMLInputElement>('input[name="itemKind"]')
  const playerRadios = document.querySelectorAll<HTMLInputElement>('input[name="player"]')
  const terrainTypeRadios = document.querySelectorAll<HTMLInputElement>('input[name="terrainType"]')
  const edgeTerrainTypeRadios = document.querySelectorAll<HTMLInputElement>('input[name="edgeTerrainType"]')
  const saveButton = document.getElementById("save-button") as HTMLButtonElement
  const loadButton = document.getElementById("load-button") as HTMLButtonElement
  const loadFileInput = document.getElementById("load-file-input") as HTMLInputElement

  // Wire up event handlers
  debugHoverCheckbox.addEventListener("change", () => {
    config.debugHover = debugHoverCheckbox.checked
    updateGrid()
  })

  rectWidthInput.addEventListener("input", () => {
    config.rectWidth = parseInt(rectWidthInput.value)
    rectWidthLabel.textContent = `Width: ${config.rectWidth}`
    updateGrid()
  })

  rectHeightInput.addEventListener("input", () => {
    config.rectHeight = parseInt(rectHeightInput.value)
    rectHeightLabel.textContent = `Height: ${config.rectHeight}`
    updateGrid()
  })

  rectStartsWideCheckbox.addEventListener("change", () => {
    config.rectStartsWide = rectStartsWideCheckbox.checked
    updateGrid()
  })

  editModeRadios.forEach((radio) => {
    radio.addEventListener("change", () => {
      config.editMode = radio.value as "none" | "add" | "remove" | "terrain"
      itemSelectorsContainer.style.display = config.editMode === "add" ? "block" : "none"
      terrainSelectorsContainer.style.display = config.editMode === "terrain" ? "block" : "none"
      updateGrid()
    })
  })

  itemKindRadios.forEach((radio) => {
    radio.addEventListener("change", () => {
      config.selectedItemKind = radio.value as ItemKind
      updateGrid()
    })
  })

  playerRadios.forEach((radio) => {
    radio.addEventListener("change", () => {
      config.selectedPlayer = parseInt(radio.value)
      updateGrid()
    })
  })

  terrainTypeRadios.forEach((radio) => {
    radio.addEventListener("change", () => {
      config.selectedTerrainType = radio.value as TerrainType
      updateGrid()
    })
  })

  edgeTerrainTypeRadios.forEach((radio) => {
    radio.addEventListener("change", () => {
      config.selectedEdgeType = radio.value as EdgeType
      updateGrid()
    })
  })

  // Save button handler
  saveButton.addEventListener("click", () => {
    const boardData = exportGridData(config)
    const json = JSON.stringify(boardData, null, 2)
    const blob = new Blob([json], { type: "application/json" })
    const url = URL.createObjectURL(blob)
    const a = document.createElement("a")
    a.href = url
    a.download = "board-data.json"
    a.click()
    URL.revokeObjectURL(url)
  })

  // Load button handler
  loadButton.addEventListener("click", () => {
    loadFileInput.click()
  })

  // File input handler
  loadFileInput.addEventListener("change", async () => {
    const file = loadFileInput.files?.[0]
    if (!file) return

    try {
      const text = await file.text()
      const data = JSON.parse(text)

      // Validate the data structure
      if (!validateBoardData(data)) {
        alert("Invalid board data format. Please check the file.")
        return
      }

      // Import the data and update config
      const newConfig = importGridData(data)

      // Update all config properties
      Object.assign(config, newConfig)

      // Update UI controls to match new config
      rectWidthInput.value = config.rectWidth.toString()
      rectWidthLabel.textContent = `Width: ${config.rectWidth}`
      rectHeightInput.value = config.rectHeight.toString()
      rectHeightLabel.textContent = `Height: ${config.rectHeight}`
      rectStartsWideCheckbox.checked = config.rectStartsWide

      // Re-render grid
      updateGrid()
    } catch (error) {
      alert(`Error loading file: ${error instanceof Error ? error.message : "Unknown error"}`)
    }

    // Reset file input
    loadFileInput.value = ""
  })
}

// Validates that the data matches the BoardData structure
function validateBoardData(data: unknown): data is BoardData {
  if (typeof data !== "object" || data === null) return false

  const d = data as Record<string, unknown>

  // Check required top-level fields
  if (typeof d.width !== "number" || typeof d.height !== "number" || typeof d.startsWide !== "boolean") {
    return false
  }

  // Check hexagons array
  if (!Array.isArray(d.hexagons)) return false
  for (const hex of d.hexagons) {
    if (typeof hex !== "object" || hex === null) return false
    const h = hex as Record<string, unknown>
    if (typeof h.location !== "object" || h.location === null) return false
    const loc = h.location as Record<string, unknown>
    if (typeof loc.x !== "number" || typeof loc.y !== "number") return false
    if (typeof h.terrain !== "string") return false
    if (!Array.isArray(h.items)) return false
    for (const item of h.items) {
      if (typeof item !== "object" || item === null) return false
      const i = item as Record<string, unknown>
      if (typeof i.player !== "number" || typeof i.kind !== "string") return false
    }
  }

  // Check edges array
  if (!Array.isArray(d.edges)) return false
  for (const edge of d.edges) {
    if (typeof edge !== "object" || edge === null) return false
    const e = edge as Record<string, unknown>
    if (typeof e.location !== "object" || e.location === null) return false
    const loc = e.location as Record<string, unknown>
    if (typeof loc.x !== "number" || typeof loc.y !== "number" || typeof loc.edge !== "number") return false
    if (typeof e.type !== "string") return false
  }

  return true
}

function main() {
  const leftPane = document.getElementById("left-pane")

  if (!leftPane) return

  // Initialize grid configuration
  const config: GridConfig = {
    rectWidth: 7,
    rectHeight: 10,
    rectStartsWide: true,
    debugHover: false,
    hexInfo: new FLocMap<LocInfo>(),
    edgeInfo: new ELocMap<EdgeType>(),
    editMode: "none",
    selectedPlayer: 1,  // Default to Player 1
    selectedItemKind: "soldier",  // Default item kind
    selectedTerrainType: "plains",  // Default hex terrain type
    selectedEdgeType: "deleted"  // Default edge type
  }

  // Render initial grid
  renderGrid(leftPane, config)

  // Wire up controls
  setupControls(leftPane, config)
}

main()
