import { renderGrid, type GridConfig, LocInfo, type TerrainType, type EdgeTerrainType, type ItemKind } from "./grid-renderer.ts"
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
      config.selectedEdgeTerrainType = radio.value as EdgeTerrainType
      updateGrid()
    })
  })
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
    edgeInfo: new ELocMap<EdgeTerrainType>(),
    editMode: "none",
    selectedPlayer: 1,  // Default to Player 1
    selectedItemKind: "soldier",  // Default item kind
    selectedTerrainType: "plains",  // Default hex terrain type
    selectedEdgeTerrainType: "deleted"  // Default edge terrain type
  }

  // Render initial grid
  renderGrid(leftPane, config)

  // Wire up controls
  setupControls(leftPane, config)
}

main()
