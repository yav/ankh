import { renderGrid, type GridConfig, WARM_PALETTE, LocInfo, type TerrainType, type EdgeTerrainType, type ItemKind } from "./grid-renderer.ts"
import { FLocMap, ELocMap } from "hex"

function createDebugHoverControl(config: GridConfig, updateGrid: () => void): HTMLElement {
  const debugSection = document.createElement("div")
  debugSection.className = "control-section-inline"

  const debugLabel = document.createElement("label")
  const debugCheckbox = document.createElement("input")
  debugCheckbox.type = "checkbox"
  debugCheckbox.id = "debug-hover-checkbox"
  debugCheckbox.checked = config.debugHover

  const debugText = document.createElement("span")
  debugText.textContent = "Debug Hover"

  debugLabel.appendChild(debugCheckbox)
  debugLabel.appendChild(debugText)

  debugCheckbox.addEventListener("change", () => {
    config.debugHover = debugCheckbox.checked
    updateGrid()
  })

  debugSection.appendChild(debugLabel)
  return debugSection
}

function createEditModeControl(config: GridConfig, updateGrid: () => void): HTMLElement {
  const editSection = document.createElement("div")
  editSection.className = "control-section"

  const editLabel = document.createElement("label")
  editLabel.textContent = "Edit Mode:"
  editSection.appendChild(editLabel)

  // Create container for radio buttons (inline row)
  const radioContainer = document.createElement("div")
  radioContainer.className = "radio-group"

  // Create radio button for "None"
  const noneLabel = document.createElement("label")
  noneLabel.className = "radio-label"
  const noneRadio = document.createElement("input")
  noneRadio.type = "radio"
  noneRadio.name = "edit-mode"
  noneRadio.value = "none"
  noneRadio.checked = config.editMode === "none"
  const noneText = document.createElement("span")
  noneText.textContent = "None"
  noneLabel.appendChild(noneRadio)
  noneLabel.appendChild(noneText)
  radioContainer.appendChild(noneLabel)

  // Create radio button for "Add"
  const addLabel = document.createElement("label")
  addLabel.className = "radio-label"
  const addRadio = document.createElement("input")
  addRadio.type = "radio"
  addRadio.name = "edit-mode"
  addRadio.value = "add"
  addRadio.checked = config.editMode === "add"
  const addText = document.createElement("span")
  addText.textContent = "Add"
  addLabel.appendChild(addRadio)
  addLabel.appendChild(addText)
  radioContainer.appendChild(addLabel)

  // Create radio button for "Remove"
  const removeLabel = document.createElement("label")
  removeLabel.className = "radio-label"
  const removeRadio = document.createElement("input")
  removeRadio.type = "radio"
  removeRadio.name = "edit-mode"
  removeRadio.value = "remove"
  removeRadio.checked = config.editMode === "remove"
  const removeText = document.createElement("span")
  removeText.textContent = "Remove"
  removeLabel.appendChild(removeRadio)
  removeLabel.appendChild(removeText)
  radioContainer.appendChild(removeLabel)

  // Create radio button for "Terrain"
  const terrainLabel = document.createElement("label")
  terrainLabel.className = "radio-label"
  const terrainRadio = document.createElement("input")
  terrainRadio.type = "radio"
  terrainRadio.name = "edit-mode"
  terrainRadio.value = "terrain"
  terrainRadio.checked = config.editMode === "terrain"
  const terrainText = document.createElement("span")
  terrainText.textContent = "Terrain"
  terrainLabel.appendChild(terrainRadio)
  terrainLabel.appendChild(terrainText)
  radioContainer.appendChild(terrainLabel)

  editSection.appendChild(radioContainer)

  // Add event listeners
  const updateEditMode = (mode: "none" | "add" | "remove" | "terrain") => {
    config.editMode = mode
    updateGrid()
  }

  noneRadio.addEventListener("change", () => updateEditMode("none"))
  addRadio.addEventListener("change", () => updateEditMode("add"))
  removeRadio.addEventListener("change", () => updateEditMode("remove"))
  terrainRadio.addEventListener("change", () => updateEditMode("terrain"))

  return editSection
}

function createItemTypeSelector(config: GridConfig, updateGrid: () => void): HTMLElement {
  const container = document.createElement("div")
  container.style.flex = "1"
  container.style.minWidth = "150px"

  const label = document.createElement("label")
  label.textContent = "Player:"
  container.appendChild(label)

  const radioGroup = document.createElement("div")
  radioGroup.style.marginTop = "8px"

  // Generate player options from the palette
  config.palette.players.forEach((playerColor, index) => {
    const playerNumber = index + 1

    const radioLabel = document.createElement("label")
    radioLabel.style.display = "block"
    radioLabel.style.marginBottom = "3px"
    radioLabel.style.cursor = "pointer"

    const radio = document.createElement("input")
    radio.type = "radio"
    radio.name = "player"
    radio.value = String(playerNumber)
    radio.checked = config.selectedPlayer === playerNumber

    const colorIndicator = document.createElement("span")
    colorIndicator.className = "item-type-indicator"
    colorIndicator.style.backgroundColor = playerColor

    radio.addEventListener("change", () => {
      config.selectedPlayer = playerNumber
      updateGrid()
    })

    radioLabel.appendChild(radio)
    radioLabel.appendChild(document.createTextNode(" "))
    radioLabel.appendChild(colorIndicator)
    radioLabel.appendChild(document.createTextNode(`Player ${playerNumber}`))
    radioGroup.appendChild(radioLabel)
  })

  container.appendChild(radioGroup)

  return container
}

function createItemKindSelector(config: GridConfig, updateGrid: () => void): HTMLElement {
  const container = document.createElement("div")
  container.style.flex = "1"
  container.style.minWidth = "150px"

  const label = document.createElement("label")
  label.textContent = "Item Kind:"
  container.appendChild(label)

  const radioGroup = document.createElement("div")
  radioGroup.style.marginTop = "8px"

  const itemKinds: Array<{ id: ItemKind, displayName: string }> = [
    { id: "god", displayName: "God" },
    { id: "soldier", displayName: "Soldier" },
    { id: "temple", displayName: "Temple" },
    { id: "obelisk", displayName: "Obelisk" },
    { id: "pyramid", displayName: "Pyramid" },
  ]

  for (const itemKind of itemKinds) {
    const radioLabel = document.createElement("label")
    radioLabel.style.display = "block"
    radioLabel.style.marginBottom = "3px"
    radioLabel.style.cursor = "pointer"

    const radio = document.createElement("input")
    radio.type = "radio"
    radio.name = "itemKind"
    radio.value = itemKind.id
    radio.checked = config.selectedItemKind === itemKind.id

    radio.addEventListener("change", () => {
      config.selectedItemKind = itemKind.id
      updateGrid()
    })

    radioLabel.appendChild(radio)
    radioLabel.appendChild(document.createTextNode(" "))
    radioLabel.appendChild(document.createTextNode(itemKind.displayName))
    radioGroup.appendChild(radioLabel)
  }

  container.appendChild(radioGroup)

  return container
}

function createItemSelectors(config: GridConfig, updateGrid: () => void): HTMLElement {
  const container = document.createElement("div")
  container.className = "control-section"
  container.style.marginBottom = "10px"

  // Flex container for item selectors
  const flexContainer = document.createElement("div")
  flexContainer.style.display = "flex"
  flexContainer.style.gap = "20px"
  flexContainer.style.flexWrap = "wrap"

  // Create both selectors
  const itemTypeSelector = createItemTypeSelector(config, updateGrid)
  const itemKindSelector = createItemKindSelector(config, updateGrid)

  flexContainer.appendChild(itemTypeSelector)
  flexContainer.appendChild(itemKindSelector)
  container.appendChild(flexContainer)

  // Show only when in add mode
  container.style.display = config.editMode === "add" ? "block" : "none"

  return container
}

function createTerrainTypeSelector(config: GridConfig, updateGrid: () => void): HTMLElement {
  const container = document.createElement("div")
  container.className = "control-section"
  container.style.marginBottom = "10px"

  // Flex container for terrain sections
  const flexContainer = document.createElement("div")
  flexContainer.style.display = "flex"
  flexContainer.style.gap = "20px"
  flexContainer.style.flexWrap = "wrap"

  // Hex terrain types section
  const hexSection = document.createElement("div")
  hexSection.style.flex = "1"
  hexSection.style.minWidth = "150px"

  const hexLabel = document.createElement("label")
  hexLabel.textContent = "Hex Terrain:"
  hexSection.appendChild(hexLabel)

  const hexRadioGroup = document.createElement("div")
  hexRadioGroup.style.marginTop = "8px"

  const hexTerrainTypes: Array<{ id: TerrainType, displayName: string }> = [
    { id: "plains", displayName: "Plains" },
    { id: "desert", displayName: "Desert" },
    { id: "water", displayName: "Water" },
    { id: "deleted", displayName: "Delete" },
  ]

  for (const terrainType of hexTerrainTypes) {
    const radioLabel = document.createElement("label")
    radioLabel.style.display = "block"
    radioLabel.style.marginBottom = "3px"
    radioLabel.style.cursor = "pointer"

    const radio = document.createElement("input")
    radio.type = "radio"
    radio.name = "terrainType"
    radio.value = terrainType.id
    radio.checked = config.selectedTerrainType === terrainType.id

    const colorIndicator = document.createElement("span")
    colorIndicator.className = "item-type-indicator"
    colorIndicator.style.backgroundColor = config.palette.terrainColors[terrainType.id]

    radio.addEventListener("change", () => {
      config.selectedTerrainType = terrainType.id
      updateGrid()
    })

    radioLabel.appendChild(radio)
    radioLabel.appendChild(document.createTextNode(" "))
    radioLabel.appendChild(colorIndicator)
    radioLabel.appendChild(document.createTextNode(terrainType.displayName))
    hexRadioGroup.appendChild(radioLabel)
  }

  hexSection.appendChild(hexRadioGroup)
  flexContainer.appendChild(hexSection)

  // Edge terrain types section
  const edgeSection = document.createElement("div")
  edgeSection.style.flex = "1"
  edgeSection.style.minWidth = "150px"

  const edgeLabel = document.createElement("label")
  edgeLabel.textContent = "Edge Terrain:"
  edgeSection.appendChild(edgeLabel)

  const edgeRadioGroup = document.createElement("div")
  edgeRadioGroup.style.marginTop = "8px"

  const edgeTerrainTypes: Array<{ id: EdgeTerrainType, displayName: string }> = [
    { id: "water", displayName: "Water" },
    { id: "camels", displayName: "Camels" },
    { id: "deleted", displayName: "Delete" },
  ]

  for (const terrainType of edgeTerrainTypes) {
    const radioLabel = document.createElement("label")
    radioLabel.style.display = "block"
    radioLabel.style.marginBottom = "3px"
    radioLabel.style.cursor = "pointer"

    const radio = document.createElement("input")
    radio.type = "radio"
    radio.name = "edgeTerrainType"
    radio.value = terrainType.id
    radio.checked = config.selectedEdgeTerrainType === terrainType.id

    const colorIndicator = document.createElement("span")
    colorIndicator.className = "item-type-indicator"
    colorIndicator.style.backgroundColor = config.palette.edgeTerrainColors[terrainType.id]

    radio.addEventListener("change", () => {
      config.selectedEdgeTerrainType = terrainType.id
      updateGrid()
    })

    radioLabel.appendChild(radio)
    radioLabel.appendChild(document.createTextNode(" "))
    radioLabel.appendChild(colorIndicator)
    radioLabel.appendChild(document.createTextNode(terrainType.displayName))
    edgeRadioGroup.appendChild(radioLabel)
  }

  edgeSection.appendChild(edgeRadioGroup)
  flexContainer.appendChild(edgeSection)

  container.appendChild(flexContainer)

  // Show only when in terrain mode
  container.style.display = config.editMode === "terrain" ? "block" : "none"

  return container
}


function createRectangularRegionControl(config: GridConfig, updateGrid: () => void): HTMLElement {
  const container = document.createElement("div")
  container.className = "control-section"

  const rectWidthLabel = document.createElement("label")
  rectWidthLabel.textContent = `Width: ${config.rectWidth}`
  const rectWidthInput = document.createElement("input")
  rectWidthInput.type = "range"
  rectWidthInput.min = "1"
  rectWidthInput.max = "20"
  rectWidthInput.value = String(config.rectWidth)
  rectWidthInput.addEventListener("input", () => {
    config.rectWidth = parseInt(rectWidthInput.value)
    rectWidthLabel.textContent = `Width: ${config.rectWidth}`
    updateGrid()
  })

  const rectHeightLabel = document.createElement("label")
  rectHeightLabel.textContent = `Height: ${config.rectHeight}`
  const rectHeightInput = document.createElement("input")
  rectHeightInput.type = "range"
  rectHeightInput.min = "1"
  rectHeightInput.max = "20"
  rectHeightInput.value = String(config.rectHeight)
  rectHeightInput.addEventListener("input", () => {
    config.rectHeight = parseInt(rectHeightInput.value)
    rectHeightLabel.textContent = `Height: ${config.rectHeight}`
    updateGrid()
  })

  const rectStartsWideLabel = document.createElement("label")
  const rectStartsWideCheckbox = document.createElement("input")
  rectStartsWideCheckbox.type = "checkbox"
  rectStartsWideCheckbox.checked = config.rectStartsWide
  rectStartsWideCheckbox.addEventListener("change", () => {
    config.rectStartsWide = rectStartsWideCheckbox.checked
    updateGrid()
  })
  const rectStartsWideText = document.createElement("span")
  rectStartsWideText.textContent = " Starts Wide"
  rectStartsWideLabel.appendChild(rectStartsWideCheckbox)
  rectStartsWideLabel.appendChild(rectStartsWideText)

  container.appendChild(rectWidthLabel)
  container.appendChild(rectWidthInput)
  container.appendChild(document.createElement("br"))
  container.appendChild(rectHeightLabel)
  container.appendChild(rectHeightInput)
  container.appendChild(document.createElement("br"))
  container.appendChild(rectStartsWideLabel)

  return container
}

function createControls(leftPane: HTMLElement, config: GridConfig): HTMLElement {
  const controlsContainer = document.createElement("div")

  // Helper function to re-render grid
  const updateGrid = () => {
    renderGrid(leftPane, config)
  }

  // Add controls - top row with debug hover
  const topRow = document.createElement("div")
  topRow.className = "controls-top-row"
  topRow.appendChild(createDebugHoverControl(config, updateGrid))
  controlsContainer.appendChild(topRow)

  controlsContainer.appendChild(createRectangularRegionControl(config, updateGrid))

  // Create edit mode control
  const editModeControl = createEditModeControl(config, updateGrid)
  controlsContainer.appendChild(editModeControl)

  // Create item selectors (color and kind)
  const itemSelectors = createItemSelectors(config, updateGrid)
  controlsContainer.appendChild(itemSelectors)

  // Create terrain type selector
  const terrainTypeSelector = createTerrainTypeSelector(config, updateGrid)
  controlsContainer.appendChild(terrainTypeSelector)

  // Update edit mode control to toggle type selector visibility
  const editModeRadios = editModeControl.querySelectorAll('input[name="edit-mode"]')
  editModeRadios.forEach((radio) => {
    radio.addEventListener("change", () => {
      itemSelectors.style.display = config.editMode === "add" ? "block" : "none"
      terrainTypeSelector.style.display = config.editMode === "terrain" ? "block" : "none"
    })
  })

  return controlsContainer
}

function main() {
  const leftPane = document.getElementById("left-pane")
  const rightPane = document.getElementById("right-pane")

  if (!leftPane || !rightPane) return

  // Render initial grid
  const initialConfig: GridConfig = {
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
    selectedEdgeTerrainType: "deleted",  // Default edge terrain type
    palette: WARM_PALETTE
  }
  renderGrid(leftPane, initialConfig)

  // Create and add controls to right pane
  rightPane.innerHTML = ""
  const controls = createControls(leftPane, initialConfig)
  rightPane.appendChild(controls)
}

main()
