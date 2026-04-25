import { srvConnect } from "./common-js/connect.ts"
import type { Connection, Question } from "./common-js/connect.ts"
import { uiFromTemplate } from "./common-js/template.ts"
import { BoardComponent } from "./boardComponent.ts"
import type { StateView, PlayerId, PlayerState, Action, ActionAmount, Input, LogItem } from "./protocol.ts"
import { List } from "./common-js/combinators.ts"
import { ActionComponent } from "./actionComponent.ts"
import { PlayerComponent } from "./playerComponent.ts"
import { LogItemComponent } from "./logComponent.ts"
import { configureQuestionActions, registerQuestionCleanup, respondToQuestion, cleanupQuestion } from "./questionActions"
import { StructureSupplyComponent } from "./structureSupplyComponent.ts"

type GUI = {
  questionContainer: HTMLElement,
  buttonsContainer: HTMLElement,
  questionsContainer: HTMLElement[],
  questionCleanup: Array<() => void>,
  boardComponent: BoardComponent,
  structuresComponent: StructureSupplyComponent,
  actionsComponent: List<[Action, ActionAmount]>,
  playersComponent: List<[PlayerId, PlayerState]>,
  logComponent: List<LogItem>
}

type GameState = {
  game: StateView,
  questions: [string, Question<Input>[]]
}

export let conn: Connection<Input>;
let gui: GUI;

const actionOrder: Action[] = ["move", "summon", "follower", "power", "testSplitRegion", "testBid", "testPlayCards", "testGainPoints", "testMonumentMajority", "testClaimMonument"]

function syncRegionsToggle(): void {
  const regionsToggle = document.getElementById("regions-toggle") as HTMLInputElement | null
  if (regionsToggle === null) {
    return
  }

  if (gui !== undefined) {
    gui.boardComponent.setShowRegions(regionsToggle.checked)
  }
}

export default
function main () {
  const cb = {
    uiRedraw,
    uiSetQuestion,
    uiQuestion,
    uiUpdate
  }
  conn = srvConnect(cb)

  const undoButton = document.getElementById("undo-button") as HTMLButtonElement | null
  if (undoButton !== null) {
    undoButton.addEventListener("click", () => {
      conn.sendJSON({ tag: "undo" })
    })
  }
}

// Redraw the whole state
function uiRedraw (state: GameState) {

  // Get containers from existing HTML
  const questionContainer = document.getElementById("question-container")!
  const buttonsContainer = document.getElementById("buttons-container")!
  const regionsToggle = document.getElementById("regions-toggle") as HTMLInputElement | null

  // Clear containers
  document.getElementById("board-container")!.innerHTML = ""
  document.getElementById("structures-supply")!.innerHTML = ""
  document.getElementById("actions-container")!.innerHTML = ""
  document.getElementById("players-container")!.innerHTML = ""
  document.getElementById("log-container")!.innerHTML = ""
  questionContainer.innerHTML = ""
  buttonsContainer.innerHTML = ""

  // Create components
  gui = {
    questionContainer,
    buttonsContainer,
    questionsContainer: [],
    questionCleanup: [],
    boardComponent: new BoardComponent(),
    structuresComponent: new StructureSupplyComponent(document.getElementById("structures-supply")!),
    actionsComponent: new List<[Action, ActionAmount]>(() => new ActionComponent()),
    playersComponent: new List<[PlayerId, PlayerState]>(() => new PlayerComponent()),
    logComponent: new List<LogItem>(() => new LogItemComponent())
  }

  configureQuestionActions(gui, conn)

  if (regionsToggle !== null) {
    regionsToggle.onchange = syncRegionsToggle
  }

  uiUpdate(state.game)
  if (regionsToggle !== null) {
    syncRegionsToggle()
  }
  conn.uiQuestions(state.questions)
}


// Set the explanation for what we are asking.
function uiSetQuestion (q: string) {
  cleanupQuestion()
  gui.questionContainer.textContent = q
  gui.buttonsContainer.innerHTML = ""
  gui.questionsContainer = []
}

// Handle AskBid question with slider and button
function handleAskBidQuestion(bidAmount: number, teammateBids: number[], q: Question<Input>) {
  const container = document.createElement("div")
  container.className = "bid-container"

  const slider = document.createElement("input")
  slider.type = "range"
  slider.min = "0"
  slider.max = bidAmount.toString()
  slider.value = "0"
  slider.className = "bid-slider"

  const valueDisplay = document.createElement("span")
  valueDisplay.className = "bid-value"
  valueDisplay.textContent = "0"

  slider.addEventListener("input", () => {
    valueDisplay.textContent = slider.value
  })

  if (teammateBids.length > 0) {
    const teammateBidsDisplay = document.createElement("div")
    teammateBidsDisplay.className = "teammate-bids"
    teammateBidsDisplay.textContent = `Teammate bids: ${teammateBids.join(", ")}`
    container.appendChild(teammateBidsDisplay)
  }

  const button = uiFromTemplate("template-btn")
  button.textContent = "Bid"
  button.title = q.chHelp

  button.addEventListener("click", () => {
    const modifiedQuestion = {
      ...q,
      chChoice: {
        tag: "AskBid" as const,
        contents: [parseInt(slider.value), teammateBids] as [number, number[]]
      }
    }
    respondToQuestion(modifiedQuestion)
  })

  container.appendChild(slider)
  container.appendChild(valueDisplay)
  container.appendChild(button)

  gui.buttonsContainer.appendChild(container)
  gui.questionsContainer.push(container)
  registerQuestionCleanup(() => container.remove())
}

// Various things that can be used to answer the question.
function uiQuestion (q: Question<Input>) {
  switch (q.chChoice.tag) {
    case "TextQuestion":
      {
        const dom = uiFromTemplate("template-btn")
        dom.textContent = q.chChoice.contents
        dom.title = q.chHelp
        dom.addEventListener("click", () => respondToQuestion(q))

        gui.buttonsContainer.appendChild(dom)
        gui.questionsContainer.push(dom)
        registerQuestionCleanup(() => dom.remove())
      }
      break

    case "AskBid":
      {
        const [bidAmount, teammateBids] = q.chChoice.contents
        handleAskBidQuestion(bidAmount, teammateBids, q)
      }
      break

    case "ChooseAction":
      {
        const action = q.chChoice.contents as Action
        const actionIndex = actionOrder.indexOf(action)
        const actionComponent = gui.actionsComponent.getElements()[actionIndex] as ActionComponent
        actionComponent.handleChooseActionQuestion(q)
      }
      break

    case "ChooseHex":
      gui.boardComponent.handleChooseHexQuestion(q.chChoice.contents, q)
      break

    case "ChooseEdge":
      gui.boardComponent.handleChooseEdgeQuestion(q.chChoice.contents, q)
      break

    case "ChoosePiece":
      gui.boardComponent.handleChoosePieceQuestion(q.chChoice.contents, q)
      break

    case "ChooseCard":
      {
        const [card, _teammateSelected] = q.chChoice.contents
        const playerComponents = gui.playersComponent.getElements()
        for (const playerComponent of playerComponents) {
          (playerComponent as PlayerComponent).handleChooseCardQuestion(card, q)
        }
      }
      break
  }
}


function uiUpdate(state: StateView) {
  // Update board display using component
  gui.boardComponent.set(state.board, state.splitSelection)

  // Update action display
  gui.actionsComponent.set(state.actions)

  // Update structure supply
  gui.structuresComponent.set(state.structures)

  // Update players display using component
  gui.playersComponent.set(state.players)

  // Update log display
  const logChanged = gui.logComponent.set(state.log)

  // Scroll to bottom of log if it changed
  if (logChanged) {
    const logContainer = document.getElementById("log-container")
    if (logContainer) {
      logContainer.scrollTop = logContainer.scrollHeight
    }
  }
}
