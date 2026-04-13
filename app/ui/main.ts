import { srvConnect } from "./common-js/connect.ts"
import type { Connection, Question } from "./common-js/connect.ts"
import { uiFromTemplate } from "./common-js/template.ts"
import { BoardComponent } from "./boardComponent.ts"
import type { StateView, PlayerId, PlayerState, Action, ActionAmount, Input } from "./protocol.ts"
import { List } from "./common-js/combinators.ts"
import { ActionComponent } from "./actionComponent.ts"
import { PlayerComponent } from "./playerComponent.ts"
import { configureQuestionActions, registerQuestionCleanup, respondToQuestion } from "./questionActions"

type GUI = {
  questionContainer: HTMLElement,
  buttonsContainer: HTMLElement,
  questionsContainer: HTMLElement[],
  questionCleanup: Array<() => void>,
  boardComponent: BoardComponent,
  actionsComponent: List<[Action, ActionAmount]>,
  playersComponent: List<[PlayerId, PlayerState]>
}

type GameState = {
  game: StateView,
  questions: [string, Question<Input>[]]
}

export let conn: Connection<Input>;
let gui: GUI;

const actionOrder: Action[] = ["move", "summon", "follower", "power", "testSplitRegion", "testBid", "testPlayCards"]

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
  document.getElementById("actions-container")!.innerHTML = ""
  document.getElementById("players-container")!.innerHTML = ""
  questionContainer.innerHTML = ""
  buttonsContainer.innerHTML = ""

  // Create components
  gui = {
    questionContainer,
    buttonsContainer,
    questionsContainer: [],
    questionCleanup: [],
    boardComponent: new BoardComponent(),
    actionsComponent: new List<[Action, ActionAmount]>(() => new ActionComponent()),
    playersComponent: new List<[PlayerId, PlayerState]>(() => new PlayerComponent())
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
  gui.questionContainer.textContent = q
  gui.buttonsContainer.innerHTML = ""
  gui.questionsContainer = []
}

// Handle AskBid question with slider and button
function handleAskBidQuestion(maxBid: number, q: Question<Input>) {
  const container = document.createElement("div")
  container.className = "bid-container"

  const slider = document.createElement("input")
  slider.type = "range"
  slider.min = "0"
  slider.max = maxBid.toString()
  slider.value = "0"
  slider.className = "bid-slider"

  const valueDisplay = document.createElement("span")
  valueDisplay.className = "bid-value"
  valueDisplay.textContent = "0"

  slider.addEventListener("input", () => {
    valueDisplay.textContent = slider.value
  })

  const button = uiFromTemplate("template-btn")
  button.textContent = "Bid"
  button.title = q.chHelp

  button.addEventListener("click", () => {
    const modifiedQuestion = {
      ...q,
      chChoice: {
        tag: "AskBid" as const,
        contents: parseInt(slider.value)
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
      handleAskBidQuestion(q.chChoice.contents, q)
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
        const card = q.chChoice.contents
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

  // Update players display using component
  gui.playersComponent.set(state.players)
}
