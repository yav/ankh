import { srvConnect, Connection, Question } from "./common-js/connect.ts"
import { uiFromTemplate } from "./common-js/template.ts"
import { BoardComponent } from "./boardComponent.ts"
import type { StateView } from "./protocol.ts"
import { List } from "./common-js/combinators.ts"
import { PlayerComponent, type PlayerData } from "./playerComponent.ts"

// Game input types (currently placeholder)
type Q = "Placeholder"

type GUI = {
  boardContainer: HTMLElement,
  playersContainer: HTMLElement,
  questionContainer: HTMLElement,
  buttonsContainer: HTMLElement,
  questionsContainer: HTMLElement[],
  boardComponent: BoardComponent | null,
  playersComponent: List<PlayerData> | null
}

type GameState = {
  game: StateView,
  questions: [string, Question<Q>[]]
}

let conn: Connection<Q>;
let gui: GUI;

export default
function main () {
  const cb = {
    uiRedraw,
    uiSetQuestion,
    uiQuestion,
    uiUpdate
  }
  conn = srvConnect(cb)
}

// Redraw the whole state
function uiRedraw (state: GameState) {

  // Get containers from existing HTML
  const boardContainer = document.getElementById("board-container")
  const playersContainer = document.getElementById("players-container")
  const questionContainer = document.getElementById("question-container")
  const buttonsContainer = document.getElementById("buttons-container")

  if (!boardContainer) throw new Error("Failed to find board-container")
  if (!playersContainer) throw new Error("Failed to find players-container")
  if (!questionContainer) throw new Error("Failed to find question-container")
  if (!buttonsContainer) throw new Error("Failed to find buttons-container")

  gui = {
    boardContainer,
    playersContainer,
    questionContainer,
    buttonsContainer,
    questionsContainer: [],
    boardComponent: null,
    playersComponent: null
  }

  // Clear containers
  boardContainer.innerHTML = ""
  playersContainer.innerHTML = ""
  questionContainer.innerHTML = ""
  buttonsContainer.innerHTML = ""

  // Create components
  gui.boardComponent = new BoardComponent(gui.boardContainer)

  // Create players list component
  gui.playersComponent = new List<PlayerData>(() => {
    return new PlayerComponent(gui.playersContainer)
  })

  uiUpdate(state.game)
  conn.uiQuestions(state.questions)
}


// Set the explanation for what we are asking.
function uiSetQuestion (q: string) {
  gui.questionContainer.textContent = q
  gui.buttonsContainer.innerHTML = ""
  gui.questionsContainer = []
}

// Various things that can be used to answer the question.
function uiQuestion (q: Question<Q>) {

  function btn(lab: string) {
    const dom = uiFromTemplate("template-btn")
    dom.textContent = lab
    dom.addEventListener("click", () => {
      const n = gui.questionsContainer.length
      for (let i = 0; i < n; ++i) gui.questionsContainer[i].remove()
      gui.questionContainer.textContent = ""
      conn.sendJSON(q)
    })

    // Add button to buttons container
    gui.buttonsContainer.appendChild(dom)
    gui.questionsContainer.push(dom)
  }

  // Display the help text for the choice
  btn(q.chHelp)
}


function uiUpdate(state: StateView) {
  // Update board display using component
  if (gui.boardComponent) {
    gui.boardComponent.set(state.board)
  }

  // Update players display using component
  if (gui.playersComponent) {
    const playerData: PlayerData[] = state.players.map(([id, playerState]) => ({
      id,
      state: playerState
    }))
    gui.playersComponent.set(playerData)
  }
}
