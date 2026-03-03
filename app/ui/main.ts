import { srvConnect, Connection, Question } from "./common-js/connect.ts"
import { uiFromTemplate } from "./common-js/template.ts"
import { BoardComponent } from "./boardComponent.ts"
import type { StateView, PlayerId, PlayerState } from "./protocol.ts"
import { List } from "./common-js/combinators.ts"
import { PlayerComponent } from "./playerComponent.ts"

// Game input types (currently placeholder)
type Q = "Placeholder"

type GUI = {
  questionContainer: HTMLElement,
  buttonsContainer: HTMLElement,
  questionsContainer: HTMLElement[],
  boardComponent: BoardComponent,
  playersComponent: List<[PlayerId, PlayerState]>
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
  const questionContainer = document.getElementById("question-container")!
  const buttonsContainer = document.getElementById("buttons-container")!

  // Clear containers
  document.getElementById("board-container")!.innerHTML = ""
  document.getElementById("players-container")!.innerHTML = ""
  questionContainer.innerHTML = ""
  buttonsContainer.innerHTML = ""

  // Create components
  gui = {
    questionContainer,
    buttonsContainer,
    questionsContainer: [],
    boardComponent: new BoardComponent(),
    playersComponent: new List<[PlayerId, PlayerState]>(() => new PlayerComponent())
  }

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
  gui.boardComponent.set(state.board)

  // Update players display using component
  gui.playersComponent.set(state.players)
}
