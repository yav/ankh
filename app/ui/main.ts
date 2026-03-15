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

let conn: Connection<Input>;
let gui: GUI;

const actionOrder: Action[] = ["move", "summon", "follower", "power"]

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
function uiQuestion (q: Question<Input>) {
  function choice(lab: string, onClick?: () => void) {
    const dom = uiFromTemplate("template-btn")
    dom.textContent = lab
    dom.title = q.chHelp

    if (onClick) {
      dom.addEventListener("click", onClick)
    }

    // Add button to buttons container
    gui.buttonsContainer.appendChild(dom)
    gui.questionsContainer.push(dom)
    registerQuestionCleanup(() => dom.remove())
  }

  switch (q.chChoice.tag) {
    case "TextQuestion":
      choice(q.chChoice.contents, () => respondToQuestion(q))
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
  }
}


function uiUpdate(state: StateView) {
  // Update board display using component
  gui.boardComponent.set(state.board)

  // Update action display
  gui.actionsComponent.set(state.actions)

  // Update players display using component
  gui.playersComponent.set(state.players)
}
