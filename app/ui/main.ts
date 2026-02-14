import { srvConnect, Connection, Question } from "./common-js/connect.ts"
import { uiFromTemplate } from "./common-js/template.ts"

// Game input types (currently placeholder)
type Q = "Placeholder"

type GUI = {
  board: HTMLElement,
  players: HTMLElement,
  question: HTMLElement,
  questions: HTMLElement[]
}

// Hexagonal coordinate system
type HexPos = {
  q: number,  // column
  r: number   // row
}

// Terrain types
type Terrain = "desert" | "grass" | "water"

// Guardian type (currently empty in server)
type GuardianType = {}

// Player piece types
type PlayerPieceType =
  | "god"
  | "soldier"
  | { tag: "guardian", contents: GuardianType }

// Structure types
type StructureType = "temple" | "obelisk" | "pyramid"

// Game pieces
type Piece =
  | { tag: "PlayerPiece", player: PlayerId, pieceType: PlayerPieceType }
  | { tag: "Structure", structureType: StructureType }

// Individual hex on the board
type Hex = {
  terrain: Terrain,
  pieces: Piece[]
}

// Game board with hexagonal grid
type Board = {
  hexes: { [key: string]: Hex },      // Key format: "q,r"
  regions: { [key: string]: HexPos[] } // Key is region ID
}

type PlayerState = {}
type PlayerId = string

type StateView = {
  board: Board,
  players: [PlayerId, PlayerState][]
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

  gui = {
    board: uiFromTemplate("template-board"),
    players: uiFromTemplate("template-players"),
    question: uiFromTemplate("template-question"),
    questions: []
  }

  const body = document.getElementById("content")
  if (body == null) { throw new Error("Failed to find the body") }

  body.innerHTML = ""
  body.appendChild(gui.board)
  body.appendChild(gui.players)
  body.appendChild(gui.question)
  uiUpdate(state.game)
  conn.uiQuestions(state.questions)
}


// Set the explanation for what we are asking.
function uiSetQuestion (q: string) {
  gui.question.textContent = q
  gui.questions = []
}

// Various things that can be used to answer the question.
function uiQuestion (q: Question<Q>) {

  function btn(lab: string) {
    const dom = uiFromTemplate("template-btn")
    dom.textContent = lab
    dom.addEventListener("click", () => {
      const n = gui.questions.length
      for (let i = 0; i < n; ++i) gui.questions[i].remove()
      gui.question.textContent = ""
      conn.sendJSON(q)
    })

    const body = document.getElementById("content")
    if (body === null) { throw new Error("Failed to find `content`") }
    body.appendChild(dom)
    gui.questions.push(dom)
  }

  // Display the help text for the choice
  btn(q.chHelp)
}


function uiUpdate(state: StateView) {
  // Update board display
  gui.board.textContent = "Board: " + JSON.stringify(state.board)

  // Update players display
  gui.players.innerHTML = "<h3>Players:</h3>"
  for (const [playerId, playerState] of state.players) {
    const playerDiv = document.createElement("div")
    playerDiv.textContent = `${playerId}: ${JSON.stringify(playerState)}`
    gui.players.appendChild(playerDiv)
  }
}
