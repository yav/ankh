import type { Connection, Question } from "./common-js/connect.ts"
import type { Input } from "./protocol.ts"

type QuestionActionState = {
  questionContainer: HTMLElement,
  questionsContainer: HTMLElement[],
  questionCleanup: Array<() => void>
}

let state: QuestionActionState | null = null
let connection: Connection<Input> | null = null

export function configureQuestionActions(
  newState: QuestionActionState,
  newConnection: Connection<Input>
) {
  state = newState
  connection = newConnection
}

export function registerQuestionCleanup(cleanup: () => void) {
  if (state === null) {
    throw new Error("Question cleanup is not configured")
  }
  state.questionCleanup.push(cleanup)
}

function cleanupCurrentQuestion() {
  if (state === null) {
    return
  }
  const cleanup = state.questionCleanup
  state.questionCleanup = []
  for (const fn of cleanup) fn()
  state.questionsContainer = []
  state.questionContainer.textContent = ""
}

export function cleanupQuestion() {
  cleanupCurrentQuestion()
}

export function respondToQuestion(question: Question<Input>) {
  if (state === null || connection === null) {
    throw new Error("Question response is not configured")
  }
  cleanupCurrentQuestion()
  connection.sendJSON(question)
}