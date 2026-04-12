module App.KOI
  ( KOI(..)
  , Interact
  , questionFor
  , module KOI.Interact
  ) where

import Data.Text qualified as T

import KOI.Basics (PlayerId(..))
import KOI.Interact hiding (Interact)
import KOI.Interact qualified as I

import App.State(State, stateIsFinal)
import App.StateView(StateView, getStateView)
import App.Input(Input(..))

data KOI = KOI

type Interact = I.Interact KOI

questionFor :: PlayerId -> T.Text -> T.Text
questionFor (PlayerId pid) question = pid <> ": " <> question

instance Component KOI where
  type AppState       KOI = State
  type AppStateView   KOI = StateView
  type AppUpdate      KOI = State
  type AppUpdateView  KOI = StateView
  type AppInput       KOI = Input

  doUpdate _ s _   = s
  playerView _     = getStateView
  playerUpdateView = playerView
  finalState _     = stateIsFinal

  validInput _ (AskBid maxBid) (AskBid bid) = bid >= 0 && bid <= maxBid
  validInput _ expected response = expected == response
