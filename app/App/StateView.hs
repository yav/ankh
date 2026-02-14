module App.StateView where

import KOI.Basics
import Data.Aeson qualified as JS
import Data.Map qualified as Map
import App.State

newtype StateView = StateView State

getStateView :: PlayerId -> State -> StateView
getStateView _pid = StateView

instance JS.ToJSON StateView where
  toJSON (StateView st) = JS.object
    [ "board" JS..= stateBoard st
    , "players" JS..= Map.toList (statePlayers st)
    ]
