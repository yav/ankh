module App.State where

import KOI.Basics
import qualified Data.Map as Map
import App.Board
import App.PlayerState

data State = State
  { stateBoard   :: Board
  , statePlayers :: Map.Map PlayerId PlayerState
  }
  deriving (Read, Show)

stateIsFinal :: State -> Bool
stateIsFinal _ = False
