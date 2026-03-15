module App.State where

import KOI.Basics
import qualified Data.Map as Map
import App.ActionType
import App.Board
import Coord (FLoc)
import App.Piece (Piece(..), PlayerPieceType(..))
import App.PlayerState

data State = State
  { stateBoard   :: Board
  , statePlayers :: Map.Map PlayerId PlayerState
  , stateActions :: Map.Map Action ActionAmount
  }
  deriving (Read, Show)

stateIsFinal :: State -> Bool
stateIsFinal _ = False

decrementAction :: Action -> State -> State
decrementAction act st =
  st { stateActions = Map.adjust dec act (stateActions st) }
  where
    dec amount = amount { actionAvailable = actionAvailable amount - 1 }

summonSoldier :: PlayerId -> FLoc -> State -> State
summonSoldier pid loc st =
  st
    { stateBoard = board1
    , statePlayers = Map.adjust spendSoldier pid (statePlayers st)
    }
  where
    board = stateBoard st
    board1 = board { boardHexes = Map.adjust (addPieces [PlayerPiece pid Soldier]) loc (boardHexes board) }
