module App.State where

import KOI.Basics
import qualified Data.Map as Map
import App.Board
import Coord (FLoc)
import App.Piece (Piece(..), PlayerPieceType(..))
import App.PlayerState

data State = State
  { stateBoard   :: Board
  , statePlayers :: Map.Map PlayerId PlayerState
  }
  deriving (Read, Show)

stateIsFinal :: State -> Bool
stateIsFinal _ = False

summonSoldier :: PlayerId -> FLoc -> State -> State
summonSoldier pid loc st =
  st
    { stateBoard = board1
    , statePlayers = Map.adjust spendSoldier pid (statePlayers st)
    }
  where
    board = stateBoard st
    board1 = board { boardHexes = Map.adjust (addPieces [PlayerPiece pid Soldier]) loc (boardHexes board) }
