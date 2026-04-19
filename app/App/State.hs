module App.State where

import KOI.Basics
import Data.Aeson qualified as JS
import qualified Data.Map as Map
import qualified Data.Set as Set
import App.ActionType
import App.Board
import Coord (ELoc, FLoc)
import App.Piece (Piece(..), PlayerPieceType(..))
import App.PlayerState
import App.Cards (Card)
import App.LogItem (LogItem(..), LogWord)

data SplitSelectionState = SplitSelectionState
  { splitSelectionEdges :: Set.Set ELoc
  , splitSelectionInvalid :: Bool
  }
  deriving (Read, Show)

instance JS.ToJSON SplitSelectionState where
  toJSON splitState = JS.object
    [ "edges" JS..= Set.toList (splitSelectionEdges splitState)
    , "invalid" JS..= splitSelectionInvalid splitState
    ]

emptySplitSelectionState :: SplitSelectionState
emptySplitSelectionState =
  SplitSelectionState
    { splitSelectionEdges = Set.empty
    , splitSelectionInvalid = False
    }

data State = State
  { stateBoard   :: Board
  , statePlayers :: Map.Map PlayerId PlayerState
  , stateActions :: Map.Map Action ActionAmount
  , stateSplitSelection :: SplitSelectionState
  , stateLog     :: [LogItem]
  }
  deriving (Read, Show)

stateIsFinal :: State -> Bool
stateIsFinal _ = False

decrementAction :: Action -> State -> State
decrementAction act st =
  st { stateActions = Map.adjust dec act (stateActions st) }
  where
    dec amount = amount { actionAvailable = actionAvailable amount - 1 }

gainFollowers :: PlayerId -> Int -> State -> State
gainFollowers pid n st =
  st { statePlayers = Map.adjust (addFollowers n) pid (statePlayers st) }

loseFollowers :: PlayerId -> Int -> State -> State
loseFollowers pid n st =
  st { statePlayers = Map.adjust (spendFollowers n) pid (statePlayers st) }

playCardForPlayer :: PlayerId -> Card -> State -> State
playCardForPlayer pid card st =
  st { statePlayers = Map.adjust (playCard card) pid (statePlayers st) }

summonSoldier :: PlayerId -> FLoc -> State -> State
summonSoldier pid loc st =
  st
    { stateBoard = board1
    , statePlayers = Map.adjust spendSoldier pid (statePlayers st)
    }
  where
    board = stateBoard st
    board1 = board { boardHexes = Map.adjust (addPieces [PlayerPiece pid Soldier]) loc (boardHexes board) }

setSplitSelection :: Set.Set ELoc -> State -> State
setSplitSelection selected st =
  st { stateSplitSelection = splitState { splitSelectionEdges = selected } }
  where
    splitState = stateSplitSelection st

setSplitSelectionInvalid :: Bool -> State -> State
setSplitSelectionInvalid isInvalid st =
  st { stateSplitSelection = splitState { splitSelectionInvalid = isInvalid } }
  where
    splitState = stateSplitSelection st

clearSplitSelection :: State -> State
clearSplitSelection st = st { stateSplitSelection = emptySplitSelectionState }

addLogItem :: LogItem -> State -> State
addLogItem item st = st { stateLog = stateLog st ++ [item] }

-- Note: Log is stored in reverse order (most recent first) for efficiency
addLogItems :: [LogItem] -> State -> State
addLogItems items st = st { stateLog = reverse items ++ stateLog st }

addLogEntry :: [LogWord] -> State -> State
addLogEntry ws st =
  case stateLog st of
    LogGroup entries : rest ->
      st { stateLog = LogGroup (LogEntry ws : entries) : rest }
    _ -> st { stateLog = LogEntry ws : stateLog st }

addLogEntries :: [[LogWord]] -> State -> State
addLogEntries wss st =
  case stateLog st of
    LogGroup entries : rest -> st { stateLog = LogGroup new : rest }
      where new = ents ++ entries
      
    _ -> st { stateLog = ents ++ stateLog st }
  where ents = reverse (map LogEntry wss)