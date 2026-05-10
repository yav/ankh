module App.State where

import KOI.Basics
import Data.Aeson qualified as JS
import qualified Data.Map as Map
import qualified Data.Set as Set
import App.ActionType
import App.Board
import Coord (ELoc, FLoc)
import App.Piece (Piece(..), PlayerPieceType(..), StructureType(..), GuardianType)
import App.PlayerState
import App.Powers (Power)
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

data Merged = Merged
  { playerLead   :: PlayerId
  , playerFollow :: PlayerId
  }
  deriving (Read, Show)

data State = State
  { stateBoard       :: Board
  , statePlayers     :: Map.Map PlayerId PlayerState
  , stateActions     :: Map.Map Action ActionAmount
  , stateStructures  :: Map.Map StructureType Int
  , stateGuardians   :: Map.Map GuardianType Int  -- ^ remaining supply of the 3 guardian types for this game
  , stateSplitSelection :: SplitSelectionState
  , stateLog         :: [LogItem]
  , playerMerged     :: Maybe Merged
  , playerOrder      :: [PlayerId]
  , remainingActions :: Int
  }
  deriving (Read, Show)

playerStateId :: State -> PlayerId -> PlayerId
playerStateId st pid =
  case playerMerged st of
    Just m | pid == playerFollow m -> playerLead m
    _ -> pid

lookupPlayer :: State -> PlayerId -> Maybe PlayerState
lookupPlayer st pid = Map.lookup (playerStateId st pid) (statePlayers st)

devotion :: State -> PlayerId -> (Int, Int)
devotion st pid = maybe (0, 0) playerDevotion (lookupPlayer st pid)

hasPower :: Power -> State -> PlayerId -> Bool
hasPower pow st pid =
  maybe False (Set.member pow . playerPowers) (lookupPlayer st pid)

stateIsFinal :: State -> Bool
stateIsFinal _ = False

decrementAction :: Action -> State -> State
decrementAction act st =
  st { stateActions = Map.adjust dec act (stateActions st) }
  where
    dec amount = amount { actionAvailable = actionAvailable amount - 1 }

gainDevotion :: PlayerId -> Int -> State -> State
gainDevotion pid n st =
  st { statePlayers = newPlayers }
  where
    players = statePlayers st
    (x, y)  = maybe (0, 0) playerDevotion (Map.lookup pid players)
    x'      = x + n
    others  = Map.delete pid players
    t       = Map.size (Map.filter ((== x') . fst . playerDevotion) others)
    adjustOther ps
      | (px, py) <- playerDevotion ps, px == x, py > y =
          ps { playerDevotion = (px, py - 1) }
      | otherwise = ps
    newPlayers =
      Map.adjust (\ps -> ps { playerDevotion = (x', t) }) pid
      (Map.map adjustOther players)

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