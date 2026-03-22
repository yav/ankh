module App.Action where

import Data.Text qualified as T
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Set (Set)
import Data.List (foldl')

import KOI.Basics (PlayerId)
import App.ActionType (Action(..), ActionAmount(..), actionLabel)
import App.KOI
import App.State (State(..), decrementAction, gainFollowers, summonSoldier)
import qualified App.State as State
import App.Board
  ( Board(..)
  , EdgeType(..)
  , RegionId
  , computeFollowersGain
  , movePiece
  , playerPieceLocations
  , subregionEdges
  , validMoveTargets
  , validSummonTargets
  )
import App.Input (Input(..))
import App.PlayerState (PlayerState(..))
import App.SplitSelection qualified as SplitSelection
import Coord (FLoc)


doAction :: PlayerId -> Interact ()
doAction pid =
  do
    st <- getState
    let availableActions =
          [ (act, amount)
          | (act, amount) <- Map.toList (stateActions st)
          , actionAvailable amount > 0
          ]
    case availableActions of
      [] -> pure ()
      _ ->
        do
          choice <-
            choose pid (questionFor pid "Choose an action")
              [ (ChooseAction act, actionHelp st pid act)
              | (act, _) <- availableActions
              ]
          case choice of
            ChooseAction act ->
              do
                update (decrementAction act st)
                runAction pid act
            _ -> pure ()

runAction :: PlayerId -> Action -> Interact ()
runAction pid act =
  case act of
    MoveFigures -> doMove pid
    SummonFigure -> doSummon pid
    GainFollowers -> doGainFollowers pid
    GainPower -> pure ()
    TestSplitRegion -> doSpliltRegion pid

actionHelp :: State -> PlayerId -> Action -> T.Text
actionHelp st pid act =
  case act of
    GainFollowers ->
      actionLabel act <> " (+" <> T.pack (show gainedFollowers) <> ")"
    _ -> actionLabel act
  where
    gainedFollowers = computeFollowersGain (stateBoard st) pid


doMove :: PlayerId -> Interact ()
doMove pid =
  do
    board <- stateBoard <$> getState
    loop (Set.fromList (playerPieceLocations pid board))
  where
    loop available | Set.null available = pure ()
    loop available =
      do
        board <- getsState stateBoard
        let pieceChoices = [ (ChoosePiece loc, "Move piece at " <> T.pack (show loc))
                           | loc <- Set.toList available ]
            stopChoice = (TextQuestion "End Moving", "I am done moving pieces")

        choice <- choose pid (questionFor pid "Select a piece to move") (stopChoice : pieceChoices)
        case choice of
          ChoosePiece loc ->
            do
              let targets = validMoveTargets board loc
              if null targets
                then loop (Set.delete loc available)
                else
                  do
                    targetChoice <-
                      choose pid (questionFor pid "Select destination")
                        [ (ChooseHex t, T.pack (show t)) | t <- targets ]
                    case targetChoice of
                      ChooseHex to ->
                        do
                          updateBoard (movePiece pid loc to)
                          loop (Set.delete loc available)
                      _ -> loop available
          _ -> pure ()

doSummon :: PlayerId -> Interact ()
doSummon pid =
  do
    st <- getState
    case Map.lookup pid (statePlayers st) of
      Just playerState
        | playerSoldiers playerState > 0 ->
            let board = stateBoard st
                targets = validSummonTargets pid board
            in
              case targets of
                [] -> pure ()
                _ ->
                  do
                    choice <-
                      choose pid (questionFor pid "Select a hex for summoning")
                        [ (ChooseHex loc, T.pack (show loc)) | loc <- targets ]
                    case choice of
                      ChooseHex loc -> update (summonSoldier pid loc st)
                      _ -> pure ()
      _ -> pure ()

doGainFollowers :: PlayerId -> Interact ()
doGainFollowers pid =
  do
    st <- getState
    let amount = computeFollowersGain (stateBoard st) pid
    update (gainFollowers pid amount st)

updateBoard :: (Board -> Board) -> Interact ()
updateBoard f = update . updateB f =<< getState
  where updateB f' s = s { stateBoard = f' (stateBoard s) }

doSpliltRegion :: PlayerId -> Interact ()
doSpliltRegion pid =
  do
    st0 <- getState
    update (State.clearSplitSelection st0)

    split <- SplitSelection.doSpliltRegion pid

    st1 <- getState
    update (State.clearSplitSelection st1)

    case split of
      Nothing -> pure ()
      Just (rid, selected) -> do
        updateBoard (applySelectedSplit rid selected)
        pure ()

-- | Apply a chosen split to the board by replacing the original region with the
-- remainder, creating a new region for the selected cells, and adding
-- separating edges as camel borders.
applySelectedSplit :: RegionId -> Set FLoc -> Board -> Board
applySelectedSplit oldRegionId selected board =
  case Map.lookup oldRegionId (boardRegions board) of
    Nothing -> board
    Just wholeRegion ->
      board
        { boardRegions = regions2
        , boardEdges = edges2
        , boardNextRegionId = newRegionId + 1
        }
      where
        selectedEdges = subregionEdges wholeRegion selected
        remainingRegion = Set.difference wholeRegion selected
        regions1 =
          if Set.null remainingRegion
            then Map.delete oldRegionId (boardRegions board)
            else Map.insert oldRegionId remainingRegion (boardRegions board)
        newRegionId = boardNextRegionId board
        regions2 = Map.insert newRegionId selected regions1
        edges2 =
          foldl'
            (\acc edge -> Map.insert edge CamelsEdge acc)
            (boardEdges board)
            (Set.toList selectedEdges)