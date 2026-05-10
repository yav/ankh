module App.Action where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Control.Monad (unless)
import Data.List (sortBy)
import Data.Ord (comparing)

import KOI.Basics (PlayerId(..))
import App.ActionType (Action(..), ActionAmount(..), actionLabel, isTestAction)
import App.KOI
import App.State (State(..), Merged(..), decrementAction, gainFollowers, summonSoldier, playerStateId)
import App.LogItem (LogWord(..))
import App.Piece (Piece(..), PlayerPieceType(..), StructureType(..), belongsTo, isNeutral, isEnemyOf)
import App.Conflict (doRegionConflict, scoreRegionMajority)
import App.Board
  ( Board(..)
  , Hex(..)

  , claimMonument
  , computeFollowersGain
  , movePiece
  , playerPieceLocations
  , validMoveTargets
  , validSummonTargets
  )
import App.Input (Input(..))
import App.PlayerState (PlayerState(..), adjustBuildLimit)
import App.ActionsBasic
import App.SplitSelection qualified as SplitSelection
import Coord (allDirections, flocAdvance, flocEdge)


doAction :: PlayerId -> Int -> Int -> Interact ()
doAction pid current total =
  do
    st <- getState
    let availableActions =
          [ (act, amount)
          | (act, amount) <- Map.toList (stateActions st)
          , isTestAction act || actionAvailable amount > 0
          ]
        label = "Choose an action (" <> Text.pack (show current)
             <> "/" <> Text.pack (show total) <> ")"
    case availableActions of
      [] -> pure ()
      _ ->
        do
          doStartLogGroup
          choice <-
            choose pid (questionFor pid label)
              [ (ChooseAction act, actionHelp st pid act)
              | (act, _) <- availableActions
              ]
          case choice of
            ChooseAction act ->
              do
                unless (isTestAction act) do
                  localUpdate_ (decrementAction act)
                runAction pid act
            _ -> pure ()

runAction :: PlayerId -> Action -> Interact ()
runAction pid act =
  case act of
    MoveFigures -> doMove pid
    SummonFigure -> doSummon pid
    GainFollowers -> doGainFollowers pid
    GainPower -> pure ()
    TestSplitRegion -> SplitSelection.doSpliltRegion pid
    TestBid -> doTestBid
    TestPlayCards -> doTestPlayCards
    TestMonumentMajority -> doTestMonumentMajority pid
    TestClaimMonument -> doClaimMonument pid
    TestConflict -> doTestConflict pid

actionHelp :: State -> PlayerId -> Action -> Text
actionHelp st pid act =
  case act of
    GainFollowers ->
      actionLabel act <> " (+" <> Text.pack (show gainedFollowers) <> ")"
    _ -> actionLabel act
  where
  gainedFollowers = computeFollowersGain (stateBoard st) pid


doMove :: PlayerId -> Interact ()
doMove pid =
  do
    st <- getState
    let lid = playerStateId st pid
    loop lid (Set.fromList (playerPieceLocations lid (stateBoard st)))
    doLog [LogPlayer pid, LogText "moved figures"]
  where
  loop _ available | Set.null available = pure ()
  loop lid available =
    do
      board <- getsState stateBoard
      let pieceChoices = [ (ChoosePiece loc False, "Move piece at " <> Text.pack (show loc))
                         | loc <- Set.toList available ]
          stopChoice = (TextQuestion "End Moving" False, "I am done moving pieces")

      choice <- choose pid (questionFor pid "Select a piece to move") (stopChoice : pieceChoices)
      case choice of
        ChoosePiece loc _ ->
          do
            let targets = validMoveTargets board loc
            if null targets
              then loop lid (Set.delete loc available)
              else
                do
                  targetChoice <-
                    choose pid (questionFor pid "Select destination")
                      [ (ChooseHex t False, Text.pack (show t)) | t <- targets ]
                  case targetChoice of
                    ChooseHex to _ ->
                      do
                        updateBoard (movePiece lid loc to)
                        loop lid (Set.delete loc available)
                    _ -> loop lid available
        _ -> pure ()

doSummon :: PlayerId -> Interact ()
doSummon pid =
  do
    st <- getState
    let lid = playerStateId st pid
    case Map.lookup lid (statePlayers st) of
      Just playerState
        | playerSoldiers playerState > 0 ->
            let board = stateBoard st
                targets = validSummonTargets lid board
            in
              case targets of
                [] -> pure ()
                _ ->
                  do
                    choice <-
                      choose pid (questionFor pid "Select a hex for summoning")
                        [ (ChooseHex loc False, Text.pack (show loc)) | loc <- targets ]
                    case choice of
                      ChooseHex loc _ ->
                        do
                          st' <- getState
                          update (summonSoldier lid loc st')
                          doLog [LogPlayer pid, LogText "summoned a soldier"]
                      _ -> pure ()
      _ -> pure ()

doGainFollowers :: PlayerId -> Interact ()
doGainFollowers pid =
  do
    st <- getState
    let lid = playerStateId st pid
    let amount = computeFollowersGain (stateBoard st) lid
    update (gainFollowers lid amount st)
    doLog [LogPlayer pid, LogText "gained", LogFollowers amount, LogText "(followers action)"]


doTestBid :: Interact ()
doTestBid =
  do
    st <- getState
    _ <- placeBids (Map.keys (statePlayers st))
    pure ()

doTestPlayCards :: Interact ()
doTestPlayCards =
  do
    st <- getState
    _ <- playCards (Map.keys (statePlayers st))
    pure ()

doTestMonumentMajority :: PlayerId -> Interact ()
doTestMonumentMajority pid =
  do
    rid <- chooseRegion pid "Select a region for monument majority"
    scoreRegionMajority rid

doTestConflict :: PlayerId -> Interact ()
doTestConflict pid =
  do
    rid <- chooseRegion pid "Select a region for conflict"
    doRegionConflict (Just pid) rid

doClaimMonument :: PlayerId -> Interact ()
doClaimMonument pid =
  do
    st <- getState
    let lid = playerStateId st pid
        board = stateBoard st
        hasMarkers =
          case Map.lookup lid (statePlayers st) of
            Just ps -> playerBuildLimit ps > 0
            Nothing -> False

        (hasNeutral, neutralAdj, enemyAdj) =
          foldl' (classify lid board) (False, [], [])
                                      (Map.toList (boardHexes board))

        targets
          | not hasMarkers = []
          | hasNeutral = neutralAdj
          | otherwise  = enemyAdj

    case targets of
      [] -> pure ()
      [loc] -> claimAt lid loc
      _  ->
        do
          players <- expandPlayers [lid]
          let locChoices =
                [ (p, [ (ChooseHex loc False, Text.pack (show loc))
                      | loc <- targets
                      ])
                | (p, _) <- players
                ]
          responses <- askInputsAll
            (\_ notResponded ->
              questionForAll notResponded "Select a monument to claim")
            locChoices
          case Map.lookup lid responses of
            Just (ChooseHex loc _) -> claimAt lid loc
            _ -> pure ()
  where
  claimAt lid loc =
    do
      st' <- getState
      let board1 = stateBoard st'
          (prevOwner, board') = claimMonument lid loc board1
          adjustPlayers =
            Map.adjust (adjustBuildLimit (-1)) lid
            . case prevOwner of
                Just prev -> Map.adjust (adjustBuildLimit 1) prev
                Nothing   -> id
      update st'
        { stateBoard = board'
        , statePlayers = adjustPlayers (statePlayers st')
        }
      doLog [LogPlayer pid, LogText "claimed a monument"]

  classify lid board (!neutral, nAdj, eAdj) (loc, hex) =
    let ps         = hexPieces hex
        isNeutMon = any isNeutral ps
        isEnemMon = any (isEnemyOf lid) ps
        adjToUs   = any (hasOurFigure lid board loc) allDirections
    in ( neutral || isNeutMon
       , if isNeutMon && adjToUs then loc : nAdj else nAdj
       , if isEnemMon && adjToUs then loc : eAdj else eAdj
       )

  hasOurFigure lid board loc dir =
    let neighbor = flocAdvance loc dir 1
    in  not (Map.member (flocEdge loc dir) (boardEdges board))
        && case Map.lookup neighbor (boardHexes board) of
             Just hex -> any (belongsTo lid) (hexPieces hex)
             Nothing  -> False


doMerge :: Interact ()
doMerge =
  do
    st <- getState
    let sorted = sortBy (comparing (playerDevotion . snd))
                        (Map.toList (statePlayers st))
    case sorted of
      (follow, followSt) : (lead, leadSt) : _ ->
        do
          let board          = stateBoard st
              (board', returned) = mergePieces follow lead board
              newLeadSt      = leadSt
                { playerDevotion    = playerDevotion followSt
                , playerFollowers = playerFollowers leadSt
                                  + playerFollowers followSt
                , playerActions   = 1
                }
              (fp, ft)       = playerDevotion followSt
              adjustOther ps
                | (px, py) <- playerDevotion ps, px == fp, py > ft =
                    ps { playerDevotion = (px, py - 1) }
                | otherwise = ps
              newStructures  = Map.unionWith (+) (stateStructures st) returned
              newPlayers     = Map.delete follow
                             $ Map.insert lead newLeadSt
                             $ Map.map adjustOther (statePlayers st)
          update st
            { stateBoard      = board'
            , statePlayers    = newPlayers
            , stateStructures = newStructures
            , playerMerged    = Just Merged
                { playerLead   = lead
                , playerFollow = follow
                }
            }
      _ -> pure ()

mergePieces :: PlayerId -> PlayerId -> Board -> (Board, Map StructureType Int)
mergePieces follow lead board =
  ( board { boardHexes = newHexes }
  , returned
  )
  where
  (returned, newHexes) =
    Map.mapAccum processHex Map.empty (boardHexes board)

  processHex acc hex =
    let (structs, pieces') = foldl' step (acc, []) (hexPieces hex)
    in (structs, hex { hexPieces = reverse pieces' })

  step (structs, acc) piece =
    case piece of
      Structure (Just p) stype | p == follow ->
        (Map.insertWith (+) stype 1 structs, acc)
      PlayerPiece p God     | p == follow -> (structs, acc)
      PlayerPiece p Soldier | p == follow -> (structs, acc)
      PlayerPiece p gt@(Guardian _) | p == follow ->
        (structs, PlayerPiece lead gt : acc)
      _ -> (structs, piece : acc)
