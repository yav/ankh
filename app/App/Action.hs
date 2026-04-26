module App.Action where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Control.Monad (unless)
import Data.List (foldl', sortBy)
import Data.Ord (comparing)

import KOI.Basics (PlayerId(..), WithPlayer(..))
import App.ActionType (Action(..), ActionAmount(..), actionLabel, isTestAction)
import App.KOI
import App.State (State(..), Merged(..), decrementAction, gainFollowers, gainPoints, loseFollowers, summonSoldier, playCardForPlayer, playerStateId)
import App.LogItem (LogWord(..))
import App.Piece (Piece(..), PlayerPieceType(..), StructureType(..), pieceOwner)
import App.Board
  ( Board(..)
  , Hex(..)
  , RegionId
  , claimMonument
  , computeFollowersGain
  , movePiece
  , playerPieceLocations
  , regionPieces
  , validMoveTargets
  , validSummonTargets
  )
import App.Input (Input(..), normalizeInput)
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
      let pieceChoices = [ (ChoosePiece loc, "Move piece at " <> Text.pack (show loc))
                         | loc <- Set.toList available ]
          stopChoice = (TextQuestion "End Moving", "I am done moving pieces")

      choice <- choose pid (questionFor pid "Select a piece to move") (stopChoice : pieceChoices)
      case choice of
        ChoosePiece loc ->
          do
            let targets = validMoveTargets board loc
            if null targets
              then loop lid (Set.delete loc available)
              else
                do
                  targetChoice <-
                    choose pid (questionFor pid "Select destination")
                      [ (ChooseHex t, Text.pack (show t)) | t <- targets ]
                  case targetChoice of
                    ChooseHex to ->
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
                        [ (ChooseHex loc, Text.pack (show loc)) | loc <- targets ]
                    case choice of
                      ChooseHex loc ->
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
    doLog [LogPlayer pid, LogText "gained", LogFollowers amount]


allSame :: Eq a => [a] -> Bool
allSame [] = True
allSame (x:xs) = all (== x) xs

askInputsAll ::
  ([PlayerId] -> [PlayerId] -> Text) ->
  [(PlayerId, [(Input, Text)])] ->
  Interact (Map PlayerId Input)
askInputsAll mkQuestion playerOpts =
  inUndoGroup
  do
    st <- getState
    let merged = playerMerged st
        allChoices = Map.fromList playerOpts
    go merged Map.empty allChoices allChoices
  where
  go merged responses remaining allChoices
    | Map.null remaining = pure (mergedResponses merged responses)
    | otherwise =
      askInputsWith q
        [ (pid :-> annotateChoice teammateResponse choice, help,
           \response ->
             let conflictingPid =
                   case myMate of
                     Just other
                       | Just prev <- Map.lookup other responses
                       , normalizeInput response /= normalizeInput prev
                       -> Just other
                     _ -> Nothing
                 newResponses =
                   maybe id Map.delete conflictingPid
                   (Map.insert pid response responses)
                 newRemaining =
                   maybe id (\cpid acc -> Map.insert cpid (allChoices Map.! cpid) acc)
                            conflictingPid
                            (Map.delete pid remaining)
             in go merged newResponses newRemaining allChoices)
        | (pid, choices) <- Map.toList remaining
        , let myMate = mate merged pid
              teammateResponse =
                [ resp
                | Just other <- [myMate]
                , Just resp  <- [Map.lookup other responses]
                ]
        , (choice, help) <- choices
        ]
      where
      responded    = Map.keys responses
      notResponded = Map.keys remaining
      q = mkQuestion responded notResponded

  mate (Just m) pid
    | pid == playerLead m   = Just (playerFollow m)
    | pid == playerFollow m = Just (playerLead m)
  mate _ _                  = Nothing

  mergedResponses (Just m) responses = Map.delete (playerFollow m) responses
  mergedResponses Nothing responses = responses

  annotateChoice teammates choice =
    case choice of
      AskBid bid _   -> AskBid bid [ b | AskBid b _ <- teammates ]
      ChooseCard c _ -> ChooseCard c (or [ c == c' | ChooseCard c' _ <- teammates ])
      other -> other

doTestBid :: Interact ()
doTestBid =
  do
    st <- getState
    let playerChoices =
          [ (p, if maxBid == 0
                    then [(AskBid 0 [], "You have no followers")]
                    else [(AskBid maxBid [], "Bid between 0 and " <> Text.pack (show maxBid))])
          | (p, playerState) <- Map.toList (statePlayers st)
          , let maxBid = playerFollowers playerState
          ]

    teamBids <- askInputsAll
      (\responded notResponded ->
        let x = length responded
            y = x + length notResponded
        in "Bidding (" <> Text.pack (show x) <> "/" <> Text.pack (show y) <> "): How many followers do you want to bid?")
      playerChoices

    st' <- getState
    let processBid state (rpid, input) =
          case input of
            AskBid bid _ -> loseFollowers rpid bid state
            _ -> state
        allBids = Map.toList teamBids
    update (foldl' processBid st' allBids)

    let bidLogs = [ [LogPlayer rpid, LogText "bid", LogFollowers bid]
                  | (rpid, AskBid bid _) <- allBids ]
    doLogMultiple bidLogs

doTestPlayCards :: Interact ()
doTestPlayCards =
  do
    st <- getState
    let playerChoices =
          [ (p, [ (ChooseCard card False, Text.pack (show card))
                | card <- playerHand playerState
                ])
          | (p, playerState) <- Map.toList (statePlayers st)
          ]

    teamCards <- askInputsAll
      (\responded notResponded ->
        let x = length responded
            y = x + length notResponded
        in "Playing cards (" <> Text.pack (show x) <> "/" <> Text.pack (show y) <> "): Select a card to play")
      playerChoices

    st' <- getState
    let playCard state (rpid, input) =
          case input of
            ChooseCard card _ -> playCardForPlayer rpid card state
            _ -> state
        allCards = Map.toList teamCards

    update (foldl' playCard st' allCards)

    -- Log results after all cards are revealed
    let cardLogs = [ [LogPlayer rpid, LogText "played", LogCard card]
                   | (rpid, ChooseCard card _) <- allCards ]
    doLogMultiple cardLogs

doTestMonumentMajority :: PlayerId -> Interact ()
doTestMonumentMajority pid =
  do
    rid <- chooseRegion pid "Select a region for monument majority"
    scoreRegionMajority rid

-- No need to worry about teams, because followers shouldn't have
-- pieces on the board
scoreRegionMajority :: RegionId -> Interact ()
scoreRegionMajority rid =
  do
    board <- getsState stateBoard
    let pieces = regionPieces board rid

        -- Players who own at least one piece in the region
        presentPlayers =
          Set.fromList [ p | piece <- pieces, Just p <- [pieceOwner piece] ]

        -- Count owned structures per (PlayerId, StructureType)
        structCounts :: Map (PlayerId, StructureType) Int
        structCounts =
          Map.fromListWith (+)
            [ ((p, st), 1)
            | Structure (Just p) st <- pieces
            ]

        -- For each structure type, find the strict majority winner
        winnerFor :: StructureType -> Maybe PlayerId
        winnerFor stype =
          let counts =
                [ (p, n)
                | p <- Set.toList presentPlayers
                , let n = Map.findWithDefault 0 (p, stype) structCounts
                , n > 0
                ]
          in case sortBy (flip (comparing snd)) counts of
               (p1, n1) : (_, n2) : _ | n1 > n2 -> Just p1
               [(p1, _)]                         -> Just p1
               _                                 -> Nothing

        -- Sum winnings per player across all structure types
        winnings :: Map PlayerId Int
        winnings =
          Map.fromListWith (+)
            [ (p, 1)
            | stype <- [minBound .. maxBound]
            , Just p <- [winnerFor stype]
            ]

    -- Sort winners by playerPoints ascending (fewest first)
    players <- getsState statePlayers
    let sortedWinners =
          sortBy (comparing (\(p, _) -> Map.lookup p players >>= Just . playerPoints))
            (Map.toList winnings)

    -- Award points one at a time, re-reading state each iteration
    mapM_ awardWinner sortedWinners

  where
    awardWinner (p, n) =
      do
        st <- getState
        update (gainPoints p n st)
        doLog [LogPlayer p, LogText "won majority in region"
              , LogText (Text.pack (show rid) <> ",")
              , LogText "gained", LogPoints n]

doClaimMonument :: PlayerId -> Interact ()
doClaimMonument pid =
  do
    st <- getState
    let lid = playerStateId st pid
        board = stateBoard st
        hasBuildLimit =
          case Map.lookup lid (statePlayers st) of
            Just ps -> playerBuildLimit ps > 0
            Nothing -> False

        (hasNeutral, neutralAdj, enemyAdj) =
          foldl' (classify lid board) (False, [], [])
                                      (Map.toList (boardHexes board))

        targets
          | not hasBuildLimit = []
          | hasNeutral = neutralAdj
          | otherwise  = enemyAdj

    case targets of
      [] -> pure ()
      _  ->
        do
          choice <-
            choose pid (questionFor pid "Select a monument to claim")
              [ (ChooseHex loc, Text.pack (show loc)) | loc <- targets ]
          case choice of
            ChooseHex loc ->
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
            _ -> pure ()
  where
  classify lid board (!neutral, nAdj, eAdj) (loc, hex) =
    let ps       = hexPieces hex
        isNeutMon = any isNeutral ps
        isEnemMon = any (isEnemy lid) ps
        adjToUs   = any (hasOurFigure lid board loc) allDirections
    in ( neutral || isNeutMon
       , if isNeutMon && adjToUs then loc : nAdj else nAdj
       , if isEnemMon && adjToUs then loc : eAdj else eAdj
       )

  hasOurFigure lid board loc dir =
    let neighbor = flocAdvance loc dir 1
    in  not (Map.member (flocEdge loc dir) (boardEdges board))
        && case Map.lookup neighbor (boardHexes board) of
             Just hex -> any (isOurs lid) (hexPieces hex)
             Nothing  -> False

  isOurs lid (PlayerPiece p _) = lid == p
  isOurs _ _                   = False

  isNeutral (Structure Nothing _) = True
  isNeutral _                     = False

  isEnemy lid (Structure (Just p) _) = p /= lid
  isEnemy _ _                        = False

doMerge :: Interact ()
doMerge =
  do
    st <- getState
    let sorted = sortBy (comparing (playerPoints . snd))
                        (Map.toList (statePlayers st))
    case sorted of
      (follow, followSt) : (lead, leadSt) : _ ->
        do
          let board          = stateBoard st
              (board', returned) = mergePieces follow lead board
              newLeadSt      = leadSt
                { playerPoints    = playerPoints followSt
                , playerFollowers = playerFollowers leadSt
                                  + playerFollowers followSt
                , playerActions   = 1
                }
              (fp, ft)       = playerPoints followSt
              adjustOther ps
                | (px, py) <- playerPoints ps, px == fp, py > ft =
                    ps { playerPoints = (px, py - 1) }
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

mergePieces ::
  PlayerId -> PlayerId -> Board -> (Board, Map StructureType Int)
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