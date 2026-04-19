module App.Action where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Set (Set)
import Data.List (foldl')

import KOI.Basics (PlayerId(..), WithPlayer(..))
import App.ActionType (Action(..), ActionAmount(..), actionLabel)
import App.KOI
import App.State (State(..), decrementAction, gainFollowers, loseFollowers, summonSoldier, playCardForPlayer)
import qualified App.State as State
import App.LogItem (LogItem(..), LogWord(..))
import App.Board
  ( Board(..)
  , EdgeType(..)
  , RegionId
  , computeFollowersGain
  , movePiece
  , playerPieceLocations
  , validMoveTargets
  , validSummonTargets
  )
import App.Input (Input(..), normalizeInput)
import App.PlayerState (PlayerState(..))
import App.SplitSelection qualified as SplitSelection
import Coord (ELoc, FLoc, findRegions)


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
          doStartLogGroup
          choice <-
            choose pid (questionFor pid "Choose an action")
              [ (ChooseAction act, actionHelp st pid act)
              | (act, _) <- availableActions
              ]
          case choice of
            ChooseAction act ->
              do
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
    TestSplitRegion -> doSpliltRegion pid
    TestBid -> doTestBid
    TestPlayCards -> doTestPlayCards

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
    board <- stateBoard <$> getState
    doLog [LogPlayer pid, LogText "moved figures"]
    loop (Set.fromList (playerPieceLocations pid board))
  where
  loop available | Set.null available = pure ()
  loop available =
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
              then loop (Set.delete loc available)
              else
                do
                  targetChoice <-
                    choose pid (questionFor pid "Select destination")
                      [ (ChooseHex t, Text.pack (show t)) | t <- targets ]
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
                        [ (ChooseHex loc, Text.pack (show loc)) | loc <- targets ]
                    case choice of
                      ChooseHex loc ->
                        do
                          update (summonSoldier pid loc st)
                          doLog [LogPlayer pid, LogText "summoned a soldier"]
                      _ -> pure ()
      _ -> pure ()

doGainFollowers :: PlayerId -> Interact ()
doGainFollowers pid =
  do
    st <- getState
    let amount = computeFollowersGain (stateBoard st) pid
    update (gainFollowers pid amount st)
    doLog [LogPlayer pid, LogText "gained", LogFollowers amount]

updateBoard :: (Board -> Board) -> Interact ()
updateBoard f = update . updateB f =<< getState
  where
  updateB f' s = s { stateBoard = f' (stateBoard s) }

sync :: Interact ()
sync = do
  st <- getState
  update st

doLog :: [LogWord] -> Interact ()
doLog ws = do
  st <- getState
  update (State.addLogEntry ws st)

doLogMultiple :: [[LogWord]] -> Interact ()
doLogMultiple wss = do
  st <- getState
  update (State.addLogEntries wss st)

doStartLogGroup :: Interact ()
doStartLogGroup = do
  st <- getState
  update (State.addLogItems [LogGroup []] st)

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
      Just (rid, selectedEdges) ->
        do
          updateBoard (applySelectedSplit rid selectedEdges)
          doLog [LogPlayer pid, LogText ("split region " <> Text.pack (show rid))]

-- | Apply a chosen split to the board by replacing the original region with the
-- two resulting subregions and adding the separating edges as camel borders.
applySelectedSplit :: RegionId -> Set ELoc -> Board -> Board
applySelectedSplit oldRegionId selectedEdges board =
  case Map.lookup oldRegionId (boardRegions board) of
    Nothing ->
      error
        ( "applySelectedSplit: selected region "
        ++ show oldRegionId
        ++ " no longer exists"
        )
    Just wholeRegion ->
      case splitRegionByEdges wholeRegion selectedEdges of
        Nothing ->
          error
            ( "applySelectedSplit: selected edges did not split region "
            ++ show oldRegionId
            ++ " into exactly two subregions: "
            ++ show (Set.toList selectedEdges)
            )
        Just (regionA, regionB) ->
          board
            { boardRegions = regions2
            , boardEdges = edges2
            , boardNextRegionId = newRegionId + 1
            }
          where
          newRegionId = boardNextRegionId board
          regions1 = Map.delete oldRegionId (boardRegions board)
          regions2 =
            Map.insert newRegionId regionB
              (Map.insert oldRegionId regionA regions1)
          edges2 =
            foldl'
              (\acc edge -> Map.insert edge CamelsEdge acc)
              (boardEdges board)
              (Set.toList selectedEdges)

splitRegionByEdges :: Set FLoc -> Set ELoc -> Maybe (Set FLoc, Set FLoc)
splitRegionByEdges wholeRegion separatorEdges =
  case Map.elems (findRegions wholeRegion separatorEdges) of
    [regionA, regionB] -> Just (regionA, regionB)
    _ -> Nothing

allSame :: Eq a => [a] -> Bool
allSame [] = True
allSame (x:xs) = all (== x) xs

{- | Ask multiple players questions and wait for all of them to respond.
Returns responses grouped first by team, then by player.
Players on the same team must reach agreement---they all need to select the same answer.
Uses 'inUndoGroup' so each player can undo their response even after others respond. -}
askInputsAll ::
  ([PlayerId] -> [PlayerId] -> Text)
  {- ^ Description of what we are asking.
       First argument: players who have already responded.
       Second argument: players who haven't responded yet. -} ->
  [(PlayerId, [(Input, Text)])]
  {- ^ For each team, selections by each player -} ->
  Interact (Map Int (Map PlayerId Input))
askInputsAll mkQuestion playerOpts =
  inUndoGroup
  do
    st <- getState
    let playerTeams = Map.fromList [ (pid, playerTeam ps)
                                   | (pid, ps) <- Map.toList (statePlayers st) ]
        allChoices = Map.fromList playerOpts
    go playerTeams Map.empty allChoices allChoices
  where
  go teams teamResponses remaining allChoices
    | Map.null remaining = pure teamResponses
    | otherwise =
      askInputsWith q
        [ (pid :-> annotateChoice myTeammateResponses choice, help,
           \response ->
             let team = Map.findWithDefault 0 pid teams
                 -- Check if this response conflicts with teammates
                 conflictingPids =
                   case Map.lookup team teamResponses of
                     Nothing -> []
                     Just teamMap ->
                       [ tpid
                       | (tpid, tresponse) <- Map.toList teamMap
                       , normalizeInput response /= normalizeInput tresponse
                       ]
                 -- Remove conflicting teammates' responses and return them to remaining
                 newTeamResponses =
                   case Map.lookup team teamResponses of
                     Nothing -> Map.insert team (Map.singleton pid response) teamResponses
                     Just teamMap ->
                       let cleanedTeamMap = foldl' (flip Map.delete) teamMap conflictingPids
                           updatedTeamMap = Map.insert pid response cleanedTeamMap
                       in Map.insert team updatedTeamMap teamResponses
                 newRemaining =
                   foldl' (\acc cpid -> Map.insert cpid (allChoices Map.! cpid) acc)
                          (Map.delete pid remaining)
                          conflictingPids
             in go teams newTeamResponses newRemaining allChoices)
        | (pid, choices) <- Map.toList remaining
        , let myTeammateResponses =
                [ resp
                | Just myTeam   <- [Map.lookup pid teams]
                , Just teamMap  <- [Map.lookup myTeam teamResponses]
                , (_, resp)     <- Map.toList teamMap
                ]
        , (choice, help) <- choices
        ]
      where
      responded    = [ pid | teamMap <- Map.elems teamResponses, pid <- Map.keys teamMap ]
      notResponded = Map.keys remaining
      q = mkQuestion responded notResponded
      
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
        allBids = [ (pid, input) | teamMap <- Map.elems teamBids, (pid, input) <- Map.toList teamMap ]
    update (foldl' processBid st' allBids)

    -- Log results after all bids are revealed
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
        allCards = [ (pid, input) | teamMap <- Map.elems teamCards, (pid, input) <- Map.toList teamMap ]

    update (foldl' playCard st' allCards)

    -- Log results after all cards are revealed
    let cardLogs = [ [LogPlayer rpid, LogText "played", LogCard card]
                   | (rpid, ChooseCard card _) <- allCards ]
    doLogMultiple cardLogs