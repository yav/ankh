module App.Action where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Set (Set)
import Data.List (foldl')

import KOI.Basics (PlayerId, WithPlayer(..))
import App.ActionType (Action(..), ActionAmount(..), actionLabel)
import App.KOI
import App.State (State(..), decrementAction, gainFollowers, loseFollowers, summonSoldier, playCardForPlayer)
import qualified App.State as State
import App.Cards (Card)
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
import App.Input (Input(..))
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
    TestBid -> doTestBid
    TestPlayCards -> doTestPlayCards pid

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
      Just (rid, selectedEdges) -> do
        updateBoard (applySelectedSplit rid selectedEdges)
        pure ()

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

{- | Ask multiple players questions and wait for all of them to respond.
Returns a map from each player to their chosen response.
The question text can vary based on who has responded and who hasn't.
Uses 'inUndoGroup' so each player can undo their response even after others respond. -}
askInputsAll ::
  ([PlayerId] -> [PlayerId] -> Text)
  {- ^ Description of what we are asking.
       First argument: players who have already responded.
       Second argument: players who haven't responded yet. -} ->
  [(PlayerId, [(Input, Text)])]
  {- ^ For each player, a list of possible choices with descriptions -} ->
  Interact (Map PlayerId Input)
askInputsAll mkQuestion playerOpts =
  inUndoGroup (go Map.empty (Map.fromList playerOpts))
  where
    go accumulated remaining =
      if Map.null remaining
        then pure accumulated
        else
          let responded = Map.keys accumulated
              notResponded = Map.keys remaining
              q = mkQuestion responded notResponded
          in askInputsWith q
            [ (pid :-> choice, help, \response -> go (Map.insert pid response accumulated) (Map.delete pid remaining))
            | (pid, choices) <- Map.toList remaining
            , (choice, help) <- choices
            ]

doTestBid :: Interact ()
doTestBid =
  do
    st <- getState
    let playerChoices =
          [ (p, if maxBid == 0
                    then [(AskBid 0, "You have no followers")]
                    else [(AskBid maxBid, "Bid between 0 and " <> Text.pack (show maxBid))])
          | (p, playerState) <- Map.toList (statePlayers st)
          , let maxBid = playerFollowers playerState
          ]

    bids <- askInputsAll
      (\responded notResponded ->
        let x = length responded
            y = x + length notResponded
        in "Bidding (" <> Text.pack (show x) <> "/" <> Text.pack (show y) <> "): How many followers do you want to bid?")
      playerChoices

    st' <- getState
    let processBid state (rpid, input) =
          case input of
            AskBid bid -> loseFollowers rpid bid state
            _ -> state
    update (foldl' processBid st' (Map.toList bids))

doTestPlayCards :: PlayerId -> Interact ()
doTestPlayCards pid =
  do
    st <- getState
    case Map.lookup pid (statePlayers st) of
      Nothing -> pure ()
      Just playerState ->
        do
          let cardChoices = [ (ChooseCard card, Text.pack (show card)) | card <- playerHand playerState ]
          mbChoice <- chooseMaybe pid (questionFor pid "Select a card to play") cardChoices
          case mbChoice of
            Just (ChooseCard card) -> update . playCardForPlayer pid card =<< getState
            _ -> pure ()