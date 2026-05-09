module App.ActionsBasic
  ( updateBoard
  , sync
  , doLog
  , doLogMultiple
  , doStartLogGroup
  , doGainDevotion
  , chooseRegion
  , askInputsAll
  , expandPlayers
  , placeBids
  , playCards
  , doBuild
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.List (foldl')

import KOI.Basics (PlayerId, WithPlayer(..))
import App.KOI
import App.State (State(..), Merged(..), playerStateId, loseFollowers, playCardForPlayer)
import qualified App.State as State
import App.Board (Board(..), RegionId, hexRegion, addPieces)
import App.Input (Input(..), normalizeInput)
import App.PlayerState (PlayerState(..), adjustBuildLimit, spendFollowers)
import App.Piece (Piece(..), StructureType(..))
import App.LogItem (LogItem(..), LogWord(..))
import App.Powers (Power(Bountiful))
import Coord (FLoc)

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

doGainDevotion :: PlayerId -> Int -> [LogWord] -> Interact ()
doGainDevotion pid amount reason =
  do
    st <- getState
    let lid      = playerStateId st pid
        curDev   = fst (maybe (0, 0) playerDevotion (State.lookupPlayer st pid))
        isBountiful = State.hasPower Bountiful st pid && curDev < 20
        total    = amount + if isBountiful then 1 else 0
    update (State.gainDevotion lid total st)
    doLog (  [LogPlayer pid, LogText "gained", LogDevotion amount]
          ++ [LogText "(+1 bountiful)" | isBountiful]
          ++ [LogText "("] ++ reason ++ [LogText ")"]
          )

chooseRegion :: PlayerId -> Text -> Interact RegionId
chooseRegion pid prompt =
  do
    board <- getsState stateBoard
    let hexChoices =
          [ (loc, rid)
          | (rid, locs) <- Map.toList (boardRegions board)
          , loc <- Set.toList locs
          ]
    choice <-
      choose pid (questionFor pid prompt)
        [ (ChooseHex loc False, "Region " <> Text.pack (show rid))
        | (loc, rid) <- hexChoices
        ]
    case choice of
      ChooseHex loc _ ->
        do
          board' <- getsState stateBoard
          case hexRegion board' loc of
            Just rid -> pure rid
            Nothing  -> chooseRegion pid prompt
      _ -> chooseRegion pid prompt

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
      ChooseHex loc _ ->
        ChooseHex loc (or [ loc == loc' | ChooseHex loc' _ <- teammates ])
      ChooseEdge loc _ ->
        ChooseEdge loc (or [ loc == loc' | ChooseEdge loc' _ <- teammates ])
      ChoosePiece loc _ ->
        ChoosePiece loc (or [ loc == loc' | ChoosePiece loc' _ <- teammates ])
      TextQuestion t _ ->
        TextQuestion t (or [ t == t' | TextQuestion t' _ <- teammates ])
      AskBid bid _ -> AskBid bid [ b | AskBid b _ <- teammates ]
      ChooseCard c _ -> ChooseCard c (or [ c == c' | ChooseCard c' _ <- teammates ])
      ChooseMonumentType s _ ->
        ChooseMonumentType s (or [ s == s' | ChooseMonumentType s' _ <- teammates ])
      other -> other

expandPlayers :: [PlayerId] -> Interact [(PlayerId, PlayerState)]
expandPlayers pids =
  do
    st <- getState
    let merged = playerMerged st
        withFollower pid =
          case merged of
            Just m | pid == playerLead m -> [pid, playerFollow m]
            _ -> [pid]
    pure
      [ (p, playerState)
      | p <- concatMap withFollower pids
      , Just playerState <- [Map.lookup (playerStateId st p) (statePlayers st)]
      ]

placeBids :: [PlayerId] -> Interact (Map PlayerId Input)
placeBids pids =
  do
    players <- expandPlayers pids
    let playerChoices =
          [ (p, if maxBid == 0
                    then [(AskBid 0 [], "You have no followers")]
                    else [(AskBid maxBid [], "Bid between 0 and " <> Text.pack (show maxBid))])
          | (p, playerState) <- players
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
            AskBid bid _ -> loseFollowers (playerStateId st' rpid) bid state
            _ -> state
        allBids = Map.toList teamBids
    update (foldl' processBid st' allBids)

    let bidLogs = [ [LogPlayer rpid, LogText "bid", LogFollowers bid]
                  | (rpid, AskBid bid _) <- allBids ]
    doLogMultiple bidLogs

    pure teamBids

playCards :: [PlayerId] -> Interact (Map PlayerId Input)
playCards pids =
  do
    players <- expandPlayers pids
    let playerChoices =
          [ (p, [ (ChooseCard card False, Text.pack (show card))
                | card <- playerHand playerState
                ])
          | (p, playerState) <- players
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
            ChooseCard card _ -> playCardForPlayer (playerStateId st' rpid) card state
            _ -> state
        allCards = Map.toList teamCards

    update (foldl' playCard st' allCards)

    let cardLogs = [ [LogPlayer rpid, LogText "played", LogCard card]
                   | (rpid, ChooseCard card _) <- allCards ]
    doLogMultiple cardLogs

    pure teamCards

doBuild :: PlayerId -> Int -> [StructureType] -> [FLoc] -> Interact ()
doBuild pid cost availableTypes emptySpaces =
  do
    players <- expandPlayers [pid]
    let typeChoices =
          [ (p, [ (ChooseMonumentType stype False, Text.pack (show stype))
                | stype <- availableTypes
                ])
          | (p, _) <- players
          ]
    typeResponses <- askInputsAll
      (\_ _ -> questionFor pid "Choose monument type")
      typeChoices
    case Map.lookup pid typeResponses of
      Just (ChooseMonumentType stype _) ->
        do
          locChoice <-
            choose pid (questionFor pid "Place the monument")
              [ (ChooseHex loc False, Text.pack (show loc))
              | loc <- emptySpaces
              ]
          case locChoice of
            ChooseHex loc _ ->
              do
                st <- getState
                let piece = Structure (Just (playerStateId st pid)) stype
                    newBoard = (stateBoard st)
                      { boardHexes =
                          Map.adjust (addPieces [piece]) loc
                            (boardHexes (stateBoard st))
                      }
                update st
                  { stateBoard      = newBoard
                  , stateStructures =
                      Map.adjust (subtract 1) stype (stateStructures st)
                  , statePlayers    =
                      Map.adjust
                        (adjustBuildLimit (-1) . spendFollowers cost)
                        (playerStateId st pid)
                        (statePlayers st)
                  }
                doLog [ LogPlayer pid, LogText "built a"
                      , LogStructure stype ]
            _ -> pure ()
      _ -> pure ()
