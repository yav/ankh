module App.ActionsBasic
  ( updateBoard
  , sync
  , doLog
  , doLogMultiple
  , doStartLogGroup
  , chooseRegion
  , askInputsAll
  , expandPlayers
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text

import KOI.Basics (PlayerId, WithPlayer(..))
import App.KOI
import App.State (State(..), Merged(..), playerStateId)
import qualified App.State as State
import App.Board (Board(..), RegionId, hexRegion)
import App.Input (Input(..), normalizeInput)
import App.PlayerState (PlayerState(..))
import App.LogItem (LogItem(..), LogWord(..))

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
        [ (ChooseHex loc, "Region " <> Text.pack (show rid))
        | (loc, rid) <- hexChoices
        ]
    case choice of
      ChooseHex loc ->
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
      AskBid bid _   -> AskBid bid [ b | AskBid b _ <- teammates ]
      ChooseCard c _ -> ChooseCard c (or [ c == c' | ChooseCard c' _ <- teammates ])
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
