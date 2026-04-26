module App.ActionsBasic
  ( updateBoard
  , sync
  , doLog
  , doLogMultiple
  , doStartLogGroup
  , chooseRegion
  ) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text

import KOI.Basics (PlayerId)
import App.KOI
import App.State (State(..))
import qualified App.State as State
import App.Board (Board(..), RegionId, hexRegion)
import App.Input (Input(..))
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
