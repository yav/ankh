module Main where

import KOI.Basics(PlayerId)
import App.Action (doAction, doMerge)
import App.ActionType (initActionSelector)
import App.KOI
import App.State
import App.PlayerState
import App.Board (parseBoard, countBoardPieces, BoardCounts(..))
import App.Cards (Card(..))
import App.Powers (Power(..))
import qualified Data.Aeson as JS
import qualified Data.Aeson.Types as JS (parseEither)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
import qualified Data.Set as Set
import KOI.Options (optionString, Options, Option)
import Data.Text (pack)

-- | Command-line option for board file
boardFileOption :: Option
getBoardFile :: Options -> String
(boardFileOption, getBoardFile) =
  optionString (pack "board") "FILE" Nothing
    "JSON file containing the initial board state"

main :: IO ()
main = startApp App
  { appId = KOI
  , appOptions = [boardFileOption]
  , appColors = [ "yellow", "purple", "orange", "blue", "green" ]
  , appJS = []
  , appInitialState = \_rng opts ps -> do
      -- Create player ID mapping: player 1, 2, 3, ... in order
      let playerMap = Map.fromList (zip [1..] ps)

      -- Load board from JSON file
      let boardFile = getBoardFile opts
      boardJson <- BSL.readFile boardFile
      board <- case JS.eitherDecode boardJson of
        Left err -> error ("Failed to parse board JSON: " ++ err)
        Right jsonVal -> case JS.parseEither (parseBoard playerMap) jsonVal of
          Left err -> error ("Failed to parse board: " ++ err)
          Right b -> pure b

      let BoardCounts
            { bcSoldiersPerPlayer   = soldiersOnBoard
            , bcStructuresPerType   = structuresOnBoard
            , bcStructuresPerPlayer = playerStructures
            } = countBoardPieces board
          numPlayers = length ps

      pure State
        { stateBoard = board
        , statePlayers = Map.fromList
            [ (p, PlayerState
                { playerFollowers = 2
                , playerSoldiers = 6 - Map.findWithDefault 0 p soldiersOnBoard
                , playerBuildLimit = 9 - Map.findWithDefault 0 p playerStructures
                , playerDevotion = (0, numPlayers - 1 - i)
                , playerActions = 2
                , playerPowers = Set.fromList [Commanding, Inspiring]
                , playerHand = [BuildMonument, Chariots, CycleOfMaat, Drought]
                , playerPlayed = [PlagueOfLocusts, Flood, Miracle]
                })
            | (i, p) <- zip [0..] ps
            ]
          , stateActions = initActionSelector numPlayers
          , stateStructures =
              Map.differenceWith (\total used -> Just (total - used))
                (Map.fromList [ (stype, 10) | stype <- [minBound .. maxBound] ])
                structuresOnBoard
          , stateSplitSelection = emptySplitSelectionState
          , stateLog = []
          , playerMerged = Nothing
          , playerOrder = ps
          , remainingActions = 0
        }
  , appStart = do doMerge; gameLoop
  }

gameLoop :: Interact ()
gameLoop =
  do
    st <- getState
    case playerOrder st of
      [] -> gameLoop
      pid : rest ->
        do
          let total = case playerMerged st of
                Just m | pid == playerLead m   -> 1
                       | pid == playerFollow m -> 1
                _ -> 2
          update st { playerOrder = rest ++ [pid], remainingActions = total }
          playerTurn total pid

playerTurn :: Int -> PlayerId -> Interact ()
playerTurn total pid =
  do
    st <- getState
    if remainingActions st <= 0
      then gameLoop
      else do
        let current = total - remainingActions st + 1
        update st { remainingActions = remainingActions st - 1 }
        doAction pid current total
        playerTurn total pid
