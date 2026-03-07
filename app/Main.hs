module Main where

import App.KOI
import App.State
import App.PlayerState
import App.Input
import App.Board (parseBoard, countSoldiersOnBoard)
import qualified Data.Aeson as JS
import qualified Data.Aeson.Types as JS (parseEither)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
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

      -- Count soldiers on board for each player
      let soldiersOnBoard = countSoldiersOnBoard board

      pure State
        { stateBoard = board
        , statePlayers = Map.fromList
            [ (p, PlayerState
                { playerFollowers = 2
                , playerSoldiers = 6 - Map.findWithDefault 0 p soldiersOnBoard
                , playerPoints = 0
                , playerActions = 2
                })
            | p <- ps
            ]
        }
  , appStart = gameLoop
  }

gameLoop :: Interact ()
gameLoop =
  do s <- getState
     -- Ask each player a trivial question to yield control
     mapM_ askPlayer (Map.keys (statePlayers s))
     update s
     gameLoop
  where
    askPlayer p = choose p "What would you like to do?" [(Placeholder, "Continue")]
