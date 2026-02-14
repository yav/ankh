module Main where

import App.KOI
import App.State
import App.Board
import App.PlayerState
import App.Input
import qualified Data.Map as Map

main :: IO ()
main = startApp App
  { appId = KOI
  , appOptions = []
  , appColors = [ "red", "green", "blue", "yellow" ]
  , appJS = []
  , appInitialState = \_rng _opts ps ->
      pure State
        { stateBoard = Board
        , statePlayers = Map.fromList [ (p, PlayerState) | p <- ps ]
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
