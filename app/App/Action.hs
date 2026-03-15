module App.Action where

import Data.Aeson(ToJSON,(.=))
import Data.Aeson qualified as JS
import Data.Text qualified as T
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import KOI.Basics (PlayerId)
import Coord (FLoc, locationsUpTo)
import Coord qualified
import App.KOI
import App.State (State(..), summonSoldier)
import App.Board (Board(..), Hex(..), Terrain(..), adjacentHexes, movePiece, playerPieceLocations)
import App.Input (Input(..))
import App.Piece (pieceOwner)
import App.PlayerState (PlayerState(..))

data Action = MoveFigures | SummonFigure | GainFollowers | GainPower
    deriving (Eq,Ord,Bounded)

data ActionAmount = ActionAmount {
    actionMax :: !Int,
    actionAvailable :: !Int
}

initActionAmount :: Int -> Action -> ActionAmount
initActionAmount playerCount act =
  ActionAmount { actionMax = ma, actionAvailable = ma }
  where
  ma  = tot + playerCount - 5
  tot =
    case act of
      GainPower -> 5
      _         -> 6

instance ToJSON Action where
  toJSON act =
    case act of
      MoveFigures       -> "move"
      SummonFigure      -> "summon"
      GainFollowers     -> "follower"
      GainPower         -> "power"

instance ToJSON ActionAmount where
  toJSON x = JS.object [ "has" .= actionAvailable x, "max" .= actionMax x ]


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
        let pieceChoices = [ (ChooseHex loc, "Move piece at " <> T.pack (show loc))
                           | loc <- Set.toList available ]
            stopChoice = (TextQuestion "End Moving", "I am done moving pieces")

        choice <- choose pid "Select a piece to move" (stopChoice : pieceChoices)
        case choice of
          ChooseHex loc ->
            do
              let targets = validMoveTargets board loc
              if null targets
                then loop (Set.delete loc available)
                else
                  do
                    targetChoice <-
                      choose pid "Select destination"
                        [ (ChooseHex t, T.pack (show t)) | t <- targets ]
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
                      choose pid "Select a hex for summoning"
                        [ (ChooseHex loc, T.pack (show loc)) | loc <- targets ]
                    case choice of
                      ChooseHex loc -> update (summonSoldier pid loc st)
                      _ -> pure ()
      _ -> pure ()

-- | Find valid move targets: up to 3 distance, on board, non-water, and empty.
validMoveTargets :: Board -> FLoc -> [FLoc]
validMoveTargets board start =
  [ loc
  | loc <- Set.toList (locationsUpTo 3 start)
  , loc /= start
  , Just hex <- [Map.lookup loc (boardHexes board)]
  , null (hexPieces hex)
  , hexTerrain hex /= Water
  ]

validSummonTargets :: PlayerId -> Board -> [FLoc]
validSummonTargets pid board =
  [ loc
  | loc <- Set.toList candidateLocations
  , Just hex <- [Map.lookup loc hexes]
  , null (hexPieces hex)
  , hexTerrain hex /= Water
  ]
  where
    hexes = boardHexes board
    occupiedLocations =
      [ loc
      | (loc, hex) <- Map.toList hexes
      , any (isPlayerPieceOrStructure pid) (hexPieces hex)
      ]

    candidateLocations = Set.fromList
      [ neighbor
      | loc <- occupiedLocations
      , dir <- Coord.allDirections
      , let neighbor = Coord.flocAdvance loc dir 1
      , adjacentHexes board loc neighbor
      ]

    isPlayerPieceOrStructure owner piece = pieceOwner piece == Just owner

updateBoard :: (Board -> Board) -> Interact ()
updateBoard f = update . updateB f =<< getState
  where updateB f' s = s { stateBoard = f' (stateBoard s) }