module App.DefaultBoard (defaultBoard) where

import App.Board
import Data.Map.Strict qualified as Map

-- | Create the default board with rectangular hexagonal grid
-- Creates a proper rectangular layout when rendered (not in axial coordinates)
-- For edge_up rendering: width=10, height=11, startsWide=true
-- Generated using hex-grid's RectangularRegion with those exact parameters
-- All terrain is Desert initially
defaultBoard :: Board
defaultBoard = Board
  { boardHexes = Map.fromList
      [ (pos, emptyHex)
      | pos <- allPositions
      , not (isCorner pos)
      ]
  , boardRegions = Map.empty  -- No regions defined yet
  }

-- | All positions in the rectangular board (generated from hex-grid)
allPositions :: [HexPos]
allPositions =
  [ HexPos 0 0, HexPos 1 0, HexPos 2 0, HexPos 3 0, HexPos 4 0
  , HexPos 5 0, HexPos 6 0, HexPos 7 0, HexPos 8 0, HexPos 9 0, HexPos 10 0
  , HexPos 10 (-1), HexPos 9 (-1), HexPos 8 (-1), HexPos 7 (-1), HexPos 6 (-1)
  , HexPos 5 (-1), HexPos 4 (-1), HexPos 3 (-1), HexPos 2 (-1), HexPos 1 (-1)
  , HexPos 1 (-2), HexPos 2 (-2), HexPos 3 (-2), HexPos 4 (-2), HexPos 5 (-2)
  , HexPos 6 (-2), HexPos 7 (-2), HexPos 8 (-2), HexPos 9 (-2), HexPos 10 (-2), HexPos 11 (-2)
  , HexPos 11 (-3), HexPos 10 (-3), HexPos 9 (-3), HexPos 8 (-3), HexPos 7 (-3)
  , HexPos 6 (-3), HexPos 5 (-3), HexPos 4 (-3), HexPos 3 (-3), HexPos 2 (-3)
  , HexPos 2 (-4), HexPos 3 (-4), HexPos 4 (-4), HexPos 5 (-4), HexPos 6 (-4)
  , HexPos 7 (-4), HexPos 8 (-4), HexPos 9 (-4), HexPos 10 (-4), HexPos 11 (-4), HexPos 12 (-4)
  , HexPos 12 (-5), HexPos 11 (-5), HexPos 10 (-5), HexPos 9 (-5), HexPos 8 (-5)
  , HexPos 7 (-5), HexPos 6 (-5), HexPos 5 (-5), HexPos 4 (-5), HexPos 3 (-5)
  , HexPos 3 (-6), HexPos 4 (-6), HexPos 5 (-6), HexPos 6 (-6), HexPos 7 (-6)
  , HexPos 8 (-6), HexPos 9 (-6), HexPos 10 (-6), HexPos 11 (-6), HexPos 12 (-6), HexPos 13 (-6)
  , HexPos 13 (-7), HexPos 12 (-7), HexPos 11 (-7), HexPos 10 (-7), HexPos 9 (-7)
  , HexPos 8 (-7), HexPos 7 (-7), HexPos 6 (-7), HexPos 5 (-7), HexPos 4 (-7)
  , HexPos 4 (-8), HexPos 5 (-8), HexPos 6 (-8), HexPos 7 (-8), HexPos 8 (-8)
  , HexPos 9 (-8), HexPos 10 (-8), HexPos 11 (-8), HexPos 12 (-8), HexPos 13 (-8), HexPos 14 (-8)
  , HexPos 14 (-9), HexPos 13 (-9), HexPos 12 (-9), HexPos 11 (-9), HexPos 10 (-9)
  , HexPos 9 (-9), HexPos 8 (-9), HexPos 7 (-9), HexPos 6 (-9), HexPos 5 (-9)
  ]

-- | Check if a position is a corner that should be removed
isCorner :: HexPos -> Bool
isCorner (HexPos q r) =
  (q == 0 && r == 0) ||      -- Top-left corner
  (q == 10 && r == 0) ||     -- Top-right corner
  (q == 14 && r == (-9)) ||  -- Bottom-left corner
  (q == 5 && r == (-9))      -- Bottom-right corner

-- | Create an empty hex with Desert terrain
emptyHex :: Hex
emptyHex = Hex
  { hexTerrain = Desert
  , hexPieces = []
  }
