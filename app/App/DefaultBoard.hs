module App.DefaultBoard (defaultBoard) where

import App.Board
import Data.Map.Strict qualified as Map

-- | Create the default board with rectangular hexagonal grid
-- 11 columns (0-10), alternating heights (10 and 9)
-- Even columns (0,2,4,6,8,10) are 10 hexagons tall (r: 0-9)
-- Odd columns (1,3,5,7,9) are 9 hexagons tall (r: 0-8)
-- The 4 corners are removed
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

-- | Create an empty hex with Desert terrain
emptyHex :: Hex
emptyHex = Hex
  { hexTerrain = Desert
  , hexPieces = []
  }

-- | Generate all positions in the rectangular grid
allPositions :: [HexPos]
allPositions =
  [ HexPos q r
  | q <- [0..10]
  , r <- [0 .. maxRow q]
  ]
  where
    -- Even columns are 10 tall (0-9), odd columns are 9 tall (0-8)
    maxRow q
      | even q    = 9
      | otherwise = 8

-- | Check if a position is one of the 4 corners
isCorner :: HexPos -> Bool
isCorner (HexPos q r) =
  (q == 0 && r == 0) ||   -- Top-left
  (q == 10 && r == 0) ||  -- Top-right
  (q == 0 && r == 9) ||   -- Bottom-left
  (q == 10 && r == 9)     -- Bottom-right
