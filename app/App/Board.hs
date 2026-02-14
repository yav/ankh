module App.Board where

import App.Piece (Piece)
import Data.Aeson qualified as JS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (fromString)

-- | The game board, represented as a map from positions to hexagons
-- and a map from region IDs to the set of positions in that region
data Board = Board
  { boardHexes   :: !(Map HexPos Hex)
  , boardRegions :: !(Map RegionId (Set HexPos))
  }
  deriving (Read, Show)

-- | A single hexagon on the board
data Hex = Hex
  { hexTerrain :: !Terrain
  , hexPieces  :: ![Piece]
  }
  deriving (Read, Show)

-- | Hexagonal position using axial coordinates (q, r)
-- This is a common coordinate system for hex grids
data HexPos = HexPos
  { hexQ :: !Int  -- column coordinate
  , hexR :: !Int  -- row coordinate
  }
  deriving (Eq, Ord, Read, Show)

-- | Terrain types for hexagons
data Terrain
  = Desert
  | Grass
  | Water
  deriving (Eq, Ord, Read, Show)

-- | Region identifier
type RegionId = Int

-- JSON instances
instance JS.ToJSON Terrain where
  toJSON x =
    case x of
      Desert -> JS.String "desert"
      Grass  -> JS.String "grass"
      Water  -> JS.String "water"

instance JS.ToJSON HexPos where
  toJSON x =
    case x of
      HexPos q r -> JS.object
        [ "q" JS..= q
        , "r" JS..= r
        ]

instance JS.ToJSON Hex where
  toJSON x =
    case x of
      Hex terrain pieces -> JS.object
        [ "terrain" JS..= terrain
        , "pieces"  JS..= pieces
        ]

instance JS.ToJSON Board where
  toJSON x =
    case x of
      Board hexes regions -> JS.object
        [ "hexes" JS..= JS.object
            [ fromString (show q ++ "," ++ show r) JS..= hex
            | (HexPos q r, hex) <- Map.toList hexes
            ]
        , "regions" JS..= JS.object
            [ fromString (show regionId) JS..=
                [ JS.object ["q" JS..= q, "r" JS..= r]
                | HexPos q r <- Set.toList positions
                ]
            | (regionId, positions) <- Map.toList regions
            ]
        ]
