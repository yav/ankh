module App.Board where

import App.Piece (Piece, parsePiece)
import Coord (FLoc(..), findRegions)
import Coord qualified
import Data.Aeson qualified as JS
import Data.Aeson ((.:))
import Data.Aeson.Types (Parser)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (fromString)
import Data.Text (Text)
import KOI.Basics (PlayerId)

-- | The game board, represented as a map from positions to hexagons
-- and a map from region IDs to the set of positions in that region.
-- Note: Regions only contain non-water hexagons. Water hexagons are not
-- included in any region.
data Board = Board
  { boardHexes   :: !(Map FLoc Hex)
  , boardRegions :: !(Map RegionId (Set FLoc))
  }
  deriving (Read, Show)

-- | A single hexagon on the board
data Hex = Hex
  { hexTerrain :: !Terrain
  , hexPieces  :: ![Piece]
  }
  deriving (Read, Show)

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
            | (FLoc q r, hex) <- Map.toList hexes
            ]
        , "regions" JS..= JS.object
            [ fromString (show regionId) JS..=
                [ JS.object ["q" JS..= q, "r" JS..= r]
                | FLoc q r <- Set.toList positions
                ]
            | (regionId, positions) <- Map.toList regions
            ]
        ]

-- Parsing functions

-- | Parse a Board from JSON using a player ID mapping
-- Player ID 0 in JSON maps to Nothing (neutral)
-- Other integers must be present in the playerMap
parseBoard :: Map Int PlayerId -> JS.Value -> Parser Board
parseBoard playerMap = JS.withObject "Board" $ \obj -> do
  hexagons <- obj .: "hexagons"
  hexList <- mapM (parseHexagon playerMap) hexagons
  let hexes = Map.fromList hexList
  let regions = computeRegions hexes
  pure (Board hexes regions)

-- | Parse a single hexagon from JSON
parseHexagon :: Map Int PlayerId -> JS.Value -> Parser (FLoc, Hex)
parseHexagon playerMap = JS.withObject "Hexagon" $ \obj -> do
  location <- obj .: "location"
  pos <- JS.parseJSON location
  terrainText <- obj .: "terrain"
  terrain <- parseTerrain terrainText
  itemsJson <- obj .: "items"
  pieces <- mapM (parsePiece (parsePlayerIdFromInt playerMap)) itemsJson
  pure (pos, Hex terrain pieces)

-- | Parse terrain from text
parseTerrain :: Text -> Parser Terrain
parseTerrain txt =
  case txt of
    "plains" -> pure Grass
    "desert" -> pure Desert
    "water" -> pure Water
    _ -> fail ("Unknown terrain type: " ++ show txt)

-- | Parse player ID from integer according to the mapping
-- 0 maps to Nothing, other integers must be in the map
parsePlayerIdFromInt :: Map Int PlayerId -> JS.Value -> Parser (Maybe PlayerId)
parsePlayerIdFromInt playerMap val =
  case val of
    JS.Null -> pure Nothing
    JS.Number n ->
      case floor n of
        0 -> pure Nothing
        playerId ->
          case Map.lookup playerId playerMap of
            Just pid -> pure (Just pid)
            Nothing -> fail ("Player ID " ++ show playerId ++ " not found in player map")
    _ -> fail "Expected null or number for player ID"

-- | Compute regions from the board hexes
-- Finds connected regions of non-water terrain
computeRegions :: Map FLoc Hex -> Map RegionId (Set FLoc)
computeRegions hexes =
  let allPositions = Map.keysSet hexes
      nonWaterPositions = Set.filter (\pos -> case Map.lookup pos hexes of
                                                Just (Hex terrain _) -> terrain /= Water
                                                Nothing -> False) allPositions
      barriers = findBarriers nonWaterPositions
      flocRegions = findRegions nonWaterPositions barriers
  in flocRegions
  where
    -- Find barrier edges (edges bordering water or missing hexes)
    findBarriers nonWaterPositions =
      Set.fromList
        [ edge
        | pos <- Set.toList nonWaterPositions
        , dir <- Coord.allDirections
        , let edge = Coord.flocEdge pos dir
        , let neighbor = Coord.flocAdvance pos dir 1
        , not (Set.member neighbor nonWaterPositions)
        ]
