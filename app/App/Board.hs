module App.Board where

import App.Piece (Piece, parsePiece)
import Coord (FLoc(..), ELoc(..), findRegions)
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
import Data.Vector qualified as V
import KOI.Basics (PlayerId)

-- | The game board, represented as a map from positions to hexagons,
-- a map from region IDs to the set of positions in that region,
-- and a map of special edges (borders) on the map.
-- Note: Regions only contain non-water hexagons. Water hexagons are not
-- included in any region.
data Board = Board
  { boardHexes   :: !(Map FLoc Hex)
  , boardRegions :: !(Map RegionId (Set FLoc))
  , boardEdges   :: !(Map ELoc EdgeType)
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

-- | Edge types for borders on the map
data EdgeType
  = WaterEdge
  | CamelsEdge
  deriving (Eq, Ord, Read, Show)

-- | Region identifier
type RegionId = Int

-- JSON instances
instance JS.ToJSON EdgeType where
  toJSON x =
    case x of
      WaterEdge  -> JS.String "water"
      CamelsEdge -> JS.String "camels"

instance JS.FromJSON EdgeType where
  parseJSON = JS.withText "EdgeType" $ \t ->
    case t of
      "water"  -> pure WaterEdge
      "camels" -> pure CamelsEdge
      _ -> fail ("Unknown edge type: " ++ show t)

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
      Board hexes regions edges -> JS.object
        [ "hexes" JS..=
            [ JS.object
                [ "location" JS..= floc
                , "terrain" JS..= hexTerrain hex
                , "pieces" JS..= hexPieces hex
                ]
            | (floc, hex) <- Map.toList hexes
            ]
        , "regions" JS..= JS.object
            [ fromString (show regionId) JS..= Set.toList positions
            | (regionId, positions) <- Map.toList regions
            ]
        , "edges" JS..=
            [ JS.object
                [ "location" JS..= eloc
                , "type" JS..= edgeType
                ]
            | (eloc, edgeType) <- Map.toList edges
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
  edgesJson <- obj .: "edges"
  edges <- parseEdges edgesJson
  let regions = computeRegions hexes edges
  pure (Board hexes regions edges)

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

-- | Parse edges from JSON
parseEdges :: JS.Value -> Parser (Map ELoc EdgeType)
parseEdges = JS.withArray "Edges" $ \arr -> do
  edgeList <- mapM parseEdge (V.toList arr)
  pure (Map.fromList edgeList)

-- | Parse a single edge from JSON
parseEdge :: JS.Value -> Parser (ELoc, EdgeType)
parseEdge = JS.withObject "Edge" $ \obj -> do
  location <- obj .: "location"
  edgeLoc <- JS.parseJSON location
  edgeType <- obj .: "type"
  pure (edgeLoc, edgeType)

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

-- | Compute regions from the board hexes and edges
-- Finds connected regions of non-water terrain, separated by water, missing hexes, and special edges
computeRegions :: Map FLoc Hex -> Map ELoc EdgeType -> Map RegionId (Set FLoc)
computeRegions hexes edges =
  let allPositions = Map.keysSet hexes
      nonWaterPositions = Set.filter (\pos -> case Map.lookup pos hexes of
                                                Just (Hex terrain _) -> terrain /= Water
                                                Nothing -> False) allPositions
      barriers = findBarriers nonWaterPositions
      flocRegions = findRegions nonWaterPositions barriers
  in flocRegions
  where
    -- Find barrier edges (edges bordering water or missing hexes, plus special edges from the map)
    findBarriers nonWaterPositions =
      Set.union naturalBarriers specialEdges
      where
        naturalBarriers = Set.fromList
          [ edge
          | pos <- Set.toList nonWaterPositions
          , dir <- Coord.allDirections
          , let edge = Coord.flocEdge pos dir
          , let neighbor = Coord.flocAdvance pos dir 1
          , not (Set.member neighbor nonWaterPositions)
          ]
        specialEdges = Map.keysSet edges
