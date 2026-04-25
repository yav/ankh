module App.Board where

import Data.String (fromString)
import Data.Text (Text)
import Data.Vector qualified as V
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.List (foldl', partition)
import Data.Maybe (mapMaybe)
import Data.Aeson qualified as JS
import Data.Aeson ((.:))
import Data.Aeson.Types (Parser)


import KOI.Basics (PlayerId)
import Coord (FLoc(..), ELoc(..), findRegions, locationsUpTo)
import Coord qualified
import App.Piece (Piece(..), PlayerPieceType(..), StructureType(..), parsePiece, pieceOwner)



-- | The game board, represented as a map from positions to hexagons,
-- a map from region IDs to the set of positions in that region,
-- and a map of special edges (borders) on the map.
-- Note: Regions only contain non-water hexagons. Water hexagons are not
-- included in any region.
data Board = Board
  { boardHexes   :: !(Map FLoc Hex)
  , boardRegions :: !(Map RegionId (Set FLoc))
  , boardEdges   :: !(Map ELoc EdgeType)
  , boardNextRegionId :: !RegionId
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
      Board hexes regions edges _nextRegionId -> JS.object
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
parseBoard playerMap = JS.withObject "Board" $ \obj ->
  do
    hexagons <- obj .: "hexagons"
    hexList <- mapM (parseHexagon playerMap) hexagons
    let hexes = Map.fromList [ (pos, hex) | (pos, hex, _mbTag) <- hexList ]
        regionTagsByHex = Map.fromList [ (pos, tag) | (pos, _hex, Just tag) <- hexList ]
    edgesJson <- obj .: "edges"
    edges <- parseEdges edgesJson
    let computedRegions = computeRegions hexes edges
    regions <- assignRegionIdsFromTags computedRegions regionTagsByHex
    let nextRegionId =
          case Map.lookupMax regions of
            Just (rid, _) -> rid + 1
            Nothing -> 0
    pure (Board hexes regions edges nextRegionId)

assignRegionIdsFromTags :: Map RegionId (Set FLoc) -> Map FLoc RegionId -> Parser (Map RegionId (Set FLoc))
assignRegionIdsFromTags computedRegions regionTagsByHex =
  do
    taggedRegions <- mapM attachTag (Map.toList computedRegions)
    let regions = Map.fromList taggedRegions
        uniqueTagCount = Set.size (Set.fromList (map fst taggedRegions))
    if uniqueTagCount /= length taggedRegions
      then fail "Duplicate region tag values found across distinct computed regions"
      else pure regions
  where
    attachTag (computedRegionId, regionHexes) =
      case mapMaybe (`Map.lookup` regionTagsByHex) (Set.toList regionHexes) of
        [] -> fail ("Computed region " ++ show computedRegionId ++ " has no region tag")
        [tag] -> pure (tag, regionHexes)
        tags -> fail
          ("Computed region " ++ show computedRegionId
          ++ " has multiple region tags: " ++ show (Set.toList (Set.fromList tags)))

-- | Parse a single item from JSON.
-- Returns either a regular piece or a region tag number.
parseItem :: Map Int PlayerId -> JS.Value -> Parser (Maybe Piece, Maybe RegionId)
parseItem playerMap = JS.withObject "Item" $ \obj ->
  do
    kind <- obj .: "kind"
    case (kind :: Text) of
      "region" ->
        do
          regionNumber <- obj .: "number"
          if regionNumber > 0
            then pure (Nothing, Just regionNumber)
            else fail "Region tag number must be a positive integer"
      _ ->
        do
          piece <- parsePiece (parsePlayerIdFromInt playerMap) (JS.Object obj)
          pure (Just piece, Nothing)

parseHexRegionTag :: FLoc -> [RegionId] -> Parser (Maybe RegionId)
parseHexRegionTag pos regionTags =
  case regionTags of
    [] -> pure Nothing
    [tag] -> pure (Just tag)
    _ -> fail ("Hex at " ++ show pos ++ " contains multiple region tags")

-- | Parse a single hexagon from JSON.
-- Region tags are stored separately from regular pieces.
parseHexagon :: Map Int PlayerId -> JS.Value -> Parser (FLoc, Hex, Maybe RegionId)
parseHexagon playerMap = JS.withObject "Hexagon" $ \obj ->
  do
    location <- obj .: "location"
    pos <- JS.parseJSON location
    terrainText <- obj .: "terrain"
    terrain <- parseTerrain terrainText
    itemsJson <- obj .: "items"
    parsedItems <- mapM (parseItem playerMap) itemsJson
    let pieces = [ piece | (Just piece, _mbTag) <- parsedItems ]
        regionTags = [ tag | (_mbPiece, Just tag) <- parsedItems ]

    mbRegionTag <- parseHexRegionTag pos regionTags

    pure (pos, Hex terrain pieces, mbRegionTag)

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
parseEdges = JS.withArray "Edges" $ \arr ->
  do
    edgeList <- mapM parseEdge (V.toList arr)
    pure (Map.fromList edgeList)

-- | Parse a single edge from JSON
parseEdge :: JS.Value -> Parser (ELoc, EdgeType)
parseEdge = JS.withObject "Edge" $ \obj ->
  do
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
computeRegions hexes edges = findRegions nonWaterPositions barriers
  where
    allPositions = Map.keysSet hexes
    nonWaterPositions = Set.filter nonWater allPositions
    nonWater pos =
      case Map.lookup pos hexes of
        Just (Hex terrain _) -> terrain /= Water
        Nothing -> False

    barriers = Set.union naturalBarriers (Map.keysSet edges)
  
    naturalBarriers = Set.fromList
      [ edge
      | pos <- Set.toList nonWaterPositions
      , dir <- Coord.allDirections
      , let edge = Coord.flocEdge pos dir
      , let neighbor = Coord.flocAdvance pos dir 1
      , not (Set.member neighbor nonWaterPositions)
      ]

-- | Count the number of soldiers on the board for each player
countSoldiersOnBoard :: Board -> Map PlayerId Int
countSoldiersOnBoard board =
  Map.fromListWith (+)
    [ (playerId, 1)
    | hex <- Map.elems (boardHexes board)
    , PlayerPiece playerId Soldier <- hexPieces hex
    ]

-- | Count the number of each structure type on the board
countStructuresOnBoard :: Board -> Map StructureType Int
countStructuresOnBoard board =
  Map.fromListWith (+)
    [ (stype, 1)
    | hex <- Map.elems (boardHexes board)
    , Structure _ stype <- hexPieces hex
    ]

-- | Count structures owned by each player on the board
countPlayerStructures :: Board -> Map PlayerId Int
countPlayerStructures board =
  Map.fromListWith (+)
    [ (pid, 1)
    | hex <- Map.elems (boardHexes board)
    , Structure (Just pid) _ <- hexPieces hex
    ]

-- | Find all hexagon locations that contain at least one player piece belonging to the given player.
playerPieceLocations :: PlayerId -> Board -> [FLoc]
playerPieceLocations pid board =
  [ loc
  | (loc, hex) <- Map.toList (boardHexes board)
  , any isPlayerPiece (hexPieces hex)
  ]
  where
  isPlayerPiece (PlayerPiece p' _) = pid == p'
  isPlayerPiece _                  = False

-- | True when two hex locations are adjacent on the grid and are not
-- separated by an explicit border edge.
adjacentHexes :: Board -> FLoc -> FLoc -> Bool
adjacentHexes board from to
  | from == to = False
  | not (Map.member from hexes && Map.member to hexes) = False
  | otherwise =
      case matchingDirections of
        [] -> False
        (dir : _) -> not (Map.member (Coord.flocEdge from dir) edges)
  where
  hexes = boardHexes board
  edges = boardEdges board
  matchingDirections =
    [ dir
    | dir <- Coord.allDirections
    , Coord.flocAdvance from dir 1 == to
    ]

adjacentLocations :: Board -> [FLoc] -> Set FLoc
adjacentLocations board froms =
  Set.fromList
    [ to
    | from <- froms
    , dir <- Coord.allDirections
    , let to = Coord.flocAdvance from dir 1
    , adjacentHexes board from to
    ]

computeFollowersGain :: Board -> PlayerId -> Int
computeFollowersGain board pid =
  sum
    [ foldl' countIfMatching 0 (hexPieces hex)
    | loc <- Set.toList followerLocations
    , Just hex <- [Map.lookup loc (boardHexes board)]
    ]
  where
  followerLocations = adjacentLocations board (playerPieceLocations pid board)
  countIfMatching acc piece
    | countsForPlayer piece = acc + 1
    | otherwise = acc
  countsForPlayer piece =
    case piece of
      Structure mbOwner _ -> mbOwner == Nothing || mbOwner == Just pid
      _ -> False

-- | Move all pieces of the given player from one location to another.
movePiece :: PlayerId -> FLoc -> FLoc -> Board -> Board
movePiece pid from to board
  | from == to  = board
  | otherwise   = board { boardHexes = hexes2 }
  where
  (toMove, hexes1)  = Map.alterF extract from (boardHexes board)
  hexes2            = Map.adjust (addPieces toMove) to hexes1
    
  extract mb =
    case mb of
      Nothing -> ([], Nothing)
      Just h ->
        let (move, stay) = partition ourPiece (hexPieces h)
        in (move, Just h { hexPieces = stay })

  ourPiece piece =
    case piece of
      PlayerPiece p' _  -> pid == p'
      _                 -> False

addPieces :: [Piece] -> Hex -> Hex
addPieces ps h = h { hexPieces = ps ++ hexPieces h }

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

    candidateLocations = adjacentLocations board occupiedLocations

    isPlayerPieceOrStructure owner piece = pieceOwner piece == Just owner


-- Find the edges that separate the two regions.
-- An edge is a separator, if it is between two hexagons `x` and `y`,
-- and `x` is in the first region, and `y` is in the second.
subregionEdges :: Set FLoc -> Set FLoc -> Set ELoc
subregionEdges wholeRegion subRegion =
  Set.fromList
    [ Coord.flocEdge x dir
    | x <- Set.toList subRegion
    , dir <- Coord.allDirections
    , let y = Coord.flocAdvance x dir 1
    , Set.member y restOfWholeRegion
    ]
  where
    restOfWholeRegion = Set.difference wholeRegion subRegion

-- | Get all pieces from all hexes in a region.
regionPieces :: Board -> RegionId -> [Piece]
regionPieces board rid =
  case Map.lookup rid (boardRegions board) of
    Nothing -> []
    Just locs ->
      [ piece
      | loc <- Set.toList locs
      , Just hex <- [Map.lookup loc (boardHexes board)]
      , piece <- hexPieces hex
      ]

-- | Returns the region a hex belongs to.
hexRegion :: Board -> FLoc -> Maybe RegionId
hexRegion board loc =
  case [ rid | (rid, locs) <- Map.toList (boardRegions board), Set.member loc locs ] of
    (rid : _) -> Just rid
    []        -> Nothing

-- | Claim the first structure at a location, setting its owner to the given
-- player.  Returns the previous owner (if any) and the updated board.
claimMonument :: PlayerId -> FLoc -> Board -> (Maybe PlayerId, Board)
claimMonument newOwner loc board =
  (prevOwner, board { boardHexes = Map.adjust claimInHex loc (boardHexes board) })
  where
    prevOwner =
      case Map.lookup loc (boardHexes board) of
        Just hex -> findStructureOwner (hexPieces hex)
        Nothing  -> Nothing
    findStructureOwner [] = Nothing
    findStructureOwner (Structure owner _ : _) = owner
    findStructureOwner (_ : rest) = findStructureOwner rest
    claimInHex hex = hex { hexPieces = go (hexPieces hex) }
    go [] = []
    go (Structure _ st : rest) = Structure (Just newOwner) st : rest
    go (p : rest) = p : go rest

-- Find all hexagons in a region that are on its border.
-- A hexagon is on the border when at least one neighboring hexagon
-- is not a member of the region.
borderHexagons :: Set FLoc -> Set FLoc
borderHexagons region =
  Set.filter isBorder region
  where
    isBorder loc =
      any
        (\dir ->
          let neighbor = Coord.flocAdvance loc dir 1
          in not (Set.member neighbor region)
        )
        Coord.allDirections