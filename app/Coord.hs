module Coord where

import qualified Data.Vector as V
import Data.Vector (Vector)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)

-- | Neighbor offset table for hexagonal grid
neighborTable :: Vector (Int, Int)
neighborTable = V.fromList [(1, 0), (0, 1), (-1, 1), (-1, 0), (0, -1), (1, -1)]

-- | Represents the orientation of a hexagonal grid.
data Orientation
  = VertexUp  -- ^ Hexagons have a vertex at the top
  | EdgeUp    -- ^ Hexagons have a flat edge at the top
  deriving (Show, Eq, Ord)

-- | Returns the opposite orientation of the given one.
otherOrientation :: Orientation -> Orientation
otherOrientation o =
  case o of
    VertexUp -> EdgeUp
    EdgeUp -> VertexUp

-- | Represents one of the 6 directions in a hexagonal grid.
-- Directions are indexed 0 to 5, typically starting from the right and moving clockwise.
newtype Dir = Dir { dirNumber :: Int }
  deriving (Eq, Ord)

-- | Creates a new direction.
-- The direction index will be normalized to the range [0, 5].
mkDir :: Int -> Dir
mkDir n = Dir (n `mod` 6)

-- | Returns a new direction rotated clockwise by n steps.
dirClockwise :: Dir -> Int -> Dir
dirClockwise (Dir n) steps = mkDir (n + steps)

-- | Returns a new direction rotated counter-clockwise by n steps.
dirCounterClockwise :: Dir -> Int -> Dir
dirCounterClockwise (Dir n) steps = mkDir (n - steps)

-- | Calculates a unit vector for this direction based on orientation and an initial angle offset.
-- Internal helper used by 'edgeUnit' and 'vertexUnit'.
relativeUnit :: Dir -> Orientation -> Double -> (Double, Double)
relativeUnit (Dir n) o th0 = (cos th, sin th)
  where
    th' = pi * fromIntegral n / 3 - th0
    th = if o == EdgeUp then th' + pi / 2 else th'

-- | Returns a unit vector pointing towards the center of an edge in this direction.
edgeUnit :: Dir -> Orientation -> (Double, Double)
edgeUnit d o = relativeUnit d o 0

-- | Returns a unit vector pointing towards a vertex in this direction.
vertexUnit :: Dir -> Orientation -> (Double, Double)
vertexUnit d o = relativeUnit d o (pi / 6)

instance Show Dir where
  show (Dir n) = "Dir(" ++ show n ++ ")"

-- | List of all 6 possible directions.
allDirections :: [Dir]
allDirections = map Dir [0, 1, 2, 3, 4, 5]

-- | Represents the location of a face (hexagon) in the grid using axial coordinates.
data FLoc = FLoc
  { flocX :: Int  -- ^ Axial x-coordinate
  , flocY :: Int  -- ^ Axial y-coordinate
  }
  deriving (Eq, Ord)

-- | Returns a new location by moving n steps in the given direction.
flocAdvance :: FLoc -> Dir -> Int -> FLoc
flocAdvance (FLoc x y) (Dir dn) n =
  case neighborTable V.! dn of
    (dx, dy) -> FLoc (x + n * dx) (y + n * dy)

-- | Returns the location of the edge in the given direction.
flocEdge :: FLoc -> Dir -> ELoc
flocEdge face dir = mkELoc face dir

-- | Returns a directed edge location.
flocDirectedEdge ::
  FLoc {-^ The face location -} ->
  Dir {-^ Direction specifying which edge relative to this face -} ->
  Bool {-^ Whether the edge direction should be clockwise around this face -} ->
  DELoc
flocDirectedEdge face d clockwise = DELoc edge reversed
  where
    edge = flocEdge face d
    reversed = if clockwise then dirNumber d > 2 else dirNumber d < 3

-- | Returns the location of the vertex in the given direction.
flocVertex :: FLoc -> Dir -> VLoc
flocVertex face dir = mkVLoc face dir

-- | Returns all 6 edges of this face.
flocEdges :: FLoc -> [ELoc]
flocEdges face = map (flocEdge face) allDirections

-- | Returns all 6 vertices of this face.
flocVertices :: FLoc -> [VLoc]
flocVertices face = map (flocVertex face) allDirections

-- | Calculates the distance from this face to another face.
-- Uses the cube coordinate distance formula.
flocDistance :: FLoc -> FLoc -> Int
flocDistance (FLoc x1 y1) (FLoc x2 y2) = (abs dx + abs dy + abs dz) `div` 2
  where
    dx = x2 - x1
    dy = y2 - y1
    dz = -dx - dy

instance Show FLoc where
  show (FLoc x y) = "FLoc(" ++ show x ++ "," ++ show y ++ ")"

-- | Represents the location of an edge between two faces.
data ELoc = ELoc
  { elocFaceLoc :: FLoc    -- ^ One of the faces touching this edge
  , elocNumber :: Int      -- ^ The edge index (0, 1, or 2) relative to face_loc
  }
  deriving (Eq, Ord)

-- | Internal constructor for 'ELoc'. Use 'flocEdge' to create instances.
mkELoc :: FLoc -> Dir -> ELoc
mkELoc face (Dir dn) =
  if dn >= 3
    then ELoc (flocAdvance face (Dir dn) 1) (dn - 3)
    else ELoc face dn

-- | Returns this edge with a specific orientation.
elocDirected :: ELoc -> Bool -> DELoc
elocDirected edge reversed = DELoc edge reversed

-- | Returns the two faces touching this edge.
elocFaces :: ELoc -> [FLoc]
elocFaces (ELoc face n) =
  [face, flocAdvance face (Dir n) 1]

-- | Returns the two vertices at the ends of this edge.
elocVertices :: ELoc -> [VLoc]
elocVertices (ELoc face n) = [flocVertex face dir, flocVertex face (dirClockwise dir 1)]
  where
    dir = Dir n

instance Show ELoc where
  show (ELoc face n) = "ELoc(" ++ show face ++ "," ++ show n ++ ")"

-- | Represents a directed edge on a hexagonal grid.
data DELoc = DELoc
  { delocEdgeLoc :: ELoc   -- ^ The underlying undirected edge
  , delocReversed :: Bool  -- ^ Whether the direction is reversed relative to the default
  }
  deriving (Eq, Ord)

-- | Returns the same edge pointing in the opposite direction.
delocReverse :: DELoc -> DELoc
delocReverse (DELoc edge rev) = DELoc edge (not rev)

-- | Returns the face to the right of this directed edge.
delocRightFace :: DELoc -> FLoc
delocRightFace (DELoc (ELoc f n) rev) =
  if rev
    then flocAdvance f (Dir n) 1
    else f

-- | Returns the face to the left of this directed edge.
delocLeftFace :: DELoc -> FLoc
delocLeftFace (DELoc (ELoc f n) rev) =
  if rev
    then f
    else flocAdvance f (Dir n) 1

-- | Returns the vertex where this directed edge starts.
delocStartVertex :: DELoc -> VLoc
delocStartVertex (DELoc (ELoc f n) rev) = mkVLoc f d
  where
    dd = if rev then 1 else 0
    d = Dir (n + dd)

-- | Returns the vertex where this directed edge ends.
delocEndVertex :: DELoc -> VLoc
delocEndVertex (DELoc (ELoc f n) rev) = mkVLoc f d
  where
    dd = if rev then 0 else 1
    d = Dir (n + dd)

-- | Returns the face directly in front of this directed edge.
delocForwardFace :: DELoc -> FLoc
delocForwardFace (DELoc (ELoc f n) rev) = flocAdvance f (Dir (n + d)) 1
  where
    d = if rev then -1 else 1

-- | Returns the next directed edge when turning left.
delocNextEdgeLeft :: DELoc -> DELoc
delocNextEdgeLeft deloc@(DELoc (ELoc _ n) rev) = flocDirectedEdge ff (Dir (n + d)) True
  where
    d = if rev then 2 else -1
    ff = delocForwardFace deloc

-- | Returns the next directed edge when turning right.
delocNextEdgeRight :: DELoc -> DELoc
delocNextEdgeRight deloc@(DELoc (ELoc _ n) rev) = flocDirectedEdge ff (Dir (n + d)) False
  where
    d = if rev then 1 else -2
    ff = delocForwardFace deloc

instance Show DELoc where
  show (DELoc edge rev) = "DELoc(" ++ show edge ++ "," ++ show rev ++ ")"

-- | Represents the location of a vertex where three faces meet.
data VLoc = VLoc
  { vlocFaceLoc :: FLoc  -- ^ One of the faces touching this vertex
  , vlocNumber :: Int    -- ^ The vertex index (0 or 1) relative to face_loc
  }
  deriving (Eq, Ord)

-- | Internal constructor for 'VLoc'. Use the methods in 'FLoc', 'DELoc' to create instances.
mkVLoc :: FLoc -> Dir -> VLoc
mkVLoc face dir =
  case normalize face dir of
    (finalFace, finalDir) -> VLoc finalFace (dirNumber finalDir)
  where
    normalize f d@(Dir n)
      | n >= 2 = normalize (flocAdvance f d 1) (dirCounterClockwise d 2)
      | otherwise = (f, d)

-- | Returns the three faces meeting at this vertex.
vlocFaces :: VLoc -> [FLoc]
vlocFaces (VLoc f n) = [f, flocAdvance f (dirCounterClockwise dir 1) 1, flocAdvance f dir 1]
  where
    dir = Dir n

-- | Returns the three edges meeting at this vertex.
vlocEdges :: VLoc -> [ELoc]
vlocEdges (VLoc f n)
  | n == 0 = [ flocEdge f (Dir 0)
             , edge 5 2
             , edge 5 1
             ]
  | otherwise = [ flocEdge f (Dir 1)
                , flocEdge f (Dir 0)
                , edge 0 2
                ]
  where
    edge advDir edgeDir = flocEdge (flocAdvance f (Dir advDir) 1) (Dir edgeDir)

instance Show VLoc where
  show (VLoc face n) = "VLoc(" ++ show face ++ "," ++ show n ++ ")"

-- | Partitions a set of face locations into connected regions.
-- Two faces are in the same region if they are adjacent and the edge between them
-- is NOT in the barrier set.
--
-- Properties:
-- 1. Every element of the input face set appears in exactly one region
-- 2. All region sets are disjoint
-- 3. If face x is in a region and face y is adjacent to x, and the edge between
--    x and y is not in the barrier set, then y is in the same region as x
findRegions :: Set FLoc -> Set ELoc -> Map Int (Set FLoc)
findRegions faces barriers = go 0 faces Map.empty
  where
    -- Process all remaining faces, assigning each to a region
    go :: Int -> Set FLoc -> Map Int (Set FLoc) -> Map Int (Set FLoc)
    go regionId remaining regions =
      case Set.minView remaining of
        Nothing -> regions
        Just (seed, rest) ->
          let region = floodFill seed remaining
              newRemaining = Set.difference rest region
              newRegions = Map.insert regionId region regions
          in go (regionId + 1) newRemaining newRegions

    -- Flood fill from a seed face to find all reachable faces
    floodFill :: FLoc -> Set FLoc -> Set FLoc
    floodFill seed available = bfs (Set.singleton seed) Set.empty
      where
        bfs :: Set FLoc -> Set FLoc -> Set FLoc
        bfs frontier visited
          | Set.null frontier = visited
          | otherwise =
              let newVisited = Set.union visited frontier
                  neighbors = Set.unions (map (getReachableNeighbors available) (Set.toList frontier))
                  newFrontier = Set.difference neighbors newVisited
              in bfs newFrontier newVisited

    -- Get all neighbors of a face that are reachable (not blocked by barriers)
    getReachableNeighbors :: Set FLoc -> FLoc -> Set FLoc
    getReachableNeighbors available face =
      Set.fromList
        [ neighbor
        | dir <- allDirections
        , let edge = flocEdge face dir
        , not (Set.member edge barriers)
        , let neighbor = flocAdvance face dir 1
        , Set.member neighbor available
        ]
