module App.SplitSelection
  ( doSpliltRegion
  ) where

import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T

import KOI.Basics (PlayerId)
import Coord (DELoc, ELoc, FLoc)
import Coord qualified

import App.Board
  ( Board(..)
  , RegionId
  )
import App.Input (Input(..))
import App.KOI
import App.State (State(..))
import qualified App.State as State

-- | Ask the player to select a separator path inside a region.
-- The path must start on the region border, stay inside the region, and reach
-- the opposite border within at most 6 edges. The split is valid only when it
-- divides the region into two subregions of size at least 6.
doSpliltRegion :: PlayerId -> Interact (Maybe (RegionId, Set ELoc))
doSpliltRegion pid =
  do
    board <- getsState stateBoard
    let eligibleRegions =
          [ (rid, region)
          | (rid, region) <- Map.toList (boardRegions board)
          , Set.size region >= 12
          ]
        startInfo =
          Map.fromList
            [ (edge, (rid, wholeRegion))
            | (rid, wholeRegion) <- eligibleRegions
            , edge <- Set.toList (regionStartEdges wholeRegion)
            ]
        startCandidates = Map.keys startInfo
    case startCandidates of
      [] -> pure Nothing
      _ -> chooseStart startInfo startCandidates
  where
    chooseStart _ [] = pure Nothing
    chooseStart startInfo startCandidates =
      do
        startChoice <-
          choose pid (questionFor pid "Select the starting edge for split")
            [ (ChooseEdge edge, T.pack (show edge))
            | edge <- startCandidates
            ]
        case startChoice of
          ChooseEdge start ->
            case Map.lookup start startInfo of
              Just (rid, wholeRegion) ->
                case startPath wholeRegion start of
                  Nothing -> chooseStart startInfo (filter (/= start) startCandidates)
                  Just path ->
                    do
                      selected <- splitLoop pid wholeRegion path
                      pure (Just (rid, selected))
              Nothing -> chooseStart startInfo startCandidates
          _ -> pure Nothing

splitLoop :: PlayerId -> Set FLoc -> SplitPath -> Interact (Set ELoc)
splitLoop pid wholeRegion path =
  do
    st <- getState
    update (State.setSplitSelectionInvalid False (State.setSplitSelection (pathEdges path) st))

    let candidateEdges = nextPathEdges wholeRegion path
        choices =
          [ (ChooseEdge edge, T.pack (show edge))
          | edge <- candidateEdges
          ]
    case choices of
      [] -> finishSplit pid wholeRegion path
      _ -> do
        choice <- choose pid (questionFor pid "Select edges for the split") choices
        case choice of
          ChooseEdge edge
            | edge `elem` candidateEdges ->
                case extendPath wholeRegion path edge of
                  Just path1 -> splitLoop pid wholeRegion path1
                  Nothing -> splitLoop pid wholeRegion path
          _ -> splitLoop pid wholeRegion path

finishSplit :: PlayerId -> Set FLoc -> SplitPath -> Interact (Set ELoc)
finishSplit pid wholeRegion path =
  if splitValid wholeRegion (pathEdges path)
    then confirmSplit pid path
    else invalidSplit pid path

confirmSplit :: PlayerId -> SplitPath -> Interact (Set ELoc)
confirmSplit pid path =
  do
    st <- getState
    update (State.setSplitSelectionInvalid False (State.setSplitSelection (pathEdges path) st))
    choice <-
      choose pid (questionFor pid "Confirm this split")
        [ (TextQuestion "Split Region", "Apply this split") ]
    case choice of
      TextQuestion "Split Region" -> pure (pathEdges path)
      _ -> confirmSplit pid path

invalidSplit :: PlayerId -> SplitPath -> Interact (Set ELoc)
invalidSplit pid path =
  do
    st <- getState
    update (State.setSplitSelectionInvalid True (State.setSplitSelection (pathEdges path) st))
    askInputs (questionFor pid "Invalid split. Use Undo to try again.") []

data SplitPath = SplitPath
  { pathEdges :: Set ELoc
  , pathCurrent :: DELoc
  }

splitValid :: Set FLoc -> Set ELoc -> Bool
splitValid wholeRegion selectedEdges =
  case Map.elems (Coord.findRegions wholeRegion selectedEdges) of
    [leftRegion, rightRegion] ->
      Set.size leftRegion >= 6 && Set.size rightRegion >= 6
    _ -> False

startPath :: Set FLoc -> ELoc -> Maybe SplitPath
startPath wholeRegion edge
  | not (Coord.isInteriorEdge wholeRegion edge) = Nothing
  | not (any (Coord.isBoundaryVertex wholeRegion) edgeVertices) = Nothing
  | otherwise = do
      startVertex <-
        case filter (Coord.isBoundaryVertex wholeRegion) edgeVertices of
          boundaryVertex : _ -> Just boundaryVertex
          [] -> Nothing
      current <- Coord.elocDirectedFrom edge startVertex
      pure
        SplitPath
          { pathEdges = Set.singleton edge
          , pathCurrent = current
          }
  where
    edgeVertices = Coord.elocVertices edge

extendPath :: Set FLoc -> SplitPath -> ELoc -> Maybe SplitPath
extendPath wholeRegion path edge =
  case Map.lookup edge nextByEdge of
    Nothing -> Nothing
    Just nextDirectedEdge ->
      Just
        SplitPath
          { pathEdges = Set.insert edge (pathEdges path)
          , pathCurrent = nextDirectedEdge
          }
  where
    nextByEdge =
      Map.fromList
        [ (Coord.delocEdgeLoc directedEdge, directedEdge)
        | directedEdge <- nextDirectedEdges wholeRegion path
        ]

nextPathEdges :: Set FLoc -> SplitPath -> [ELoc]
nextPathEdges wholeRegion = map Coord.delocEdgeLoc . nextDirectedEdges wholeRegion

nextDirectedEdges :: Set FLoc -> SplitPath -> [DELoc]
nextDirectedEdges wholeRegion path
  | Set.size (pathEdges path) >= 6 = []
  | Coord.isBoundaryVertex wholeRegion (Coord.delocEndVertex current) = []
  | otherwise =
      [ directedEdge
      | directedEdge <- [Coord.delocNextEdgeLeft current, Coord.delocNextEdgeRight current]
      , let edge = Coord.delocEdgeLoc directedEdge
      , Coord.isInteriorEdge wholeRegion edge
      , not (edge `Set.member` pathEdges path)
      ]
  where
    current = pathCurrent path

regionStartEdges :: Set FLoc -> Set ELoc
regionStartEdges wholeRegion =
  Set.fromList
    [ edge
    | edge <- regionInteriorEdges wholeRegion
    , any (Coord.isBoundaryVertex wholeRegion) (Coord.elocVertices edge)
    ]

regionInteriorEdges :: Set FLoc -> [ELoc]
regionInteriorEdges wholeRegion =
  Set.toList . Set.fromList $
    [ Coord.flocEdge loc dir
    | loc <- Set.toList wholeRegion
    , dir <- Coord.allDirections
    , let edge = Coord.flocEdge loc dir
    , Coord.isInteriorEdge wholeRegion edge
    ]
