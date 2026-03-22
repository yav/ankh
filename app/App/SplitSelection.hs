module App.SplitSelection
  ( doSpliltRegion
  ) where

import Data.List (foldl')
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T

import KOI.Basics (PlayerId)
import Coord (FLoc)
import Coord qualified

import App.Board
  ( Board(..)
  , RegionId
  , borderHexagons
  , subregionEdges
  )
import App.Input (Input(..))
import App.KOI
import App.State (State(..))
import qualified App.State as State

-- | Ask the player to select a connected subregion for splitting.
-- The selected subregion must remain connected by construction, and can end
-- only when it has at least 6 hexes and no more than 6 separating edges.
doSpliltRegion :: PlayerId -> Interact (Maybe (RegionId, Set FLoc))
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
            [ (loc, (rid, wholeRegion))
            | (rid, wholeRegion) <- eligibleRegions
            , loc <- Set.toList (borderHexagons wholeRegion)
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
          choose pid (questionFor pid "Select the starting hex for split")
            [ (ChooseHex loc, T.pack (show loc))
            | loc <- startCandidates
            ]
        case startChoice of
          ChooseHex start ->
            case Map.lookup start startInfo of
              Just (rid, wholeRegion) ->
                do
                  let (memo, viable) = splitHasViableFutureWithMemo wholeRegion Map.empty (Set.singleton start)
                  if viable
                    then
                      do
                        selected <- splitLoop pid wholeRegion memo (Set.singleton start)
                        pure (Just (rid, selected))
                    else
                      chooseStart startInfo (filter (/= start) startCandidates)
              Nothing -> chooseStart startInfo startCandidates
          _ -> pure Nothing

splitLoop :: PlayerId -> Set FLoc -> SplitMemo -> Set FLoc -> Interact (Set FLoc)
splitLoop pid wholeRegion memo selected =
  do
    st <- getState
    update (State.setSplitSelection selected st)

    let (memo1, candidateHexes) = selectableNext wholeRegion memo selected
        canEnd = endSplitValid wholeRegion selected
        choices =
          [ (ChooseHex loc, T.pack (show loc))
          | loc <- Set.toList candidateHexes
          ]
        choicesWithEnd =
          if canEnd
            then (TextQuestion "End Split", "Finish selecting this split") : choices
            else choices
    case choicesWithEnd of
      [] -> pure selected
      _ -> do
        choice <- choose pid (questionFor pid "Select hexagons for the new subregion") choicesWithEnd
        case choice of
          ChooseHex loc
            | Set.member loc candidateHexes -> splitLoop pid wholeRegion memo1 (Set.insert loc selected)
          TextQuestion "End Split"
            | canEnd -> pure selected
          _ -> splitLoop pid wholeRegion memo1 selected

-- | The player may end splitting once the new subregion is big enough and
-- its separating boundary is small enough.
endSplitValid :: Set FLoc -> Set FLoc -> Bool
endSplitValid wholeRegion selected =
  Set.size selected >= 6 && Set.size (subregionEdges wholeRegion selected) <= 6

-- | Remaining hexes outside the currently selected subregion.
remainingHexes :: Set FLoc -> Set FLoc -> Int
remainingHexes wholeRegion selected = Set.size wholeRegion - Set.size selected

-- | Immediate adjacent growth candidates that stay within the original region.
rawAdjacentCandidates :: Set FLoc -> Set FLoc -> Set FLoc
rawAdjacentCandidates wholeRegion selected =
  Set.fromList
    [ neighbor
    | loc <- Set.toList selected
    , dir <- Coord.allDirections
    , let neighbor = Coord.flocAdvance loc dir 1
    , Set.member neighbor wholeRegion
    , not (Set.member neighbor selected)
    ]

-- | Next selectable hexagons, excluding moves that would force an invalid dead end.
selectableNext :: Set FLoc -> SplitMemo -> Set FLoc -> (SplitMemo, Set FLoc)
selectableNext wholeRegion memo selected
  | remainingHexes wholeRegion selected <= 6 = (memo, Set.empty)
  | otherwise =
      foldl'
        step
        (memo, Set.empty)
        (Set.toList (rawAdjacentCandidates wholeRegion selected))
  where
    step (m, acc) loc =
      let (m1, ok) = splitHasViableFutureWithMemo wholeRegion m (Set.insert loc selected)
      in if ok then (m1, Set.insert loc acc) else (m1, acc)

type SplitMemo = Map.Map (Set FLoc) Bool

-- | True iff there exists a sequence of future picks leading to a valid end state.
-- This prevents offering moves that eventually force a stuck invalid state.
splitHasViableFutureWithMemo :: Set FLoc -> SplitMemo -> Set FLoc -> (SplitMemo, Bool)
splitHasViableFutureWithMemo wholeRegion memo0 selected0 = go memo0 selected0
  where
    go :: SplitMemo -> Set FLoc -> (SplitMemo, Bool)
    go memo selected =
      case Map.lookup selected memo of
        Just cached -> (memo, cached)
        Nothing ->
          let (memo1, result)
                | endSplitValid wholeRegion selected = (memo, True)
                | remainingHexes wholeRegion selected <= 6 = (memo, False)
                | otherwise =
                    let nexts = Set.toList (rawAdjacentCandidates wholeRegion selected)
                    in tryNext memo nexts
              memo2 = Map.insert selected result memo1
          in (memo2, result)
      where
        tryNext m [] = (m, False)
        tryNext m (loc : more) =
          let (m1, ok) = go m (Set.insert loc selected)
          in if ok then (m1, True) else tryNext m1 more
