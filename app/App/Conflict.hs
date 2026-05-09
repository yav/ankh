module App.Conflict
  ( BattleState(..)
  , emptyBattleState
  , doRegionConflict
  , scoreRegionMajority
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.List (foldl', partition, sortBy)
import Control.Monad (forM_, when)
import Data.Ord (comparing)

import KOI.Basics (PlayerId(..))
import App.KOI
import App.State
  ( State(..), gainFollowers, loseFollowers
  , playerStateId, lookupPlayer, devotion, hasPower)
import App.LogItem (LogWord(..))
import App.Piece (Piece(..), PlayerPieceType(..), StructureType(..), pieceOwner)
import App.Cards (Card(..), cardStrength)
import App.Powers (Power(..))
import App.Board
  ( Board(..)
  , Hex(..)
  , RegionId
  , Terrain(..)
  , regionPieces
  , adjacentHexes
  )
import App.Input (Input(..))
import App.PlayerState (PlayerState(..), reclaimCards)
import App.ActionsBasic (playCards, placeBids, doBuild, doLog, doLogMultiple, doStartLogGroup, doGainDevotion)
import Coord (FLoc)

{- Gain devotion:
1-Monument Majority (+ Bountiful Ankh Power)
2-Winner of the Battle (+ Drought Card + Glorious Ankh
Power + Bountiful Ankh Power)
3-Magnanimous Ankh Power (+ Bountiful Ankh Power)
4-Miracle Card (+ Bountiful Ankh Power)
5-Worshipful Ankh Power (+ Bountiful Ankh Power
-}

data BattleState = BattleState
  { battleRegion       :: !RegionId
  , battleLocs         :: !(Set FLoc)
  , battlePlayers      :: !(Set PlayerId)
  , battleCards        :: !(Map PlayerId Card)
  , battleFloodProtected :: !(Set FLoc)
  , battleKills        :: !(Map PlayerId Int)
  , battleFiguresAtResolution :: !(Map PlayerId Int)
  , battleTiebreaker   :: !(Maybe PlayerId)
  , battleWinner       :: !(Maybe PlayerId)
  , battleLosers       :: !(Set PlayerId)
  }

emptyBattleState ::
  Board -> RegionId -> Set PlayerId -> Maybe PlayerId -> BattleState
emptyBattleState board rid players tiebreaker = BattleState
  { battleRegion       = rid
  , battleLocs         = Map.findWithDefault Set.empty rid (boardRegions board)
  , battlePlayers      = players
  , battleCards        = Map.empty
  , battleFloodProtected = Set.empty
  , battleKills        = Map.empty
  , battleFiguresAtResolution = Map.empty
  , battleTiebreaker   = tiebreaker
  , battleWinner       = Nothing
  , battleLosers       = Set.empty
  }

battleHexes :: Board -> BattleState -> [(FLoc, Hex)]
battleHexes board bs =
  [ (loc, hex)
  | loc <- Set.toList (battleLocs bs)
  , Just hex <- [Map.lookup loc (boardHexes board)]
  ]

-------------------------------------------------------------------------------
-- Region conflict
-------------------------------------------------------------------------------

doRegionConflict :: Maybe PlayerId -> RegionId -> Interact ()
doRegionConflict tiebreaker rid =
  do
    board <- getsState stateBoard
    let pieces = regionPieces board rid
        presentPlayers =
          Set.toList (Set.fromList [ p | PlayerPiece p _ <- pieces ])
    case presentPlayers of
      [] -> pure ()
      [p] ->
        do
          doStartLogGroup
          doLog [LogText "Conflict in region", LogRegion rid]
          scoreRegionMajority rid
          doGainDevotion p 1
            [LogText "dominates"]
      _ ->
        do
          doStartLogGroup
          doLog [LogText "Conflict in region", LogRegion rid]
          let bs = emptyBattleState board rid (Set.fromList presentPlayers) tiebreaker
          bs1 <- doPlayCards bs
          bs2 <- doFlood bs1
          bs3 <- doPlague bs2
          buildMonument bs3
          scoreRegionMajority rid
          bs4 <- doBattleResolution bs3
          doMagnanimous bs4
          doMiracle bs4
          doWorshipful bs4
          doCycleOfMaat bs4

-------------------------------------------------------------------------------
-- Monument majority
-------------------------------------------------------------------------------

-- No need to worry about teams, because followers shouldn't have
-- pieces on the board
scoreRegionMajority :: RegionId -> Interact ()
scoreRegionMajority rid =
  do
    board <- getsState stateBoard
    let pieces = regionPieces board rid

        -- Players who own at least one piece in the region
        presentPlayers =
          Set.fromList [ p | piece <- pieces, Just p <- [pieceOwner piece] ]

        -- Count owned structures per (PlayerId, StructureType)
        structCounts :: Map (PlayerId, StructureType) Int
        structCounts =
          Map.fromListWith (+)
            [ ((p, st), 1)
            | Structure (Just p) st <- pieces
            ]

        -- For each structure type, find the strict majority winner
        winnerFor :: StructureType -> Maybe PlayerId
        winnerFor stype =
          let counts =
                [ (p, n)
                | p <- Set.toList presentPlayers
                , let n = Map.findWithDefault 0 (p, stype) structCounts
                , n > 0
                ]
          in case sortBy (flip (comparing snd)) counts of
               (p1, n1) : (_, n2) : _ | n1 > n2 -> Just p1
               [(p1, _)]                         -> Just p1
               _                                 -> Nothing

        -- Collect winning structure types per player
        winnings :: Map PlayerId [StructureType]
        winnings =
          Map.fromListWith (++)
            [ (p, [stype])
            | stype <- [minBound .. maxBound]
            , Just p <- [winnerFor stype]
            ]

    -- Sort winners by playerDevotion ascending (fewest first)
    players <- getsState statePlayers
    let sortedWinners =
          sortBy (comparing (\(p, _) -> Map.lookup p players >>= Just . playerDevotion))
            (Map.toList winnings)

    -- Award points one at a time, re-reading state each iteration
    mapM_ awardWinner sortedWinners

  where
    awardWinner (p, stypes) =
      doGainDevotion p (length stypes)
        ( LogText "won"
        : concatMap (\s -> [LogStructure s]) stypes
        ++ [LogText "majority"]
        )

-------------------------------------------------------------------------------
-- Battle phases
-------------------------------------------------------------------------------

-- Phase 1: All players simultaneously choose a card
doPlayCards :: BattleState -> Interact BattleState
doPlayCards bs =
  do
    let pids = Set.toList (battlePlayers bs)
    cardInputs <- playCards pids
    let cards = Map.fromList
          [ (pid, card)
          | (pid, ChooseCard card _) <- Map.toList cardInputs
          ]
    pure bs { battleCards = cards }

-- Phase 2: Flood — gain followers and mark figures as protected
doFlood :: BattleState -> Interact BattleState
doFlood bs =
  do
    let floodPlayers =
          [ pid
          | (pid, Flood) <- Map.toList (battleCards bs)
          ]
    case floodPlayers of
      [] -> pure bs
      _  ->
        do
          st <- getState
          let floodInfo =
                [ (pid, loc, count)
                | pid <- floodPlayers
                , let lid = playerStateId st pid
                , (loc, hex) <- battleHexes (stateBoard st) bs
                , hexTerrain hex == Grass
                , let count = length
                        [ ()
                        | PlayerPiece p _ <- hexPieces hex
                        , p == lid
                        ]
                , count > 0
                ]

              gains = Map.fromListWith (+)
                [ (pid, count) | (pid, _, count) <- floodInfo ]

          st1 <- getState
          let applyGains s (pid, n) =
                gainFollowers (playerStateId s pid) n s
          update (foldl' applyGains st1 (Map.toList gains))

          doLogMultiple
            [ [LogPlayer pid, LogText "gained", LogFollowers n, LogText "(flood)"]
            | (pid, n) <- Map.toList gains
            ]

          pure bs
            { battleFloodProtected =
                Set.fromList [ loc | (_, loc, _) <- floodInfo ]
            }

-- Phase 3: Plague of Locusts — bid, kill losers' non-god figures in region
doPlague :: BattleState -> Interact BattleState
doPlague bs =
  do
    let plaguePlayers =
          [ pid | (pid, PlagueOfLocusts) <- Map.toList (battleCards bs) ]
    case plaguePlayers of
      [] -> pure bs
      _  ->
        do
          bids <- placeBids (Set.toList (battlePlayers bs))
          let bidAmounts  = [ (pid, n) | (pid, AskBid n _) <- Map.toList bids ]
              maxBid     = maximum (map snd bidAmounts)
              topBidders = [ pid | (pid, n) <- bidAmounts, n == maxBid ]
              saved = case topBidders of
                        [pid] -> Just pid
                        _     -> Nothing
              losers = case saved of
                         Just winner -> Set.fromList
                           [ pid | (pid, _) <- bidAmounts, pid /= winner ]
                         Nothing -> Set.fromList (map fst bidAmounts)

          -- Plague doesn't respect flood protection
          bs1 <- killFigures bs losers (battleLocs bs) Set.empty

          case saved of
            Just winner ->
              doLog [ LogPlayer winner, LogText "survived the plague" ]
            Nothing ->
              doLog [ LogText "No one survived the plague" ]

          pure bs1

-- Phase 4: Build Monument — each player who played it may place a monument
buildMonument :: BattleState -> Interact ()
buildMonument bs =
  do
    st0 <- getState
    let builders =
          sortBy (comparing (devotion st0))
            [ pid | (pid, BuildMonument) <- Map.toList (battleCards bs) ]
    forM_ builders \pid ->
      do
        st <- getState
        let mbPS = lookupPlayer st pid
            hasMarkers   = maybe False (\ps -> playerBuildLimit ps > 0) mbPS
            hasInspiring = hasPower Inspiring st pid
            cost         = if hasInspiring then 0 else 3
            canAfford    = maybe False (\ps -> playerFollowers ps >= cost) mbPS

            availableTypes =
              [ stype
              | stype <- [minBound .. maxBound]
              , Map.findWithDefault 0 stype (stateStructures st) > 0
              ]

            emptySpaces =
              [ loc
              | loc <- Set.toList (battleLocs bs)
              , Just hex <- [Map.lookup loc (boardHexes (stateBoard st))]
              , null (hexPieces hex)
              , hexTerrain hex /= Water
              ]

        when (hasMarkers && canAfford
              && not (null availableTypes) && not (null emptySpaces))
          (doBuild pid cost availableTypes emptySpaces)

-------------------------------------------------------------------------------
-- Battle resolution
-------------------------------------------------------------------------------

-- Phase 6: Battle Resolution
doBattleResolution :: BattleState -> Interact BattleState
doBattleResolution bs =
  do
    (mbWinner, winMargin, bs1) <- determineBattleWinner bs
    let losers =
          case mbWinner of
            Nothing -> battlePlayers bs
            Just winner -> Set.delete winner (battlePlayers bs)
        bs2 = bs1 { battleWinner = mbWinner, battleLosers = losers }
    case mbWinner of
      Nothing -> doLog [LogText "The battle is a tie!"]
      Just winner -> awardBattleWinner winner winMargin bs2
    killFigures bs2 (battleLosers bs2) (battleLocs bs2) (battleFloodProtected bs2)

-- Compute strengths, determine winner (asking about tiebreaker if needed),
-- and record figure counts at resolution time.
determineBattleWinner ::
  BattleState -> Interact (Maybe PlayerId, Int, BattleState)
determineBattleWinner bs =
  do
    st <- getState
    let board   = stateBoard st
        hexes   = battleHexes board bs
        players = statePlayers st

        figureCounts :: Map PlayerId Int
        figureCounts = Map.fromListWith (+)
          [ (p, 1)
          | (_loc, hex) <- hexes
          , PlayerPiece p _ <- hexPieces hex
          ]

        strengths :: Map PlayerId Int
        strengths = Map.fromList
          [ (pid, baseStr + cardStr + templeStr)
          | pid <- Set.toList (battlePlayers bs)
          , Map.findWithDefault 0 pid figureCounts > 0
          , let pPowers   = maybe Set.empty playerPowers (Map.lookup pid players)
                baseStr   = figureStrength board pid pPowers hexes
                cardStr   = maybe 0 cardStrength (Map.lookup pid (battleCards bs))
                templeStr = templeAttunedStrength board pid hexes pPowers
          ]

        ranked = sortBy (flip (comparing snd)) (Map.toList strengths)

    (mbWinner, usedTiebreaker) <-
      case ranked of
        [(p1, _)]     -> pure (Just p1, False)
        (p1, s1) : (_, s2) : _
          | s1 > s2   -> pure (Just p1, False)
          | otherwise ->
              case battleTiebreaker bs of
                Just tb | Map.lookup tb strengths == Just s1 ->
                  do
                    useIt <- choose tb
                      (questionFor tb "Use tiebreaker token to win the battle?")
                      [ (TextQuestion "Yes" False, "Use tiebreaker token")
                      , (TextQuestion "No" False, "Keep tiebreaker token")
                      ]
                    pure
                      case useIt of
                        TextQuestion "Yes" _ -> (Just tb, True)
                        _                  -> (Nothing, False)
                _ -> pure (Nothing, False)
        _ -> pure (Nothing, False)

    let winMargin =
          case ranked of
            (_, s1) : (_, s2) : _ -> s1 - s2
            _ -> 0

        bs1 = bs
          { battleFiguresAtResolution = figureCounts
          , battleTiebreaker =
              if usedTiebreaker then Nothing else battleTiebreaker bs
          }

    pure (mbWinner, winMargin, bs1)

awardBattleWinner :: PlayerId -> Int -> BattleState -> Interact ()
awardBattleWinner winner winMargin bs =
  do
    st <- getState
    let droughtBonus =
          if Map.lookup winner (battleCards bs) == Just Drought
          then length
            [ ()
            | (_loc, hex) <- battleHexes (stateBoard st) bs
            , hexTerrain hex == Desert
            , PlayerPiece p _ <- hexPieces hex
            , p == winner
            ]
          else 0

        glorious = hasPower Glorious st winner
        reward = (if glorious && winMargin >= 3 then 3 else 1)
               + droughtBonus

    doGainDevotion winner reward [LogText "won the battle"]

    when (hasPower Commanding st winner)
      do
        st2 <- getState
        update (gainFollowers (playerStateId st2 winner) 3 st2)
        doLog [ LogPlayer winner, LogText "gained"
              , LogFollowers 3, LogText "(commanding)" ]

-- Phase 7: Magnanimous — losers with 2+ figures at resolution gain 2 devotion
doMagnanimous :: BattleState -> Interact ()
doMagnanimous bs =
  do
    st <- getState
    let eligible =
          sortBy (comparing (devotion st))
            [ pid
            | pid <- Set.toList (battleLosers bs)
            , hasPower Magnanimous st pid
            , Map.findWithDefault 0 pid (battleFiguresAtResolution bs) >= 2
            ]
    forM_ eligible \pid ->
      do
        doGainDevotion pid 2 [LogText "magnanimous"]

-- Phase 8: Miracle — 1 devotion per figure killed during battle
doMiracle :: BattleState -> Interact ()
doMiracle bs =
  do
    st <- getState
    let eligible =
          sortBy (comparing (devotion st))
            [ pid
            | (pid, Miracle) <- Map.toList (battleCards bs)
            ]
    forM_ eligible \pid ->
      do
        st1 <- getState
        let kills = Map.findWithDefault 0 (playerStateId st1 pid)
                                          (battleKills bs)
        when (kills > 0)
          (doGainDevotion pid kills [LogText "miracle"])

-- Phase 9: Worshipful — sacrifice 2 followers for 1 devotion
doWorshipful :: BattleState -> Interact ()
doWorshipful bs =
  do
    st <- getState
    let eligible =
          sortBy (comparing (devotion st))
            [ pid
            | pid <- Set.toList (battlePlayers bs)
            , hasPower Worshipful st pid
            , maybe False (\ps -> playerFollowers ps >= 2) (lookupPlayer st pid)
            ]
    forM_ eligible \pid ->
      do
        answer <- choose pid
          (questionFor pid "Sacrifice 2 followers for 1 devotion?")
          [ (TextQuestion "Yes" False, "Sacrifice 2 followers")
          , (TextQuestion "No" False, "Keep followers")
          ]
        case answer of
          TextQuestion "Yes" _ ->
            do
              st1 <- getState
              let lid = playerStateId st1 pid
              update (loseFollowers lid 2 st1)
              doGainDevotion pid 1
                [LogText "worshipful, sacrificed", LogFollowers 2]
          _ -> pure ()

-- Phase 10: Cycle of Ma'at — reclaim all played cards to hand
doCycleOfMaat :: BattleState -> Interact ()
doCycleOfMaat bs =
  do
    st <- getState
    let players =
          [ (pid, playerStateId st pid)
          | (pid, CycleOfMaat) <- Map.toList (battleCards bs)
          ]
    forM_ players \(pid, lid) ->
      do
        st1 <- getState
        update st1
          { statePlayers = Map.adjust reclaimCards lid (statePlayers st1) }
        doLog [ LogPlayer pid, LogText "reclaimed cards" ]

-------------------------------------------------------------------------------
-- Strength helpers
-------------------------------------------------------------------------------

-- Compute base figure strength for a player in a region.
-- Each figure = 1, except God = 3 if Resplendent and player owns 3+
-- monuments of the same type anywhere on the board.
figureStrength :: Board -> PlayerId -> Set Power -> [(FLoc, Hex)] -> Int
figureStrength board pid powers hexes =
  sum [ pieceStr piece | (_loc, hex) <- hexes, piece <- hexPieces hex
      , isOwned piece ]
  where
  isOwned (PlayerPiece p _) = p == pid
  isOwned _                 = False

  godStrength
    | not (Set.member Resplendent powers) = 1
    | otherwise =
        if any (\stype ->
          length [ ()
                 | (_, h) <- Map.toList (boardHexes board)
                 , Structure (Just p) st <- hexPieces h
                 , p == pid, st == stype
                 ] >= 3
          ) [minBound .. maxBound]
        then 3 else 1

  pieceStr (PlayerPiece _ God) = godStrength
  pieceStr (PlayerPiece _ _)   = 1
  pieceStr _                   = 0

-- TempleAttuned: +2 per owned temple in region with at least 1 adjacent figure
templeAttunedStrength :: Board -> PlayerId -> [(FLoc, Hex)] -> Set Power -> Int
templeAttunedStrength board pid hexes powers
  | not (Set.member TempleAttuned powers) = 0
  | otherwise = 2 * length
      [ ()
      | (loc, hex) <- hexes
      , Structure (Just p) Temple <- hexPieces hex
      , p == pid
      , any (hasAdjacentFigure loc) hexes
      ]
  where
  hasAdjacentFigure templeLoc (figLoc, figHex) =
    adjacentHexes board templeLoc figLoc
    && any isOurFigure (hexPieces figHex)
  isOurFigure (PlayerPiece p _) = p == pid
  isOurFigure _                 = False

-------------------------------------------------------------------------------
-- Kill helpers
-------------------------------------------------------------------------------

-- Kill non-god figures belonging to the given set of players in the
-- given hex locations, skipping protected locations. Updates the board,
-- returns soldiers to player pools, and accumulates kill counts in the
-- BattleState.
killFigures ::
  BattleState -> Set PlayerId -> Set FLoc -> Set FLoc -> Interact BattleState
killFigures bs losers targetLocs protected =
  do
    st <- getState
    let loserLids = Set.map (playerStateId st) losers
        board     = stateBoard st

        processHex loc (kills, soldiers, hexMap) =
          case Map.lookup loc hexMap of
            Nothing -> (kills, soldiers, hexMap)
            Just hex
              | Set.member loc protected -> (kills, soldiers, hexMap)
              | otherwise ->
                  let (removed, kept) =
                        partition (isLoserNonGod loserLids) (hexPieces hex)
                      newKills = foldl' countKill kills removed
                      newSoldiers = foldl' countSoldier soldiers removed
                  in (newKills, newSoldiers,
                      Map.insert loc (hex { hexPieces = kept }) hexMap)

        (killCounts, soldierCounts, newHexMap) =
          foldr processHex (Map.empty, Map.empty, boardHexes board)
                (Set.toList targetLocs)

        -- TODO: also return guardians once player state tracks them
        newPlayers =
          Map.mapWithKey (returnSoldiers soldierCounts) (statePlayers st)

    update st
      { stateBoard   = board { boardHexes = newHexMap }
      , statePlayers = newPlayers
      }

    let mergedKills = Map.unionWith (+) (battleKills bs) killCounts
    pure bs { battleKills = mergedKills }
  where
  isLoserNonGod loserLids piece =
    case piece of
      PlayerPiece _ God -> False
      PlayerPiece pid _ -> Set.member pid loserLids
      _                 -> False

  countKill acc piece =
    case piece of
      PlayerPiece pid _ -> Map.insertWith (+) pid 1 acc
      _                 -> acc

  countSoldier acc piece =
    case piece of
      PlayerPiece pid Soldier -> Map.insertWith (+) pid 1 acc
      _                       -> acc

  returnSoldiers soldierCounts pid ps =
    case Map.lookup pid soldierCounts of
      Just n  -> ps { playerSoldiers = playerSoldiers ps + n }
      Nothing -> ps
