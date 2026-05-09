module App.StateView where

import KOI.Basics
import Data.Aeson qualified as JS
import Data.Map qualified as Map
import App.ActionType ()
import App.State

newtype StateView = StateView State

getStateView :: PlayerId -> State -> StateView
getStateView pid st =
  case Map.lookup (playerStateId st pid) (statePlayers st) of
    Just ps -> StateView st { statePlayers = addFollower (Map.insert pid ps (statePlayers st)) }
    Nothing -> StateView st
  where
  addFollower players =
    case playerMerged st of
      Just m
        | Just ps <- Map.lookup (playerLead m) players ->
            Map.insert (playerFollow m) ps players
      _ -> players

instance JS.ToJSON StateView where
  toJSON (StateView st) = JS.object
    [ "board" JS..= stateBoard st
    , "players" JS..= Map.toList (statePlayers st)
    , "actions" JS..= Map.toList (stateActions st)
    , "structures" JS..= Map.toList (stateStructures st)
    , "splitSelection" JS..= stateSplitSelection st
    , "log" JS..= reverse (stateLog st)  -- Log is stored reversed, reverse for JSON
    , "merged" JS..= fmap mergedToJSON (playerMerged st)
    ]
    where
    mergedToJSON m = JS.object
      [ "lead" JS..= playerLead m
      , "follow" JS..= playerFollow m
      ]
