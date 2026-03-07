module App.Event where

import Data.Text(Text)
import Data.Aeson(ToJSON(..))

data Event = ControlMonument | ResolveConflicts Conflict | SplitRegion

data Conflict =
    NormalConflict
  | MergeConflict
  | EliminateConflict
  | FinalConflict

conflictText :: Conflict -> Text
conflictText conflict =
  case conflict of
    NormalConflict      -> "normal"
    MergeConflict       -> "merge"
    EliminateConflict   -> "eliminate"
    FinalConflict       -> "final"

instance ToJSON Event where
  toJSON event =
    case event of
      ControlMonument       -> "control"
      ResolveConflicts c    -> toJSON ("conflict-" <> conflictText c)
      SplitRegion           -> "split"