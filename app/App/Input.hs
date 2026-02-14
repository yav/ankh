module App.Input where

import GHC.Generics(Generic)
import Data.Aeson qualified as JS

-- TODO: Define actual game input types
data Input = Placeholder
  deriving (Read,Show,Eq,Ord,Generic,JS.ToJSON,JS.FromJSON)
