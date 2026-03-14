module App.Input where

import GHC.Generics(Generic)
import Data.Aeson qualified as JS
import Data.Text (Text)
import Coord (FLoc)

data Input
  = Placeholder
  | ChooseHex FLoc
  | TextQuestion Text
  deriving (Read,Show,Eq,Ord,Generic,JS.ToJSON,JS.FromJSON)
