module App.Input where

import GHC.Generics(Generic)
import Data.Aeson qualified as JS
import Data.Text (Text)
import Coord (ELoc, FLoc)
import App.ActionType (Action)
import App.Cards (Card)

data Input
  = ChooseHex FLoc
  | ChooseEdge ELoc
  | ChoosePiece FLoc
  | ChooseAction Action
  | TextQuestion Text
  | AskBid Int
  | ChooseCard Card
  deriving (Read,Show,Eq,Ord,Generic,JS.ToJSON,JS.FromJSON)
