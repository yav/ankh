module App.Input where

import GHC.Generics(Generic)
import Data.Aeson qualified as JS
import Data.Text (Text)
import Coord (ELoc, FLoc)
import App.ActionType (Action)
import App.Cards (Card)
import App.Piece (StructureType)

data Input
  = ChooseHex FLoc
  | ChooseEdge ELoc
  | ChoosePiece FLoc
  | ChooseAction Action
  | TextQuestion Text
  | AskBid Int [Int]      -- ^ Bid amount, teammate bids
  | ChooseCard Card Bool  -- ^ Card to play, teammate selected this card
  | ChooseMonumentType StructureType Bool -- ^ Type, teammate selected this type
  deriving (Read,Show,Eq,Ord,Generic,JS.ToJSON,JS.FromJSON)

normalizeInput :: Input -> Input
normalizeInput input =
    case input of
      AskBid bid _ -> AskBid bid []
      ChooseCard card _ -> ChooseCard card False
      ChooseMonumentType stype _ -> ChooseMonumentType stype False
      other -> other
