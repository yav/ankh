module App.Input where

import GHC.Generics(Generic)
import Data.Aeson qualified as JS
import Data.Text (Text)
import Coord (ELoc, FLoc)
import App.ActionType (Action)
import App.Cards (Card)
import App.Piece (StructureType)

data Input
  = ChooseHex FLoc Bool
  | ChooseEdge ELoc Bool
  | ChoosePiece FLoc Bool
  | ChooseAction Action
  | TextQuestion Text Bool
  | AskBid Int [Int]      -- ^ Bid amount, teammate bids
  | ChooseCard Card Bool  -- ^ Card to play, teammate selected this card
  | ChooseMonumentType StructureType Bool -- ^ Type, teammate selected this type
  deriving (Read,Show,Eq,Ord,Generic,JS.ToJSON,JS.FromJSON)

normalizeInput :: Input -> Input
normalizeInput input =
    case input of
      ChooseHex loc _ -> ChooseHex loc False
      ChooseEdge loc _ -> ChooseEdge loc False
      ChoosePiece loc _ -> ChoosePiece loc False
      TextQuestion t _ -> TextQuestion t False
      AskBid bid _ -> AskBid bid []
      ChooseCard card _ -> ChooseCard card False
      ChooseMonumentType stype _ -> ChooseMonumentType stype False
      ChooseAction x -> ChooseAction x
