module App.Piece where

import Data.Aeson qualified as JS
import KOI.Basics (PlayerId)

-- | Game pieces
data Piece
  = PlayerPiece PlayerId PlayerPieceType
  | Structure StructureType
  deriving (Read, Show)

-- | Types of player pieces
data PlayerPieceType
  = God
  | Soldier
  | Guardian GuardianType
  deriving (Read, Show)

-- | Types of structures
data StructureType
  = Temple
  | Obelisk
  | Pyramid
  deriving (Read, Show)

-- | Placeholder for guardian types
data GuardianType = GuardianType
  deriving (Read, Show)

-- JSON instances
instance JS.ToJSON GuardianType where
  toJSON GuardianType = JS.object []

instance JS.ToJSON PlayerPieceType where
  toJSON x =
    case x of
      God -> JS.String "god"
      Soldier -> JS.String "soldier"
      Guardian guardianType -> JS.object
        [ "type" JS..= JS.String "guardian"
        , "guardianType" JS..= guardianType
        ]

instance JS.ToJSON StructureType where
  toJSON x =
    case x of
      Temple  -> JS.String "temple"
      Obelisk -> JS.String "obelisk"
      Pyramid -> JS.String "pyramid"

instance JS.ToJSON Piece where
  toJSON x =
    case x of
      PlayerPiece player pieceType -> JS.object
        [ "player" JS..= player
        , "pieceType" JS..= pieceType
        ]
      Structure structType -> JS.object
        [ "type" JS..= JS.String "structure"
        , "structureType" JS..= structType
        ]
