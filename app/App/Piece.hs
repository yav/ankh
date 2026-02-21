module App.Piece where

import Data.Aeson qualified as JS
import Data.Aeson ((.:))
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import KOI.Basics (PlayerId)

-- | Game pieces
data Piece
  = PlayerPiece PlayerId PlayerPieceType
  | Structure (Maybe PlayerId) StructureType
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
      Guardian _guardianType -> JS.String "guardian"

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
        , "kind" JS..= pieceType
        ]
      Structure mbPlayer structType -> JS.object
        [ "player" JS..= mbPlayer
        , "kind" JS..= structType
        ]

instance JS.FromJSON Piece where
  parseJSON = parsePiece JS.parseJSON

-- Parsing functions

parsePiece :: (JS.Value -> Parser (Maybe PlayerId)) -> JS.Value -> Parser Piece
parsePiece parsePlayerId = JS.withObject "Piece" $ \obj -> do
  kind <- obj .: "kind"
  playerVal <- obj .: "player"
  mbPlayer <- parsePlayerId playerVal

  -- Try to parse as structure type first
  case parseStructureType kind of
    Just structType -> pure (Structure mbPlayer structType)
    Nothing -> do
      -- Must be a player piece type
      pieceType <- parsePlayerPieceType kind
      case mbPlayer of
        Nothing -> fail "Player piece requires a non-null player field"
        Just player -> pure (PlayerPiece player pieceType)

parsePlayerPieceType :: Text -> Parser PlayerPieceType
parsePlayerPieceType txt =
  case txt of
    "god" -> pure God
    "soldier" -> pure Soldier
    "guardian" -> pure (Guardian GuardianType)
    _ -> fail ("Unknown player piece type: " ++ show txt)

parseStructureType :: Text -> Maybe StructureType
parseStructureType txt =
  case txt of
    "temple" -> Just Temple
    "obelisk" -> Just Obelisk
    "pyramid" -> Just Pyramid
    _ -> Nothing
