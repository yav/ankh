module App.Piece where

import Data.Aeson qualified as JS
import Data.Aeson ((.:))
import Data.Aeson.Types (Parser)
import Data.Maybe (isNothing)
import Data.Text (Text, stripPrefix)
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
  deriving (Eq, Ord, Enum, Bounded, Read, Show)

-- | Guardian types available in the game
data GuardianType
  = CatMummy
  | Satet
  | Mummy
  | Apep
  | GiantScorpion
  | Androsphinx
  deriving (Eq, Ord, Enum, Bounded, Read, Show)

-- | Guardian size categories
data GuardianSize = Small | Large
  deriving (Eq, Ord, Read, Show)

guardianSize :: GuardianType -> GuardianSize
guardianSize gt =
  case gt of
    CatMummy      -> Small
    Satet         -> Small
    Mummy         -> Small
    Apep          -> Large
    GiantScorpion -> Large
    Androsphinx   -> Large

guardianTier :: GuardianType -> Int
guardianTier gt =
  case gt of
    CatMummy      -> 1
    Satet         -> 1
    Mummy         -> 2
    Apep          -> 2
    GiantScorpion -> 3
    Androsphinx   -> 3

pieceOwner :: Piece -> Maybe PlayerId
pieceOwner piece =
  case piece of
    PlayerPiece playerId _ -> Just playerId
    Structure mbPlayer _ -> mbPlayer

belongsTo :: PlayerId -> Piece -> Bool
belongsTo lid piece = pieceOwner piece == Just lid

isNeutral :: Piece -> Bool
isNeutral piece = isNothing (pieceOwner piece)

isEnemyOf :: PlayerId -> Piece -> Bool
isEnemyOf lid piece = maybe False (/= lid) (pieceOwner piece)

guardianTypeText :: GuardianType -> Text
guardianTypeText gt =
  case gt of
    CatMummy      -> "cat-mummy"
    Satet         -> "satet"
    Mummy         -> "mummy"
    Apep          -> "apep"
    GiantScorpion -> "giant-scorpion"
    Androsphinx   -> "androsphinx"

parseGuardianType :: Text -> Maybe GuardianType
parseGuardianType txt =
  case txt of
    "cat-mummy"     -> Just CatMummy
    "satet"         -> Just Satet
    "mummy"         -> Just Mummy
    "apep"          -> Just Apep
    "giant-scorpion" -> Just GiantScorpion
    "androsphinx"   -> Just Androsphinx
    _               -> Nothing

-- JSON instances
instance JS.ToJSON GuardianType where
  toJSON = JS.String . guardianTypeText

instance JS.ToJSON GuardianSize where
  toJSON sz =
    JS.String
      (case sz of
         Small -> "small"
         Large -> "large"
      )

instance JS.FromJSON GuardianType where
  parseJSON = JS.withText "GuardianType" $ \txt ->
    case parseGuardianType txt of
      Just gt -> pure gt
      Nothing -> fail ("Unknown guardian type: " ++ show txt)

instance JS.ToJSON PlayerPieceType where
  toJSON x =
    case x of
      God -> JS.String "god"
      Soldier -> JS.String "soldier"
      Guardian gt -> JS.String ("guardian:" <> guardianTypeText gt)

instance JS.ToJSON StructureType where
  toJSON x =
    case x of
      Temple  -> JS.String "temple"
      Obelisk -> JS.String "obelisk"
      Pyramid -> JS.String "pyramid"

instance JS.FromJSON StructureType where
  parseJSON = JS.withText "StructureType" $ \txt ->
    case parseStructureType txt of
      Just st -> pure st
      Nothing -> fail ("Unknown structure type: " ++ show txt)

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
    _ -> case stripPrefix "guardian:" txt of
           Just gtTxt ->
             case parseGuardianType gtTxt of
               Just gt -> pure (Guardian gt)
               Nothing -> fail ("Unknown guardian type: " ++ show gtTxt)
           Nothing -> fail ("Unknown player piece type: " ++ show txt)

parseStructureType :: Text -> Maybe StructureType
parseStructureType txt =
  case txt of
    "temple" -> Just Temple
    "obelisk" -> Just Obelisk
    "pyramid" -> Just Pyramid
    _ -> Nothing
