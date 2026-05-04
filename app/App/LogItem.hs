module App.LogItem where

import Data.Text (Text)
import Data.Aeson qualified as JS
import KOI.Basics (PlayerId)
import App.Cards (Card)
import App.Piece (StructureType)
import App.Board (RegionId)

data LogWord
  = LogText Text
  | LogPlayer PlayerId
  | LogCard Card
  | LogFollowers Int
  | LogDevotion Int
  | LogStructure StructureType
  | LogRegion RegionId
  deriving (Read, Show)

instance JS.ToJSON LogWord where
  toJSON (LogText txt) = JS.object
    [ "tag" JS..= ("text" :: Text)
    , "contents" JS..= txt
    ]
  toJSON (LogPlayer pid) = JS.object
    [ "tag" JS..= ("player" :: Text)
    , "contents" JS..= pid
    ]
  toJSON (LogCard card) = JS.object
    [ "tag" JS..= ("card" :: Text)
    , "contents" JS..= card
    ]
  toJSON (LogFollowers n) = JS.object
    [ "tag" JS..= ("followers" :: Text)
    , "contents" JS..= n
    ]
  toJSON (LogDevotion n) = JS.object
    [ "tag" JS..= ("devotion" :: Text)
    , "contents" JS..= n
    ]
  toJSON (LogStructure stype) = JS.object
    [ "tag" JS..= ("structure" :: Text)
    , "contents" JS..= stype
    ]
  toJSON (LogRegion rid) = JS.object
    [ "tag" JS..= ("region" :: Text)
    , "contents" JS..= rid
    ]

data LogItem
  = LogEntry [LogWord]
  | LogGroup [LogItem]
  deriving (Read, Show)

instance JS.ToJSON LogItem where
  toJSON (LogEntry ws) = JS.object
    [ "tag" JS..= ("entry" :: Text)
    , "contents" JS..= ws
    ]
  toJSON (LogGroup items) = JS.object
    [ "tag" JS..= ("group" :: Text)
    , "contents" JS..= reverse items  -- Groups are stored reversed, reverse for JSON
    ]
