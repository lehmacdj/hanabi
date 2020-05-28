-- | Implements a more complicated bot that interprets the hints to keep
-- track of the information that other players know about their own hand.
module ComplexBot where

import MyPrelude
import Game

-- | the information that a player can have about the game
data BotInformation p = BotInformation
  { player :: p
  , myHandPossibilities :: PlayerMap p HandPossibilities
  -- ^ what cards are in players hands based on all of the information at
  -- this bots disposal. In particular, for players other than this player,
  -- this is information to determine exactly what card other players are
  -- holding
  , hintedHandPossibilities :: PlayerMap p HandPossibilities
  -- ^ information that players know based solely on hints. This information
  -- is common knowledge so, it is possible to assume that every other
  -- player might know this information
  }
  deriving (Show, Generic)
