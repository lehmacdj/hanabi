-- | Implements a simple bot that plays by keeping track of all information
-- they have been given and making decisions based only on that information.
-- This bot could also be called egocentric bot, because it only cares about
-- itself.
module SimpleBot where

import Game
import MyPrelude

data BotInformation = BotInformation
  { player :: Player',
    handPossibilities :: HandPossibilities
  }
  deriving (Show, Generic)

updateHandPossibilities ::
  Player' -> Information -> HandPossibilities -> HandPossibilities
updateHandPossibilities p i hp
  | hasn't (aboutPlayer . only p) i = hp
  | otherwise = case i of
    RemovedFromHand _ cix -> mempty : deleteAt cix hp
    CardSatisfies _ cix ci -> over (ix cix) (<> ci) hp

updateBotInfo ::
  Information ->
  BotInformation ->
  BotInformation
updateBotInfo i bi =
  over #handPossibilities (updateHandPossibilities (view #player bi) i) bi
