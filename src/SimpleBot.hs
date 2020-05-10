-- | Implements a simple bot that plays with generally good heuristics,
-- that deduced while playing hanabi myself(Devin)
module SimpleBot where

import MyPrelude
import Game

-- | the information that a player can have about the game
data PlayerInformation p = PlayerInformation
  { handPossibilities :: HandPossibilities
  -- ^ the possibilities that this player thinks their hand could be
  , perspectiveMap :: PlayerMap p (PlayerInformation p)
  -- ^ the information this player thinks that other players know
  -- if the info is for player p then the following is an invariant
  -- > perspectiveMap . at p == id
  }
  deriving (Show, Generic)

updatePlayerInfo
  :: Ord p => Information p -> PlayerInformation p -> PlayerInformation p
updatePlayerInfo i pi = case i of
  RemovedFromHand p cix -> undefined
  CardSatisfies p cix ci -> undefined

data BotInformation p = BotInformation
  { player :: p
  , playerInformation :: PlayerInformation p
  }
  deriving (Show, Generic)

updateBotInfo
  :: Ord p => Information p -> BotInformation p -> BotInformation p
updateBotInfo i = over #playerInformation (updatePlayerInfo i)
