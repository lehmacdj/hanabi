-- | Implements a simple bot that plays with generally good heuristics,
-- that deduced while playing hanabi myself(Devin)
module SimpleBot where

import MyPrelude
import Game
import ThreePlayer

type HandsInformation = PlayerMap HandPossibilities

-- | the information that a player can have about the game
data PlayerInformation = PlayerInformation
    { player :: Player
    , board :: Board
    , cardsInDeck :: Set Card
    , handPossibilities :: HandPossibilities
    }
    deriving (Show, Generic)
