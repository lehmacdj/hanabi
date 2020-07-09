module Player where

import Data.Random
import Data.UUID
import MyPrelude

-- | player for a game of Hanabi. The player is represented by a name and a
-- UUID. The UUID should be stored in a cookie on the client to allow rejoins
-- with the same name. Authentication is done via basic auth; to authenticate
-- the password used is "<HGID>/<UUID>", we check for this in our database of
-- currently running games to make sure that the player should be allowed to
-- make an action in the game that they are trying to make an action in
data Player = Player
  { name :: String,
    identifier :: UUID
  }
  deriving (Show, Generic, Eq, Ord)

simplePlayer :: String -> Player
simplePlayer n = Player n nil

randomUUID :: RVar UUID
randomUUID =
  fromWords <$> stdUniform <*> stdUniform <*> stdUniform <*> stdUniform

randomGuestPlayer :: String -> RVar Player
randomGuestPlayer name = Player name <$> randomUUID
