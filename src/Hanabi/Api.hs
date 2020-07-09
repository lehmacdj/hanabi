module Hanabi.Api where

import Data.UUID
import Game
import HGID
import MyPrelude
import Player
import Polysemy.KVStore
import Polysemy.RandomFu
import Servant.API
import Servant.API.Generic
import Servant.API.WebSocketConduit
import Servant.Server.Generic

data HanabiApi route = HanabiApi
  { createGuest ::
      route
        :- Summary "creates a guest account for a session of hanabi"
        :> "guest"
        :> ReqBody '[JSON] String
        :> Put '[JSON] Player,
    gameApi ::
      route
        :- BasicAuth "Hanabi Game" Player
        :> "game"
        :> Capture "game-id" HGID
        :> ToServant HanabiGameApi AsApi
  }
  deriving (Generic)

data HanabiGameApi route = HanabiGameApi
  { join ::
      route
        :- Summary
             "Attempts to join the game. \
             \Responds with 400 bad request if the game can't be joined."
        :> "join"
        :> PostNoContent '[JSON] NoContent,
    -- state ::
    --   route
    --     :- Summary "Get the current state, as your player can see."
    --     :> "state"
    --     :> Get '[JSON] GameState,
    connect ::
      route
        :- Summary
             "creates a websocket that receives state updates"
        :> "state"
        :> "subscribe"
        :> WebSocketSource Information,
    act ::
      route
        :- Summary "perform an action in the hanabi game"
        :> "act"
        :> ReqBody '[JSON] RawAction
        :> PostNoContent '[JSON] NoContent
  }
  deriving (Generic)

-- | TODO: think about how I want to handle users, do I want players to be
-- intrinsically tied to games or not. Probably best to continue operating with
-- the current design at least until I finish implementing this API because I
-- will have a better idea of what my goals should be then I think
createGuestUser ::
  Members [RandomFu, KVStore UUID Player] r =>
  String ->
  Sem r Player
createGuestUser name = do
  player <- sampleRVar (randomGuestPlayer name)
  writeKV (identifier player) player
  pure player

hanabiApi ::
  Members [RandomFu, KVStore UUID Player, KVStore HGID GameState] r =>
  HanabiApi (AsServerT (Sem r))
hanabiApi =
  HanabiApi
    { createGuest = createGuestUser,
      gameApi = \p gid -> toServant (hanabiGameApi p gid)
    }

hanabiGameApi ::
  Members [RandomFu, KVStore UUID Player, KVStore HGID GameState] r =>
  Player ->
  HGID ->
  HanabiGameApi (AsServerT (Sem r))
hanabiGameApi player gameId =
  HanabiGameApi
    { join = undefined,
      connect = undefined,
      act = undefined
    }
