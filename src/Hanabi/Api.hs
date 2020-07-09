module Hanabi.Api where

import Game
import HGID
import MyPrelude
import Player
import Polysemy.Input
import Servant.API
import Servant.API.Generic
import Servant.API.WebSocketConduit
import Servant.Server
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

createGuestUser ::
  Members [RandomFu, KVStore UUID Player] r =>
  String ->
  Sem r Player
createGuestUser name = do
  player <- sampleRVar (randomGuestPlayer name)
  writeKV (identifier player) player
  pure player

-- TODO: use AsServerT (Sem r) for some r
hanabiApi :: HanabiApi AsServer
hanabiApi =
  HanabiApi
    { createGuest = undefined,
      gameApi = \p gid -> toServant (hanabiGameApi p gid)
    }

hanabiGameApi :: Player -> HGID -> HanabiGameApi AsServer
hanabiGameApi player gameId =
  HanabiGameApi
    { join = undefined,
      connect = undefined,
      act = undefined
    }
