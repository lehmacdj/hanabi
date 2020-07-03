module Hanabi.Api where

import Game
import HGID
import MyPrelude
import Player
import Servant.API
import Servant.API.Generic
import Servant.API.WebSocketConduit

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
        :> Capture "game-id" HGID
        :> ToServant HanabiGameApi route
  }

data HanabiGameApi route = HanabiGameApi
  { join ::
      route
        :- Summary
             "Attempts to join the game. \
             \Responds with 400 bad request if the game can't be joined."
        :> "join"
        :> PostNoContent '[JSON] NoContent,
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
