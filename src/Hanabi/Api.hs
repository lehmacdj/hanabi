module Hanabi.Api where

import Data.Random
import Data.UUID
import Game
import HGID
import MyPrelude
import Player
import Polysemy.Error
import Polysemy.KVStore
import Polysemy.RandomFu
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
  deriving (Generic)

data HanabiGameApi route = HanabiGameApi
  { join ::
      route
        :- Summary
             "Attempts to join the game. \
             \Responds with 400 bad request if the game can't be joined."
        :> "players"
        :> "join"
        :> PostNoContent '[JSON] NoContent,
    -- | maybe worth trying to move this out of Hanabi game context so it
    -- doesn't require authentication
    playersGet ::
      route
        :- Summary "Gets the players in a game."
        :> "players"
        :> Get '[JSON] [Player],
    start ::
      route
        :- Summary
             "Attempts to start the game. \
             \Responds with 400 bad request if there is an invalid player count \
             \or if the game can't be started in its current state."
        :> "start"
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
  Members
    [ RandomFu,
      KVStore UUID Player,
      KVStore HGID LobbyState,
      Error ServerError
    ]
    r =>
  HanabiApi (AsServerT (Sem r))
hanabiApi =
  HanabiApi
    { createGuest = createGuestUser,
      gameApi = \p gid -> toServant (hanabiGameApi p gid)
    }

data LobbyState
  = Starting [Player]
  | -- | current game state + whose turn it is
    -- TODO: migrate to a model where we spin up a thread running the game
    -- and interact with that thread via active connections + a state that
    -- is only written to by the game process but can be read externally
    -- as well, we need to implement Input in terms of read operations on a
    -- channel that we write to when receiving a /<gameid>/act request and
    -- Output as write operations to websockets which we store in an array of
    -- all of the listeners, additionally we want to log all of the actions in
    -- the game, so when the game is complete we can have a log of what
    -- happened over the course of the game
    -- so:
    -- - choose what state we need to track for in progress games
    -- - logging actions taken
    -- - input channel for /act endpoint
    -- - output to websocket endpoints
    -- - interpreter that allows other threads to also read game state
    InProgress Player GameState
  | -- | TODO: add some extra details, i.e. log of game
    Completed GameState

badGame :: HGID -> ServerError
badGame hgid = err400 {errBody = "Couldn't find game: " <> blshow hgid}

joinGame ::
  Members
    [ KVStore UUID Player,
      KVStore HGID LobbyState,
      Error ServerError
    ]
    r =>
  Player ->
  HGID ->
  Sem r NoContent
joinGame player hgid = do
  -- @concurrency: probably worth throwing a lock around this eventually
  -- to prevent a race condition where two players join at the same time
  lobbyState <- lookupOrThrowKV badGame hgid
  case lobbyState of
    -- TODO: add logic to check that number of players is valid
    Starting players -> writeKV hgid (Starting (player : players)) $> NoContent
    _ -> throw (gameAlreadyStartedCannot "join")

getPlayers ::
  Members [KVStore HGID LobbyState, Error ServerError] r =>
  HGID ->
  Sem r [Player]
getPlayers hgid = playersFromState <$> lookupOrThrowKV badGame hgid
  where
    playersFromState = \case
      Starting players -> players
      InProgress _ state -> view #players state
      Completed state -> view #players state

gameAlreadyStartedCannot :: LByteString -> ServerError
gameAlreadyStartedCannot action =
  err400 {errBody = "Game has already started. Can't " <> action <> " game."}

badPlayerCount :: Int -> ServerError
badPlayerCount playerCount =
  err400
    { errBody =
        "Bad player count: "
          <> blshow playerCount
          <> ". The number of players must be between 3 and 5."
    }

startGame ::
  forall r.
  Members [RandomFu, KVStore HGID LobbyState, Error ServerError] r =>
  HGID ->
  Sem r NoContent
startGame hgid = doStart <$> lookupOrThrowKV badGame hgid $> NoContent
  where
    doStart :: LobbyState -> Sem r ()
    doStart = \case
      Starting players -> do
        let playerCount = length players
            validPlayerCount = playerCount <= 5 && playerCount >= 2
        unless validPlayerCount $ throw (badPlayerCount playerCount)
        players <- sampleRVar (shuffle players)
        let firstPlayer = headEx players
        startingState <- sampleRVar (startingState players)
        writeKV hgid (InProgress firstPlayer startingState)
      _ -> throw (gameAlreadyStartedCannot "start")

-- subscribeToState ::
--   Members [RandomFu, KVStore HGID LobbyState, Error ServerError] r =>
--   Player ->
--   HGID ->
--   _
-- subscribeToState = undefined

hanabiGameApi ::
  Members
    [ RandomFu,
      KVStore UUID Player,
      KVStore HGID LobbyState,
      Error ServerError
    ]
    r =>
  Player ->
  HGID ->
  HanabiGameApi (AsServerT (Sem r))
hanabiGameApi player gameId =
  HanabiGameApi
    { join = joinGame player gameId,
      playersGet = getPlayers gameId,
      start = startGame gameId,
      connect = undefined,
      act = undefined
    }
