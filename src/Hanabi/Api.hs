module Hanabi.Api where

import Control.Concurrent (ThreadId, forkIO)
import Data.Aeson
import Data.Random
import Data.UUID
import Game
import HGID
import MyPrelude
import qualified Network.WebSockets as WebSocket
import Player
import Polysemy.Error
import Polysemy.Input
import Polysemy.KVStore
import Polysemy.Output
import Polysemy.State
import Servant.API
import Servant.API.Generic
import Servant.API.WebSocket
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
        :> WebSocket,
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
  Members [Embed IO, KVStore UUID Player] r =>
  String ->
  Sem r Player
createGuestUser name = do
  player <- embed @IO $ sample (randomGuestPlayer name)
  writeKV (identifier player) player
  pure player

hanabiApi ::
  Members
    [ Embed IO,
      KVStore UUID Player,
      KVStore HGID LobbyState,
      Error ServerError,
      Input (Chan GError)
    ]
    r =>
  HanabiApi (AsServerT (Sem r))
hanabiApi =
  HanabiApi
    { createGuest = createGuestUser,
      gameApi = \p gid -> toServant (hanabiGameApi p gid)
    }

-- | State needed for every connection from a client subscribing to updates
data StateSubscriber = StateSubscriber
  { stateSubscriberThread :: ThreadId,
    -- | Channel for information to pass to the player
    infoChan :: Chan Information
  }
  deriving (Generic)

data InProgressState = InProgressState
  { gameState :: IORef GameState,
    actionInput :: Chan RawTurn,
    gameThread :: ThreadId
  }
  deriving (Generic)

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
    InProgress InProgressState
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
  Members [KVStore HGID LobbyState, Error ServerError, Embed IO] r =>
  HGID ->
  Sem r [Player]
getPlayers hgid = do
  lobbyState <- lookupOrThrowKV badGame hgid
  case lobbyState of
    Starting players -> pure players
    InProgress state ->
      embed @IO
        . fmap (view #players)
        . readIORef
        $ view #gameState state
    Completed state -> pure $ view #players state

gameAlreadyStartedCannot :: LByteString -> ServerError
gameAlreadyStartedCannot action =
  err400 {errBody = "Game has already started. Can't " <> action <> " game."}

gameMustBeInProgressTo :: LByteString -> ServerError
gameMustBeInProgressTo action =
  err400 {errBody = "Game must be in progress to " <> action <> "game."}

badPlayerCount :: Int -> ServerError
badPlayerCount playerCount =
  err400
    { errBody =
        "Bad player count: "
          <> blshow playerCount
          <> ". The number of players must be between 3 and 5."
    }

data GError
  = InvalidCard HGID
  | GError HGID String

hgidOfGError :: Lens' GError HGID
hgidOfGError = lens get (flip set)
  where
    get = \case
      InvalidCard hgid -> hgid
      GError hgid _ -> hgid
    set hgid = \case
      InvalidCard _ -> InvalidCard hgid
      GError _ errMsg -> GError hgid errMsg

-- | TODO: add global logging for stuff like turns being taken etc.
-- two approaches:
-- 1. add record of game to game state itself
-- 2. add a global logger that absorbs this info and writes to console or
--    something in the cloud
runGameThread ::
  HGID ->
  -- | The channel for exceptions that failed
  Chan GError ->
  -- | The GameState to be accessed atomically
  IORef GameState ->
  -- | Channel for providing RawAction to this game from player input
  Chan RawTurn ->
  Sem
    [ Output Turn,
      PlayerIO,
      State GameState,
      Error CardDoesNotExist,
      Embed IO
    ]
    () ->
  IO ()
runGameThread hgid errChan stateIORef turnChan game =
  game
    & ignoreOutput @Turn
    & raiseUnder @(Input RawTurn)
    & raiseUnder @(Output (Player', Information))
    & runPlayerIOToInputOutput
    & ignoreOutput @(Player', Information)
    & runInputChan turnChan
    & runStateIORef stateIORef
    & raiseUnder @(Error GError)
    & runErrorChan errChan . mapError (\_ -> InvalidCard hgid)
    & runM

startGame ::
  forall r.
  Members
    [ Embed IO,
      Input (Chan GError),
      KVStore HGID LobbyState,
      Error ServerError
    ]
    r =>
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
        startingState <- embed @IO $ sample (startingState players)
        stateIORef <- embed @IO $ newIORef startingState
        turnChan <- newChan
        gerrorChan <- input @(Chan GError)
        threadId <-
          embed . forkIO $
            runGameThread hgid gerrorChan stateIORef turnChan fullGameLoop
        let inProgressState = InProgressState stateIORef turnChan threadId
        writeKV hgid (InProgress inProgressState)
      -- TODO: possibly allow starting game from completed game, i.e. with same
      -- players
      _ -> throw (gameAlreadyStartedCannot "start")

writeWebSocket :: ToJSON a => WebSocket.Connection -> a -> IO ()
writeWebSocket connection value =
  WebSocket.sendDataMessage connection (WebSocket.Text (encode value) Nothing)

-- | The thread that sends information to the client on reading it from its
-- channel
stateSender :: Chan Information -> WebSocket.Connection -> IO ()
stateSender infoChan connection = do
  info <- readChan infoChan
  writeWebSocket connection info
  stateSender infoChan connection

subscribeToState ::
  Members [Embed IO, KVStore HGID LobbyState, Error ServerError] r =>
  Player ->
  HGID ->
  WebSocket.Connection ->
  Sem r ()
subscribeToState = undefined

doAct ::
  forall r.
  Members
    [ Embed IO,
      KVStore UUID Player,
      KVStore HGID LobbyState,
      Error ServerError,
      Input (Chan GError)
    ]
    r =>
  Player ->
  HGID ->
  RawAction ->
  Sem r NoContent
doAct player hgid action = inputAction <$> lookupOrThrowKV badGame hgid $> NoContent
  where
    inputAction :: LobbyState -> Sem r ()
    inputAction = \case
      InProgress state ->
        writeChan
          (view #actionInput state)
          (RawTurn player action)
      _ -> throw (gameMustBeInProgressTo "perform an action")

hanabiGameApi ::
  Members
    [ Embed IO,
      KVStore UUID Player,
      KVStore HGID LobbyState,
      Error ServerError,
      Input (Chan GError)
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
      act = doAct player gameId
    }
