module Main where

import ConsoleIO
import ConsoleUI
import Data.Function ((&))
import Data.Random
import Game
import MyPrelude
import Player
import Polysemy.Error
import Polysemy.Output
import Polysemy.State
import Refined
import System.Console.Haskeline

printEndgameResult :: Member ConsoleIO r => GameState -> Sem r ()
printEndgameResult state = do
  writeln "The game has ended!"
  let totalScore =
        sumOf
          (#board . #fireworks . #underlyingMap . traverse . to unrefine)
          state
  writeln $ "Your total score is: " ++ show totalScore
  writeln "And you played cards:"
  for_ (state ^@.. #board . #fireworks . #underlyingMap . itraversed) $
    \(c, n) -> writeln $ show c ++ " => " ++ show (unrefine n)

handleIndexError ::
  Member ConsoleIO r => Sem (Error CardDoesNotExist : r) () -> Sem r ()
handleIndexError = (>>= either (const writeErrorMessage) pure) . runError
  where
    writeErrorMessage = writeln "error: unrecoverable index out of bounds error"

playGame ::
  GameState ->
  Sem
    [ Output Turn,
      PlayerIO,
      Error GameOver,
      State GameState,
      Error CardDoesNotExist,
      ConsoleIO
    ]
    Void ->
  IO ()
playGame startingState game =
  game
    & ignoreOutput @Turn . outputTurnToConsoleIO
    & runPlayerIOAsConsoleUI
    & runGame startingState
    & (=<<) printEndgameResult
    & handleIndexError
    & raiseUnder @(Embed (InputT IO))
    & runConsoleIOAsInputT @IO "hanabi> "
    & runM
    & runInputT defaultSettings

main :: IO ()
main = do
  let players = simplePlayer <$> ["P0", "P1", "P2"]
  startingState <- sample (startingState players)
  playGame startingState fullGameLoop
