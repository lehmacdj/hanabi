module Main where

import ConsoleIO
import ConsoleUI
import Data.Function ((&))
import Data.Random
import Game
import MyPrelude
import Polysemy.Error
import Polysemy.Output
import Refined
import System.Console.Haskeline
import ThreePlayer

printEndgameResult :: Member ConsoleIO r => GameState p -> Sem r ()
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
  forall p.
  ( Enum p,
    Bounded p,
    Show p,
    Ord p
  ) =>
  GameState p ->
  Sem
    [ Output (Turn p),
      PlayerIO p,
      Error GameOver,
      HasGameState p,
      Error CardDoesNotExist,
      ConsoleIO
    ]
    Void ->
  IO ()
playGame startingState game =
  game
    & ignoreOutput @(Turn p) . outputTurnToConsoleIO @p
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
  startingState <- sample startingState
  playGame @Player startingState (fullGameLoop @Player)
