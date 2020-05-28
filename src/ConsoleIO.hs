{-# LANGUAGE TemplateHaskell #-}

module ConsoleIO where

import MyPrelude

import System.Console.Haskeline
import Control.Monad.Catch

data ConsoleIO m a where
  GetLine :: ConsoleIO m (Maybe String)
  Write :: String -> ConsoleIO m ()
makeSem ''ConsoleIO

writeln :: Member ConsoleIO r => String -> Sem r ()
writeln s = write s >> write "\n"

runConsoleIOAsInputT
  :: forall m a r.
    ( Member (Embed (InputT m)) r
    , MonadIO m
    , MonadException m
    , MonadMask m
    )
  => String -- ^ a prompt to show when getting a line
  -> Sem (ConsoleIO : r) a
  -> Sem r a
runConsoleIOAsInputT prompt = interpret $ \case
  GetLine -> embed @(InputT m) (getInputLine prompt)
  Write s -> embed @(InputT m) (outputStr s)
