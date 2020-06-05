{-# LANGUAGE TemplateHaskell #-}

module ConsoleIO where

import MyPrelude

import qualified System.Console.Haskeline as H
import Control.Monad.Catch

data ConsoleIO m a where
  GetInputLine :: ConsoleIO m (Maybe String)
  Write :: String -> ConsoleIO m ()
makeSem ''ConsoleIO

writeln :: Member ConsoleIO r => String -> Sem r ()
writeln s = write s >> write "\n"

runConsoleIOAsInputT
  :: forall m a r.
    ( Member (Embed (H.InputT m)) r
    , MonadIO m
    , H.MonadException m
    , MonadMask m
    )
  => String -- ^ a prompt to show when getting a line
  -> Sem (ConsoleIO : r) a
  -> Sem r a
runConsoleIOAsInputT prompt = interpret $ \case
  GetInputLine -> embed @(H.InputT m) (H.getInputLine prompt)
  Write s -> embed @(H.InputT m) (H.outputStr s)
