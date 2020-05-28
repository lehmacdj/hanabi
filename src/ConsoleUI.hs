module ConsoleUI where

import MyPrelude

import ConsoleIO
import Game

import Text.Read (reads)
import Text.ParserCombinators.ReadP

import Polysemy.Output

outputTurnToConsoleIO
  :: forall p r a. (Show p, Members [ConsoleIO, Output (Turn p)] r)
  => Sem r a -> Sem r a
outputTurnToConsoleIO = intercept @(Output (Turn p)) $ \case
  Output (Turn p action) -> writeln ("everyone> " ++ show p ++ " did " ++ show action)

outputPrivateInfoToConsoleIO
  :: forall p r a. (Show p, Members [ConsoleIO, Output (p, Information p)] r)
  => Sem r a -> Sem r a
outputPrivateInfoToConsoleIO = intercept @(Output (p, Information p)) $ \case
  Output (p, info) -> writeln (show p ++ "> " ++ show info)

data Command p
  = Quit
  | TakeTurn (Turn p)
  deriving (Show, Eq)

pRead :: Read a => ReadP a
pRead = readS_to_P reads

pCommand :: forall p. Read p => ReadP (Command p)
pCommand =
  ((string ":q" *> eof) $> Quit)
  <|> (TakeTurn <$> (Turn <$> (pRead :: ReadP p) <* munch1 (==' ') *> (pRead :: ReadP (Turn p)) <* eof :: ReadP (Turn p)))

-- instance Read Command

getInputCommandFromConsoleIO :: ()
getInputCommandFromConsoleIO = ()
