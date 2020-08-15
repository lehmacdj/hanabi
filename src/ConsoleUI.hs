module ConsoleUI where

import ConsoleIO
import Game
import MyPrelude hiding (bracket, some, try)
import Player
import Polysemy.Error
import Polysemy.Input
import Polysemy.Output
import Refined
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data PError
  = OutOfBoundsNumber Int
  | Other
  deriving (Show, Eq, Ord)

instance ShowErrorComponent PError where
  showErrorComponent = show

-- basic setup for Megaparsec lexeme parsing
type Parser = Parsec PError String

s :: Parser ()
s = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme s

symbol :: String -> Parser String
symbol = L.symbol s

bracket :: Parser a -> Parser a
bracket = between (symbol "[") (symbol "]")

outputTurnToConsoleIO ::
  Members [ConsoleIO, Output Turn] r =>
  Sem r a ->
  Sem r a
outputTurnToConsoleIO = intercept @(Output Turn) $ \case
  Output (Turn p action) -> writeln ("<everyone> " ++ show p ++ " did " ++ show action)

outputPrivateInfoToConsoleIO ::
  Members [ConsoleIO, Output (Player', Information)] r =>
  Sem r a ->
  Sem r a
outputPrivateInfoToConsoleIO = intercept @(Output (Player', Information)) $ \case
  Output (p, info) -> writeln ("<" ++ show p ++ "> " ++ show info)

data Command
  = Quit
  | TakeTurn RawTurn
  deriving (Show, Eq)

playerChar :: Parser Char
playerChar = oneOf $ ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9']

pPlayer :: Parser Player
pPlayer = lexeme (simplePlayer <$> some playerChar)

pCardIx :: Parser CardIx
pCardIx = lexeme L.decimal

pCardIxSet :: Parser (Set CardIx)
pCardIxSet = lexeme . bracket $ setFromList <$> pCardIx `sepBy` symbol ","

pColorNL :: Parser Color
pColorNL =
  choice
    [ symbol "R" $> Red,
      symbol "B" $> Blue,
      symbol "G" $> Green,
      symbol "Y" $> Yellow,
      symbol "W" $> White
    ]

pColor :: Parser Color
pColor = lexeme pColorNL

pNumberNL :: Parser Number
pNumberNL =
  fmap Number $
    L.decimal >>= \n ->
      maybe (customFailure (OutOfBoundsNumber n)) pure $ refineThrow n

pNumber :: Parser Number
pNumber = lexeme pNumberNL

pHint :: Parser Hint
pHint =
  lexeme . choice $
    [ try $ flip AreColor <$> (pCardIxSet <* symbol "are color") <*> pColor,
      flip AreNumber <$> (pCardIxSet <* symbol "are number") <*> pNumber
    ]

pAction :: Parser RawAction
pAction =
  lexeme . choice $
    [ symbol "play" *> (RawPlay <$> pCardIx),
      symbol "discard" *> (RawDiscard <$> pCardIx),
      symbol "hint" *> (RawHint <$> pPlayer <*> pHint)
    ]

pCommand :: Parser Command
pCommand =
  lexeme . choice $
    [ symbol ":q" $> Quit,
      fmap TakeTurn $ RawTurn <$> pPlayer <*> pAction
    ]

getInputCommandFromConsoleIO ::
  forall r a.
  Member ConsoleIO r =>
  Sem (Input Command : r) a ->
  Sem r a
getInputCommandFromConsoleIO = runInputSem go
  where
    go :: Sem r Command
    go = do
      inputLine <- untilJust getInputLine
      case parse (pCommand <* eof) "<interactive>" inputLine of
        Left e -> writeln (errorBundlePretty e) >> go
        Right command -> pure command

data QuitGame = QuitGame

-- | interpreting a command returns the turn that the command produces if it
-- is one and otherwise executes the command in the effect context
interpretCommand ::
  Throws '[QuitGame] r =>
  Command ->
  Sem r RawTurn
interpretCommand = \case
  Quit -> throw QuitGame
  TakeTurn t -> pure t

runPlayerIOAsConsoleUI ::
  Members [ConsoleIO, Error QuitGame] r =>
  Sem (PlayerIO : r) a ->
  Sem r a
runPlayerIOAsConsoleUI =
  subsume
    . subsume
    . ignoreOutput @(Player', Information)
    . outputPrivateInfoToConsoleIO
    . getInputCommandFromConsoleIO
    . contramapInput interpretCommand
    -- effect for interpret command to interpret in terms of
    . raiseUnder @(Input Command)
    . runPlayerIOToInputOutput
    -- effects for runPlayerIOToInputOutput to interpret in terms of
    . raiseUnder @(Input RawTurn)
    . raiseUnder @(Output (Player', Information))
    -- final effects we want to interpret in terms of
    . raiseUnder @(Error QuitGame)
    . raiseUnder @ConsoleIO
