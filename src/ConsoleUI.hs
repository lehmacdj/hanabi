module ConsoleUI where

import ConsoleIO
import Game
import MyPrelude hiding (bracket, try)
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
  forall p r a.
  (Show p, Members [ConsoleIO, Output (Turn p)] r) =>
  Sem r a ->
  Sem r a
outputTurnToConsoleIO = intercept @(Output (Turn p)) $ \case
  Output (Turn p action) -> writeln ("<everyone> " ++ show p ++ " did " ++ show action)

outputPrivateInfoToConsoleIO ::
  forall p r a.
  (Show p, Members [ConsoleIO, Output (p, Information p)] r) =>
  Sem r a ->
  Sem r a
outputPrivateInfoToConsoleIO = intercept @(Output (p, Information p)) $ \case
  Output (p, info) -> writeln ("<" ++ show p ++ "> " ++ show info)

data Command p
  = Quit
  | TakeTurn (Turn p)
  deriving (Show, Eq)

pPlayer :: (Show p, Enum p, Bounded p) => Parser p
pPlayer =
  lexeme . choice $
    fmap (\p -> symbol (show p) $> p) [minBound .. maxBound]

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

pAction :: (Show p, Enum p, Bounded p) => Parser (Action p)
pAction =
  lexeme . choice $
    [ symbol "play" *> (Play <$> pCardIx),
      symbol "discard" *> (Discard <$> pCardIx),
      symbol "hint" *> (Hint <$> pPlayer <*> pHint)
    ]

pCommand :: (Show p, Enum p, Bounded p) => Parser (Command p)
pCommand =
  lexeme . choice $
    [ symbol ":q" $> Quit,
      fmap TakeTurn $ Turn <$> pPlayer <*> pAction
    ]

getInputCommandFromConsoleIO ::
  forall p r a.
  ( Member ConsoleIO r,
    Show p,
    Enum p,
    Bounded p
  ) =>
  Sem (Input (Command p) : r) a ->
  Sem r a
getInputCommandFromConsoleIO = runInputSem go
  where
    go :: Sem r (Command p)
    go = do
      inputLine <- untilJust getInputLine
      case parse (pCommand <* eof) "<interactive>" inputLine of
        Left e -> writeln (errorBundlePretty e) >> go
        Right command -> pure command

-- | interpreting a command returns the turn that the command produces if it
-- is one and otherwise executes the command in the effect context
interpretCommand ::
  forall p r.
  ( Throws '[GameOver] r,
    Show p,
    Enum p,
    Bounded p
  ) =>
  Command p ->
  Sem r (Turn p)
interpretCommand = \case
  Quit -> throw GameOver
  TakeTurn t -> pure t

runPlayerIOAsConsoleUI ::
  forall p r a.
  ( Show p,
    Eq p,
    Enum p,
    Bounded p,
    Members [ConsoleIO, Error GameOver] r
  ) =>
  Sem (PlayerIO p : r) a ->
  Sem r a
runPlayerIOAsConsoleUI =
  subsume
    . subsume
    . ignoreOutput @(p, Information p)
    . outputPrivateInfoToConsoleIO @p
    . getInputCommandFromConsoleIO
    . contramapInput interpretCommand
    -- effect for interpret command to interpret in terms of
    . raiseUnder @(Input (Command p))
    . runPlayerIOToInputOutput @p
    -- effects for runPlayerIOToInputOutput to interpret in terms of
    . raiseUnder @(Input (Turn p))
    . raiseUnder @(Output (p, Information p))
    -- final effects we want to interpret in terms of
    . raiseUnder @(Error GameOver)
    . raiseUnder @ConsoleIO
