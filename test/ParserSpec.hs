{-# OPTIONS_GHC -Wno-missing-signatures #-}

module ParserSpec where

import ConsoleUI
import Game
import MyPrelude
import Player
import Test.Tasty.HUnit
import Text.Megaparsec
import Text.Megaparsec.Error (errorBundlePretty)

failsToParse :: Show a => Parser a -> String -> Assertion
failsToParse p s = case parse (p <* eof) "<testcase>" s of
  Right x ->
    assertFailure $
      "parsed:\n"
        ++ show x
        ++ "\n"
        ++ "but expected parse error"
  Left _ -> pure ()

parserAssertion :: (Show a, Eq a) => Parser a -> String -> a -> Assertion
parserAssertion p s e = case parse (p <* eof) "<testcase>" s of
  Right x ->
    e @?= x
  Left e ->
    assertFailure $
      "got parse error:\n"
        ++ errorBundlePretty e

commandParsesTo :: String -> Command -> Assertion
commandParsesTo = parserAssertion pCommand

p0, p1, p2 :: Player
[p0, p1, p2] = simplePlayer <$> ["P0", "P1", "P2"]

unit_commandQuit = ":q" `commandParsesTo` Quit

unit_commandHintNumber =
  "P0 hint P1 [0, 2, 3] are number 3"
    `commandParsesTo` TakeTurn (RawTurn p0 (RawHint p1 (AreNumber (toEnum 3) (setFromList [0, 2, 3]))))

unit_commandHintColor =
  "P0 hint P1 [0, 2, 3] are color R"
    `commandParsesTo` TakeTurn (RawTurn p0 (RawHint p1 (AreColor Red (setFromList [0, 2, 3]))))

unit_commandPlay =
  "P1 play 3"
    `commandParsesTo` TakeTurn (RawTurn p1 (RawPlay 3))

unit_commandDiscard =
  "P2 discard 4"
    `commandParsesTo` TakeTurn (RawTurn p2 (RawDiscard 4))

unit_numberOutOfBounds0 = failsToParse pNumber "0"

unit_numberOutOfBounds6 = failsToParse pNumber "6"
