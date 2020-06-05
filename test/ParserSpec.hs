{-# OPTIONS_GHC -Wno-missing-signatures #-}

module ParserSpec where

import MyPrelude
import Test.Tasty.HUnit
import Text.Megaparsec
import Text.Megaparsec.Error (errorBundlePretty)

import Game
import ConsoleUI
import ThreePlayer

failsToParse :: Show a => Parser a -> String -> Assertion
failsToParse p s = case parse (p <* eof) "<testcase>" s of
  Right x -> assertFailure $
    "parsed:\n"
    ++ show x ++ "\n"
    ++ "but expected parse error"
  Left _ -> pure ()

parserAssertion :: (Show a, Eq a) => Parser a -> String -> a -> Assertion
parserAssertion p s e = case parse (p <* eof) "<testcase>" s of
  Right x ->
    e @?= x
  Left e -> assertFailure $
    "got parse error:\n"
    ++ errorBundlePretty e

commandParsesTo :: String -> Command Player -> Assertion
commandParsesTo = parserAssertion pCommand

unit_commandQuit = ":q" `commandParsesTo` Quit

unit_commandHintNumber =
  "P0 hint P1 [0, 2, 3] are number 3"
  `commandParsesTo`
  TakeTurn (Turn P0 (Hint P1 (AreNumber (toEnum 3) (setFromList [0, 2, 3]))))

unit_commandHintColor =
  "P0 hint P1 [0, 2, 3] are color R"
  `commandParsesTo`
  TakeTurn (Turn P0 (Hint P1 (AreColor Red (setFromList [0, 2, 3]))))

unit_commandPlay =
  "P1 play 3"
  `commandParsesTo`
  TakeTurn (Turn P1 (Play 3))

unit_commandDiscard =
  "P2 play 4"
  `commandParsesTo`
  TakeTurn (Turn P2 (Play 4))

unit_numberOutOfBounds0 = failsToParse pNumber "0"
unit_numberOutOfBounds6 = failsToParse pNumber "6"
