module GameSpec where

import MyPrelude
import Game

import Test.Tasty.HUnit

unit_readShowAllCards =
  let allCards = [minBound .. maxBound :: Card]
   in map Just allCards @=? map (readMay . show) allCards
