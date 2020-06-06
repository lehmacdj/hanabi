{-# OPTIONS_GHC -Wno-missing-signatures #-}

module GameSpec where

import MyPrelude
import Game

import Test.Tasty.HUnit

unit_readShowAllCards =
  let allCards = [minBound .. maxBound :: Card]
   in map Just allCards @=? map (readMay . show) allCards

unit_toFromEnumAllCards =
  let allCards = [minBound .. maxBound :: Card]
   in allCards @=? map (toEnum . fromEnum) allCards
