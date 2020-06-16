module ThreePlayer where

import MyPrelude

data Player = P0 | P1 | P2
  deriving (Show, Generic, Eq, Ord, Enum, Bounded)
