module HGID where

import MyPrelude

-- | ID for a game of hanabi.
-- TODO: decide what the exact representation of this will be/implement method
-- for generating random ids
newtype HGID = HGID {unHGID :: String}
  deriving (Show, Generic)
