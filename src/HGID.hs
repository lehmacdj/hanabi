{-# LANGUAGE OverloadedStrings #-}

module HGID where

import MyPrelude
import Servant.API

-- | ID for a game of hanabi.
-- TODO: decide what the exact representation of this will be/implement method
-- for generating random ids
newtype HGID = HGID {unHGID :: String}
  deriving (Show, Read, Generic)

instance FromHttpApiData HGID where
  parseQueryParam = maybeToRight "couldn't parse HGID" . readMay

instance ToHttpApiData HGID where
  toQueryParam = tshow
