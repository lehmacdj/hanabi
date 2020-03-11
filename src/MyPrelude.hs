module MyPrelude
    ( module ClassyPrelude
    , module Control.Lens
    , overM
    , mapOfFunction
    , module Polysemy
    ) where

import qualified Data.Map as Map

import ClassyPrelude hiding (catchIO)
import Control.Lens hiding (snoc, Index, (<.>), (<|), index, uncons, unsnoc, cons)

import Polysemy

-- | mapM but for an arbitrary lens not for monads
overM :: forall m s t a b. Monad m => ALens s t a b -> (a -> m b) -> s -> m t
overM l f d = f (d ^# l) >>= \x -> pure (storing l x d)

mapOfFunction :: Ord k => [k] -> (k -> v) -> Map k v
mapOfFunction ks f = Map.fromList (ks `zip` map f ks)
