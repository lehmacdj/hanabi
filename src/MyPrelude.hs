module MyPrelude
    ( module ClassyPrelude
    , module Control.Lens
    , overM
    ) where

import ClassyPrelude hiding (catchIO)
import Control.Lens hiding (snoc, Index, (<.>), (<|), index, uncons, unsnoc, cons)

-- | mapM but for an arbitrary lens not for monads
overM :: forall m s t a b. Monad m => ALens s t a b -> (a -> m b) -> s -> m t
overM l f d = f (d ^# l) >>= \x -> pure (storing l x d)
