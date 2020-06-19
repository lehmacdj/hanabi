module MyPrelude
  ( module ClassyPrelude,
    module Control.Lens,
    module Data.Void,
    module Polysemy,
    module Fcf.Core,
    type (++),
    HasCallStack,
    Throws,
    justOrThrow,
    deleteAt,
    isPermutationOf,
    next,
    untilJust,
    whenJust,
    setExcept,
    setAny,
    codiagonal,
    runError',
    raiseDeepUnder,
    raiseDeepUnder2,
    contramapInput,
    maybeToRight,
    rightToMaybe,
  )
where

import ClassyPrelude hiding (catch, catchIO)
import Control.Lens hiding (Index, cons, index, rewrite, snoc, transform, uncons, unsnoc, (<.>), (<|))
import Data.Void
import Fcf.Core
import Fcf.Data.List (type (++))
import GHC.Stack (HasCallStack)
import Polysemy
import Polysemy.Error (Error, runError, throw)
import Polysemy.Input (Input, input, runInputSem)
import Polysemy.Internal
import Polysemy.Internal.Union

type family TMap (f :: k -> k') (xs :: [k]) :: [k'] where
  TMap f '[] = '[]
  TMap f (x : xs) = f x : TMap f xs

-- | for throwing multiple errors together
type Throws errs r = Members (TMap Error errs) r

deleteAt :: HasCallStack => Int -> [a] -> [a]
deleteAt 0 (_ : xs) = xs
deleteAt n (x : xs) | n >= 0 = x : deleteAt (n -1) xs
deleteAt _ _ = error "index out of range"

justOrThrow :: Throws '[err] r => err -> Maybe a -> Sem r a
justOrThrow err = \case
  Just x -> pure x
  Nothing -> throw err

isPermutationOf :: Ord a => [a] -> [a] -> Bool
isPermutationOf xs ys = sort xs == sort ys

-- | succ but wraps around if the element is maxBound
next :: (Eq a, Enum a, Bounded a) => a -> a
next x
  | x == maxBound = minBound
  | otherwise = succ x

-- | Run the supplied "Maybe" computation repeatedly until it returns a
-- value.  Returns that value.
-- taken from: monad-loops
untilJust :: Monad m => m (Maybe a) -> m a
untilJust m = go
  where
    go = do
      x <- m
      maybe go pure x

-- | taken from: extra
whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust mg f = maybe (pure ()) f mg

setExcept :: (Enum a, Bounded a, Ord a) => a -> Set a
setExcept x = setAny `difference` singleton x

setAny :: (Enum a, Bounded a, Ord a) => Set a
setAny = setFromList [minBound .. maxBound]

codiagonal :: Either a a -> a
codiagonal (Left x) = x
codiagonal (Right x) = x

runError' :: forall e r. Sem (Error e : r) Void -> Sem r e
runError' = fmap (either id absurd) . runError

raiseDeepUnder :: forall e3 e1 e2 r a. Sem (e1 : e2 : r) a -> Sem (e1 : e2 : e3 : r) a
raiseDeepUnder = hoistSem $ hoist raiseDeepUnder . weakenDeepUnder
  where
    weakenDeepUnder :: forall m x. Union (e1 : e2 : r) m x -> Union (e1 : e2 : e3 : r) m x
    weakenDeepUnder (Union SZ a) = Union SZ a
    weakenDeepUnder (Union (SS SZ) a) = Union (SS SZ) a
    weakenDeepUnder (Union (SS (SS n)) a) = Union (SS (SS (SS n))) a

raiseDeepUnder2 :: forall e3 e4 e1 e2 r a. Sem (e1 : e2 : r) a -> Sem (e1 : e2 : e3 : e4 : r) a
raiseDeepUnder2 = hoistSem $ hoist raiseDeepUnder2 . weakenDeepUnder2
  where
    weakenDeepUnder2 :: forall m x. Union (e1 : e2 : r) m x -> Union (e1 : e2 : e3 : e4 : r) m x
    weakenDeepUnder2 (Union SZ a) = Union SZ a
    weakenDeepUnder2 (Union (SS SZ) a) = Union (SS SZ) a
    weakenDeepUnder2 (Union (SS (SS n)) a) = Union (SS (SS (SS (SS n)))) a

-- | Input effects may be mapped contravariantly
contramapInput ::
  forall x y r a.
  ( Member (Input y) r
  ) =>
  (y -> Sem r x) ->
  Sem (Input x : r) a ->
  Sem r a
contramapInput f = runInputSem $ input @y >>= f

-- | credit to either/relude package for the name
maybeToRight :: e -> Maybe a -> Either e a
maybeToRight e Nothing = Left e
maybeToRight _ (Just x) = Right x

rightToMaybe :: Either e a -> Maybe a
rightToMaybe (Right x) = Just x
rightToMaybe _ = Nothing
