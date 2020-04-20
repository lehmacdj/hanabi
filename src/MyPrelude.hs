module MyPrelude
  ( module ClassyPrelude
  , module Control.Lens
  , module Data.Void
  , module Polysemy

  , HasCallStack

  , mapOfFunction
  , Throws
  , justOrThrow
  , deleteAt
  , isPermutationOf
  , next
  , untilJust
  ) where

import qualified Data.Map as Map

import ClassyPrelude hiding (catchIO)
import Control.Lens hiding (snoc, Index, (<.>), (<|), index, uncons, unsnoc, cons)

import Data.Void

import Polysemy
import Polysemy.Error (Error, throw)

import GHC.Stack (HasCallStack)

type family TMap (f :: k -> k') (xs :: [k]) :: [k'] where
  TMap f '[] = '[]
  TMap f (x : xs) = f x : TMap f xs

-- | for throwing multiple errors together
type Throws errs r = Members (TMap Error errs) r

deleteAt :: HasCallStack => Int -> [a] -> [a]
deleteAt 0 (_:xs) = xs
deleteAt n (x:xs) | n >= 0 = x : deleteAt (n-1) xs
deleteAt _ _ = error "index out of range"

justOrThrow :: Throws '[err] r => err -> Maybe a -> Sem r a
justOrThrow err = \case
  Just x -> pure x
  Nothing -> throw err

-- | Contructs a map from a function applied to each given key
mapOfFunction :: Ord k => [k] -> (k -> v) -> Map k v
mapOfFunction ks f = Map.fromList (ks `zip` map f ks)

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
