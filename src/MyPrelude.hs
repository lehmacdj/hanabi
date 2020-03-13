module MyPrelude
    ( module ClassyPrelude
    , module Control.Lens
    , module Polysemy

    , mapOfFunction
    , Throws
    , justOrThrow
    , deleteAt
    ) where

import qualified Data.Map as Map

import ClassyPrelude hiding (catchIO)
import Control.Lens hiding (snoc, Index, (<.>), (<|), index, uncons, unsnoc, cons)

import Polysemy
import Polysemy.Error (Error, throw)

type family TMap (f :: k -> k') (xs :: [k]) :: [k'] where
    TMap f '[] = '[]
    TMap f (x : xs) = f x : TMap f xs

-- | for throwing multiple errors together
type Throws errs r = Members (TMap Error errs) r

deleteAt :: Int -> [a] -> [a]
deleteAt 0 (_:xs) = xs
deleteAt n (x:xs) | n >= 0 = x : deleteAt (n-1) xs
deleteAt _ _ = error "index out of range"

justOrThrow :: Throws '[err] r => err -> Maybe a -> Sem r a
justOrThrow err = \case
    Just x -> pure x
    Nothing -> throw err

mapOfFunction :: Ord k => [k] -> (k -> v) -> Map k v
mapOfFunction ks f = Map.fromList (ks `zip` map f ks)
