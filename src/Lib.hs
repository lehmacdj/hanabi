{-# LANGUAGE TemplateHaskell #-}

module Lib
    ( module Lib
    ) where

import MyPrelude
import Refined
import Refined.Unsafe as Unsafe
import Data.Generics.Labels ()

-- | amount of time available to the players
type IsTime = And (From 0) (To 8)
newtype Time = Time { unTime :: Refined IsTime Int }
    deriving (Show, Eq, Ord, Generic)

maxTime :: Time
maxTime = Time $$(refineTH 8)

-- | increase time by 1 step
addTime :: Time -> Time
addTime = maybe maxTime Time . refineThrow . succ . unrefine . unTime

-- | decrease time by 1 step, returning nothing if that is impossible
loseTime :: Time -> Maybe Time
loseTime = fmap Time . refineThrow . pred . unrefine . unTime

-- | the number of times the players have played a card incorrectly
type IsFuse = And (From 0) (To 2)
newtype Fuse = Fuse { unFuse :: Refined IsFuse Int }
    deriving (Show, Eq, Ord, Generic)

fuseStart :: Fuse
fuseStart = Fuse $$(refineTH 0)

-- | try to increase the fuse by one, if the players loose because of this
-- return nothing
bumpFuse :: Fuse -> Maybe Fuse
bumpFuse = fmap Fuse . refineThrow . succ . unrefine . unFuse

type IsCardNumber = And (From 1) (To 5)
newtype CardNumber = CardNumber { unCardNumber :: Refined IsCardNumber Int }
    deriving (Show, Eq, Ord, Generic)

-- | increase number by 1 failing if that is impossible
bumpCardNumber :: CardNumber -> Maybe CardNumber
bumpCardNumber = fmap CardNumber . refineThrow . succ . unrefine . unCardNumber

type IsFireworkNumber = And (From 0) (To 5)
newtype FireworkNumber = FireworkNumber
    { unFireworkNumber :: Refined IsFireworkNumber Int }
    deriving (Show, Eq, Ord, Generic)

-- | increase number by 1 failing if that is impossible
bumpFireworkNumber :: FireworkNumber -> Maybe FireworkNumber
bumpFireworkNumber =
    fmap FireworkNumber . refineThrow . succ . unrefine . unFireworkNumber

injectCardNumber :: CardNumber -> FireworkNumber
injectCardNumber =
    FireworkNumber . Unsafe.reallyUnsafeRefine . unrefine . unCardNumber

data Color = Red | Blue | Green | Yellow | White
    deriving (Show, Eq, Ord, Enum, Bounded, Generic)

data Card = Card
    { color :: Color
    , number :: CardNumber
    }
    deriving (Show, Eq, Ord, Generic)

newtype Fireworks = Fireworks { underlyingMap :: Map Color FireworkNumber }
    deriving (Show, Generic)

numberFor :: Color -> Lens' Fireworks FireworkNumber
numberFor c = lens getter (flip setter') where
    getter = fromMaybe (FireworkNumber $$(refineTH 0)) . view (#underlyingMap . at c)
    setter' n = #underlyingMap . at c ?~ n

-- | the board is the state shared between all players
data Board = Board
    { fireworks :: Fireworks
    , time :: Time
    , fuse :: Fuse
    }
    deriving (Show, Generic)

-- | Either update the state (i.e. increase fuse/correct number) or
-- return Nothing if the game is over
playToBoard :: Card -> Board -> Maybe Board
playToBoard c b = (<|> overM #fuse bumpFuse b) $ do
    let fireworkNumber = view (#fireworks . numberFor (view #color c)) b
    newFireworkNumber <- bumpFireworkNumber fireworkNumber
    guard (newFireworkNumber == injectCardNumber (view #number c))
    pure (set (#fireworks . numberFor (view #color c)) newFireworkNumber b)

someFunc :: IO ()
someFunc = pure ()
