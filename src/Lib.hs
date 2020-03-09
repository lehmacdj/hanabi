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
type Time = Refined IsTime Int

maxTime :: Time
maxTime = $$(refineTH 8)

-- | increase time by 1 step
addTime :: Time -> Time
addTime = fromMaybe maxTime . refineThrow . succ . unrefine

-- | decrease time by 1 step, returning nothing if that is impossible
loseTime :: Time -> Maybe Time
loseTime = refineThrow . pred . unrefine

-- | the number of times the players have played a card incorrectly
type IsFuse = And (From 0) (To 2)
type Fuse = Refined IsFuse Int

fuseStart :: Fuse
fuseStart = $$(refineTH 0)

-- | try to increase the fuse by one, if the players loose because of this
-- return nothing
bumpFuse :: Fuse -> Maybe Fuse
bumpFuse = refineThrow . succ . unrefine

type IsCardNumber = And (From 1) (To 5)
type CardNumber = Refined IsCardNumber Int

-- | increase number by 1 failing if that is impossible
bumpCardNumber :: CardNumber -> Maybe CardNumber
bumpCardNumber = refineThrow . succ . unrefine

type IsFireworkNumber = And (From 0) (To 5)
type FireworkNumber = Refined IsFireworkNumber Int

-- | increase number by 1 failing if that is impossible
bumpFireworkNumber :: FireworkNumber -> Maybe FireworkNumber
bumpFireworkNumber = refineThrow . succ . unrefine

injectCardNumber :: CardNumber -> FireworkNumber
injectCardNumber = Unsafe.reallyUnsafeRefine . unrefine

fn0 :: FireworkNumber
fn0 = $$(refineTH 0)

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
    getter = fromMaybe fn0 . view (#underlyingMap . at c)
    setter' n = #underlyingMap . at c ?~ n

-- | the board is the state shared between all players
data Board = Board
    { fireworks :: Fireworks
    , time :: Time
    , fuse :: Fuse
    , discarded :: Set Card
    }
    deriving (Show, Generic)

-- | Either update the state (i.e. increase fuse/correct number) or
-- return Nothing if the game is over
playB :: Card -> Board -> Maybe Board
playB c b = (<|> overM #fuse bumpFuse b) $ do
    let fireworkNumber = view (#fireworks . numberFor (view #color c)) b
    newFireworkNumber <- bumpFireworkNumber fireworkNumber
    guard (newFireworkNumber == injectCardNumber (view #number c))
    pure (set (#fireworks . numberFor (view #color c)) newFireworkNumber b)

discardB :: Card -> Board -> Board
discardB c = (#discarded <>~ singleton c) . over #time addTime

type Deck = [Card]

type IsCardIx = And (From 0) (To 4)
type CardIx = Refined IsCardIx Int
c0, c1, c2, c3, c4 :: CardIx
c0 = $$(refineTH 0)
c1 = $$(refineTH 1)
c2 = $$(refineTH 2)
c3 = $$(refineTH 3)
c4 = $$(refineTH 4)

newtype Hand = Hand { underlyingMap :: CardIx -> Card }
    deriving (Generic)
instance Show Hand where
    -- | show (Hand (const c)) == "[c, c, c, c, c]"
    show hs = squared . intercalate ", " . map showCard $ [c0,c1,c2,c3,c4] where
        showCard = show . view #underlyingMap hs
        squared s = "[" ++ s ++ "]"

getCard :: CardIx -> Hand
getCard = flip (view #underlyingMap)

data Player = P0 | P1 | P2
    deriving (Show, Generic, Eq, Ord, Enum, Bounded)

newtype Hands = Hands { underlyingMap :: Player -> Hand }
    deriving (Generic)
instance Show Hands where
    -- | show (Hands (const [])) == "{P0 -> [], P1 -> [], P2 -> []}"
    show hs = bracketed . intercalate ", " . map showHand $ [P0 .. P2] where
        showHand p = show p ++ " -> " ++ show (view #underlyingMap hs p)
        bracketed s = "{" ++ s ++ "}"

getHand :: Player -> Hands -> Hand
getHand = flip (view #underlyingMap)

-- | a god's eye view of the state of the game, used for the core game loop,
-- judging the actions of the players
data State = State
    { board :: Board
    , deck :: Deck
    , hands :: Hands
    }
    deriving (Show, Generic)

someFunc :: IO ()
someFunc = pure ()
