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

-- | add a given card to the discard pile
discardCard :: Card -> Board -> Board
discardCard c = #discarded <>~ singleton c

-- | Either update the state (i.e. increase fuse/correct number) or
-- return Nothing if the game is over
playB :: Card -> Board -> Maybe Board
playB c b = (<|> overM #fuse bumpFuse (discardCard c b)) $ do
    let fireworkNumber = view (#fireworks . numberFor (view #color c)) b
    newFireworkNumber <- bumpFireworkNumber fireworkNumber
    guard (newFireworkNumber == injectCardNumber (view #number c))
    pure (set (#fireworks . numberFor (view #color c)) newFireworkNumber b)

discardB :: Card -> Board -> Board
discardB c = discardCard c . over #time addTime

type Deck = [Card]

type IsCardIx = And (From 0) (To 4)
type CardIx = Refined IsCardIx Int
c0, c1, c2, c3, c4 :: CardIx
c0 = $$(refineTH 0)
c1 = $$(refineTH 1)
c2 = $$(refineTH 2)
c3 = $$(refineTH 3)
c4 = $$(refineTH 4)

type Hand = [Card]

-- returns an affine traversal (one returning 0 or 1 entries only)
cardFor :: CardIx -> Traversal' Hand Card
cardFor = element . unrefine

data Player = P0 | P1 | P2
    deriving (Show, Generic, Eq, Ord, Enum, Bounded)

-- | invariant: Map is total, every entry has a value
-- makeHands checks this invariant; use it over the Hands construtor
newtype Hands = Hands { underlyingMap :: Map Player Hand }
    deriving (Generic, Show)

makeHands :: (Player -> Hand) -> Hands
makeHands = Hands . mapOfFunction [P0, P1, P2]

handFor :: Player -> Lens' Hands Hand
handFor p = lens getter (flip setter') where
    err = error "invariant violated: handFor lens getter"
    getter = fromMaybe err . view (#underlyingMap . at p)
    setter' h = #underlyingMap . at p ?~ h

-- | a god's eye view of the state of the game, used for the core game loop,
-- judging the actions of the players
data State = State
    { board :: Board
    , deck :: Deck
    , hands :: Hands
    }
    deriving (Show, Generic)

-- | take a card out of a players hand, replacing that card with a new card
-- from the deck. returns Nothing if there are no cards left in the deck
takeCard :: Player -> CardIx -> State -> Maybe (Card, State)
takeCard p cix s = do
    -- TODO: use effects from polysemy so that I can signal different error
    -- conditions effectively
    card <- s ^? #hands . handFor p . cardFor cix
    nextCard <- s ^? #deck . element 0
    let updateCard = #hands . handFor p . cardFor cix .~ nextCard
    let updateDeck = #deck .~ drop 1 (view #deck s)
    pure (card, updateCard . updateDeck $ s)

play :: Player -> CardIx -> State -> Maybe State
play p cix s = do
    (c, s') <- takeCard p cix s
    overM #board (playB c) s'

discard :: Player -> CardIx -> State -> Maybe State
discard p cix s = do
    (c, s') <- takeCard p cix s
    pure $ over #board (discardB c) s'

someFunc :: IO ()
someFunc = pure ()
