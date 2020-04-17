{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE AllowAmbiguousTypes #-} -- for makeSem with polymorphic type

module Game
    ( module Game
    ) where

import MyPrelude
import Refined
import Refined.Unsafe as Unsafe
import Data.Generics.Labels ()

import Polysemy.State
import Polysemy.Error

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

type IsNumber = And (From 1) (To 5)
type Number = Refined IsNumber Int

-- | Needed for Enum/Bounded instance for card
numberCount :: Int
numberCount = 5

-- | increase number by 1 failing if that is impossible
bumpNumber :: Number -> Maybe Number
bumpNumber = refineThrow . succ . unrefine

type IsFireworkNumber = And (From 0) (To 5)
type FireworkNumber = Refined IsFireworkNumber Int

-- | increase number by 1 failing if that is impossible
bumpFireworkNumber :: FireworkNumber -> Maybe FireworkNumber
bumpFireworkNumber = refineThrow . succ . unrefine

injectNumber :: Number -> FireworkNumber
injectNumber = Unsafe.reallyUnsafeRefine . unrefine

fn0 :: FireworkNumber
fn0 = $$(refineTH 0)

data Color = Red | Blue | Green | Yellow | White
    deriving (Eq, Ord, Enum, Bounded, Generic)

instance Show Color where
    show = \case
        Red -> "R"
        Blue -> "B"
        Green -> "G"
        Yellow -> "Y"
        White -> "W"

-- | needed for Enum Card instance + Bounded Instance
colorCount :: Int
colorCount = length [minBound :: Color .. maxBound]

data Card = Card
    { color :: Color
    , number :: Number
    }
    deriving (Eq, Ord, Generic)

instance Show Card where
    show (Card c n) = show c ++ show (unrefine n)

-- TODO: add test for instance
instance Enum Card where
    fromEnum (Card c n)  = colorCount * pred (unrefine n) + fromEnum c
    toEnum = helper where
        helper :: HasCallStack => Int -> Card
        helper i =
            case over _1 (refineThrow . succ) $ i `divMod` colorCount of
                (Nothing, _) -> error "out of bounds index for Enum Card"
                (Just n, c) -> Card (toEnum c) n

-- TODO: add test for instance
instance Bounded Card where
    minBound = toEnum 0
    maxBound = toEnum $ (numberCount - 1) * colorCount + (colorCount - 1)

-- | given a card what is its frequency in the deck
cardDuplicity :: Card -> Int
cardDuplicity (Card _ n)
  | unrefine n == 1 = 3
  | unrefine n == 5 = 1
  | otherwise = 2

-- | All of the cards is a list of each card its duplicity times
allCards :: [Card]
allCards = [minBound..maxBound] >>= \c -> replicate (cardDuplicity c) c

newtype Fireworks = Fireworks { underlyingMap :: Map Color FireworkNumber }
    deriving (Show, Generic)

startingFireworks :: Fireworks
startingFireworks = Fireworks . mapFromList $ [Red .. White] `zip` repeat fn0

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

startingBoard :: Board
startingBoard = Board startingFireworks maxTime fuseStart mempty

-- | add a given card to the discard pile
discardCard :: Card -> Board -> Board
discardCard c = #discarded <>~ singleton c

-- | Either update the state (i.e. increase fuse/correct number) or
-- return Nothing if the game is over
playB :: Card -> Board -> Maybe Board
playB c b = (<|> mapMOf #fuse bumpFuse (discardCard c b)) $ do
    let fireworkNumber = view (#fireworks . numberFor (view #color c)) b
    newFireworkNumber <- bumpFireworkNumber fireworkNumber
    guard (newFireworkNumber == injectNumber (view #number c))
    pure (set (#fireworks . numberFor (view #color c)) newFireworkNumber b)

discardB :: Card -> Board -> Board
discardB c = discardCard c . over #time addTime

type Deck = [Card]

type CardIx = Int
type Hand = [Card]

-- returns an affine traversal (one returning 0 or 1 entries only)
cardFor :: Int -> Traversal' Hand Card
cardFor = ix

-- | invariant: Map is total, every entry has a value
-- makePlayerMap checks this invariant; use it over the Hands construtor
-- generally p needs to be Enum, Bounded, and Ord
-- this ensures that we can enumerate all the players and that they can
-- be used as a key in a map
newtype PlayerMap p h = PlayerMap { underlyingMap :: Map p h }
    deriving (Generic, Show)

makePlayerMap
    :: (HasCallStack, Ord p, Enum p, Bounded p)
    => [(p,h)] -> PlayerMap p h
makePlayerMap = PlayerMap . mapFromList . checkInvariant where
  checkInvariant xs
    | map fst xs `isPermutationOf` [minBound..maxBound] = xs
    | otherwise = error "every player must be accounted for in PlayerMap"

handFor
    :: ( HasCallStack
       , Eq h
       , Ord p, Enum p, Bounded p
       )
    => p -> Lens (PlayerMap p h) (PlayerMap p h) h h
handFor p = lens getter (flip setter') where
    err = error "invariant violated: handFor lens getter"
    getter = view (#underlyingMap . at p . non err)
    setter' h = #underlyingMap . at p ?~ h

type Hands p = PlayerMap p Hand

-- | a god's eye view of the state of the game, used for the core game loop,
-- judging the actions of the players
data GameState p = GameState
    { board :: Board
    , deck :: Deck
    , hands :: Hands p
    }
    deriving (Show, Generic)

data CardDoesNotExist = CardDoesNotExist
    deriving (Show)
data GameOver = GameOver
    deriving (Show)

-- | take a card out of a players hand, replacing that card with a new card
-- from the deck. returns Nothing if there are no cards left in the deck
takeCard
    :: ( Throws '[CardDoesNotExist] r
       , Ord p, Enum p, Bounded p
       )
    => p -> CardIx -> GameState p -> Sem r (Card, GameState p)
takeCard p cix s = do
    card <- justOrThrow CardDoesNotExist $ s ^? #hands . handFor p . cardFor cix
    let
      updateDeck = #deck .~ drop 1 (view #deck s)
      updateHand
        = case s ^? #deck . ix 0 of
            -- this is gross and should be cleaner, could be cleaner
            -- if I was smarter
            Nothing -> over (#hands . handFor p) (deleteAt cix)
            Just nextCard -> #hands . handFor p . cardFor cix .~ nextCard
    pure (card, updateHand . updateDeck $ s)

play
    :: ( Throws [CardDoesNotExist, GameOver] r
       , Ord p, Enum p, Bounded p
       )
    => p -> CardIx -> GameState p -> Sem r (GameState p)
play p cix s = do
    (c, s') <- takeCard p cix s
    justOrThrow GameOver $ mapMOf #board (playB c) s'

discard
    :: ( Throws [CardDoesNotExist, GameOver] r
       , Ord p, Enum p, Bounded p
       )
    => p -> CardIx -> GameState p -> Sem r (GameState p)
discard p cix s = do
    (c, s') <- takeCard p cix s
    pure $ over #board (discardB c) s'

data Hint
    = AreColor Color (Set CardIx)
    | AreNumber Color (Set Number)
    deriving (Show, Generic, Eq, Ord)
makePrisms ''Hint

data Action p
    = Play CardIx
    | Discard CardIx
    | Hint p Hint
    -- ^ the player specifies the player being given the hint
    deriving (Show, Generic)

data Turn p = Turn
    { player :: p
    , action :: Action p
    }
    deriving (Show, Generic)

type HasGameState p = State (GameState p)

-- | Player input/output, the way the game interacts with the players
data PlayerIO p m a where
    Prompt :: p -- ^ the player to prompt
           -> Board -- ^ the board at the time of the prompt
           -> Int -- ^ the number of cards left in the deck at time of prompt
           -> PlayerIO p m (Action p)
    Inform :: p -> PlayerIO p m ()
makeSem ''PlayerIO

-- | The number of cards each player starts with in their hand. Dependent
-- on the number of players.
startingCardsInHand :: HasCallStack => [p] -> Int
startingCardsInHand ps
  | playerCount > 5 = error "startingCardsInHand: too many players"
  | playerCount > 3 = 4
  | playerCount > 0 = 5
  | otherwise = error "startingCardsInHand: too few players"
  where
    playerCount = length ps

dealHands
  :: forall p. (Ord p, Enum p, Bounded p)
  => Deck -> (Deck, Hands p)
dealHands startingDeck = (remainingDeck, makePlayerMap (players `zip` hs)) where
    players = [minBound :: p .. maxBound]
    startingCards = startingCardsInHand [minBound :: p .. maxBound]
    (remainingDeck, hs) = run . runState startingDeck $ playerHands
    playerHands = replicateM (length players) dealHand
    dealHand :: Member (State Deck) r => Sem r Hand
    dealHand = do
      d <- get @Deck
      let (hand, rest) = splitAt startingCards d
      put @Deck rest
      pure hand

cycledSucc :: (Eq a, Enum a, Bounded a) => a -> a
cycledSucc x
  | x == maxBound = minBound
  | otherwise = succ x

gameLoop
    :: forall p r.
       ( Members [PlayerIO p, HasGameState p] r
       , Throws [CardDoesNotExist, GameOver] r
       , Ord p, Enum p, Bounded p
       )
    => p -- ^ the player whose turn it is
    -> Sem r Void
gameLoop currentPlayer = do
    s <- get @(GameState p)
    a <- prompt currentPlayer (view #board s) (view (#deck . to length) s)
    undefined a -- give info to players + update the state
    gameLoop (cycledSucc currentPlayer)

-- | Does precondition verification + sets up GameState/interprets HasGameState
runGame
    :: forall p r.
       ( HasCallStack
       , Member (PlayerIO p) r
       , Throws '[CardDoesNotExist] r
       , Ord p, Enum p, Bounded p
       )
    => Deck -- ^ must be a permutation of the full deck
    -> Sem r Fireworks -- ^ result is the fireworks produced by the game
runGame shuffledDeck =
    let
      _players = [minBound..maxBound]
      (startingDeck, startingHands) = dealHands @p shuffledDeck
      startingState = GameState startingBoard startingDeck startingHands

      startingDeckMsg = "startingDeck must be a permutation of allCards"
      startingDeckPre = startingDeck `isPermutationOf` allCards
      checkedLoop
        | not startingDeckPre = error startingDeckMsg
        | otherwise = gameLoop @p @(Error GameOver : State (GameState p) : r)
     in fmap (view (_1 . #board . #fireworks))
      . runState startingState
      . runError'
      $ checkedLoop (minBound @p)
      where
        runError' :: forall e r. Sem (Error e : r) Void -> Sem r e
        runError' = fmap (either id absurd) . runError
