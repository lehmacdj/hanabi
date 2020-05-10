{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE AllowAmbiguousTypes #-} -- for makeSem with polymorphic type
-- makeWrapped needs these two
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Game
  ( module Game
  ) where

import MyPrelude
import Refined
import Refined.Unsafe as Unsafe
import Data.Generics.Labels ()

import Polysemy.State
import Polysemy.ConstraintAbsorber.MonadState
import Polysemy.Error
import Polysemy.Output

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
newtype Number = Number { unNumber :: Refined IsNumber Int }
  deriving (Generic, Eq, Ord)
makeWrapped ''Number

instance Show Number where
  show = show . unrefine . unNumber

instance Enum Number where
  fromEnum = unrefine . unNumber
  toEnum = Number . Unsafe.unsafeRefine

instance Bounded Number where
  minBound = Number ($$(refineTH 1) :: Refined IsNumber Int)
  maxBound = Number ($$(refineTH 5) :: Refined IsNumber Int)

-- | Needed for Enum/Bounded instance for card
numberCount :: Int
numberCount = length [minBound :: Number .. maxBound]

-- | increase number by 1 failing if that is impossible
bumpNumber :: Number -> Maybe Number
bumpNumber = fmap Number . refineThrow . succ . unrefine . unNumber

type IsFireworkNumber = And (From 0) (To 5)
type FireworkNumber = Refined IsFireworkNumber Int

-- | increase number by 1 failing if that is impossible
bumpFireworkNumber :: FireworkNumber -> Maybe FireworkNumber
bumpFireworkNumber = refineThrow . succ . unrefine

injectNumber :: Number -> FireworkNumber
injectNumber = Unsafe.reallyUnsafeRefine . unrefine . unNumber

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
  show (Card c n) = show c ++ show (unrefine (unNumber n))

-- TODO: add test for instance
instance Enum Card where
  fromEnum (Card c n)  = colorCount * pred (unrefine (unNumber n)) + fromEnum c
  toEnum = helper where
    helper :: HasCallStack => Int -> Card
    helper i =
      case over _1 (fmap Number . refineThrow . succ) $ i `divMod` colorCount of
        (Nothing, _) -> error "out of bounds index for Enum Card"
        (Just n, c) -> Card (toEnum c) n

-- TODO: add test for instance
instance Bounded Card where
  minBound = toEnum 0
  maxBound = toEnum $ (numberCount - 1) * colorCount + (colorCount - 1)

-- | given a card what is its frequency in the deck
cardDuplicity :: Card -> Int
cardDuplicity (Card _ n)
  | unrefine (unNumber n) == 1 = 3
  | unrefine (unNumber n) == 5 = 1
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

-- | The number of cards each player starts with in their hand. Dependent
-- on the number of players.
startingCardsInHand :: forall p. (Enum p, Bounded p, HasCallStack) => Int
startingCardsInHand
  | playerCount > 5 = error "too many players"
  | playerCount > 3 = 4
  | playerCount > 0 = 5
  | otherwise = error "too few players"
  where
    playerCount = length [minBound :: p .. maxBound]

allCardIxs :: forall p. (Enum p, Bounded p) => [CardIx]
allCardIxs = [0 .. startingCardsInHand @p - 1]

-- | A hand is just a list of cards with a couple of invariants, in terms of
-- gameplay.
-- 1. Cards are always inserted at index 0. A corrolary of this is that the
-- oldest card in a players hand is the card at the greatest index.
-- 2. startingCardsInHand p - 1 <= length hand <= startingCardsInHand p
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

type HasGameState p = State (GameState p)

data Hint
  = AreColor Color (Set CardIx)
  | AreNumber Number (Set CardIx)
  deriving (Show, Generic, Eq, Ord)
makePrisms ''Hint

data Action p
  = Play CardIx
  | Discard CardIx
  | Hint p Hint
  -- ^ the player specifies the player being given the hint
  deriving (Show, Generic)

data Turn p = Turn p (Action p)

-- | the possibilities  of what a card can be in a players hand
data CardPossibilities = CardPossibilities
    { colors :: Set Color
    , numbers :: Set Number
    }
    deriving (Show, Generic, Eq, Ord)

-- | operation for this instance is intersection or merging of information
instance Semigroup CardPossibilities where
  cp1 <> cp2 = CardPossibilities
    (view #colors cp1 `intersect` view #colors cp2)
    (view #numbers cp1 `intersect` view #numbers cp2)

-- | unit is Top, i.e. no information
instance Monoid CardPossibilities where
  mempty = CardPossibilities setAny setAny

cardIs :: Card -> CardPossibilities
cardIs c = CardPossibilities color number where
  color = singleton (c ^. #color)
  number = singleton (c ^. #number)

cardIsNotColor :: Color -> CardPossibilities
cardIsNotColor c = CardPossibilities (setExcept c) setAny

cardIsColor :: Color -> CardPossibilities
cardIsColor c = CardPossibilities (singleton c) setAny

cardIsNumber :: Number -> CardPossibilities
cardIsNumber n = CardPossibilities setAny (singleton n)

cardIsNotNumber :: Number -> CardPossibilities
cardIsNotNumber n = CardPossibilities setAny (setExcept n)

-- | indices in HandPossibilities have the same invariants as Hand has
type HandPossibilities = [CardPossibilities]

data Information p
  = RemovedFromHand p CardIx
  | CardSatisfies p CardIx CardPossibilities
  deriving (Show, Generic)

aboutPlayer :: Lens (Information p) (Information q) p q
aboutPlayer = lens get set where
  get = \case
    RemovedFromHand p _ -> p
    CardSatisfies p _ _ -> p
  set i q = case i of
    RemovedFromHand _ cix -> RemovedFromHand q cix
    CardSatisfies _ cix cps -> CardSatisfies q cix cps

informationFromHint
  :: forall p.
     (Enum p, Bounded p)
  => p -> Hint -> [Information p]
informationFromHint p = \case
  AreColor c cixs ->
    [ CardSatisfies p cix $ cardIsColor c
    | cix <- setToList cixs
    ] ++
    [ CardSatisfies p cix $ cardIsNotColor c
    | cix <- filter (not . (`elem` cixs)) $ allCardIxs @p
    ]
  AreNumber n cixs ->
    [ CardSatisfies p cix $ cardIsNumber n
    | cix <- setToList cixs
    ] ++
    [ CardSatisfies p cix $ cardIsNotNumber n
    | cix <- filter (not . (`elem` cixs)) $ allCardIxs @p
    ]

data CardDoesNotExist = CardDoesNotExist
  deriving (Show)
data GameOver = GameOver
  deriving (Show)

-- | Player input/output, the way the game interacts with the players
data PlayerIO p m a where
  Prompt :: p -- ^ the player to prompt
         -> Board -- ^ the board at the time of the prompt
         -> Int -- ^ the number of cards left in the deck at time of prompt
         -> PlayerIO p m (Action p)
  Inform :: p -> Information p -> PlayerIO p m ()
makeSem ''PlayerIO

informExcept
  :: forall p r.
     ( Member (PlayerIO p) r
     , Enum p, Bounded p, Eq p
     )
  => p -> Information p -> Sem r ()
informExcept p info =
  for_ (filter (/= p) [minBound :: p .. maxBound]) $ \p -> inform p info

broadcast
  :: forall p r.
     ( Member (PlayerIO p) r
     , Enum p, Bounded p
     )
  => Information p -> Sem r ()
broadcast info = for_ [minBound :: p .. maxBound] $ \p -> inform p info

-- | take a card out of a players hand, replacing that card with a new card
-- from the deck. returns Nothing if there are no cards left in the deck
-- TODO: distribute information about what cards are drawn from the deck
takeCard
  :: forall p r.
     ( Members [PlayerIO p, HasGameState p] r
     , Throws '[CardDoesNotExist] r
     , Ord p, Enum p, Bounded p
     )
  => p -> CardIx -> Sem r Card
takeCard p cix = absorbState @(GameState p) $ do
  -- we need to reference the original state after we modify it so save a copy
  s <- get @(GameState p)
  -- this needs to be here and not computed at the end because otherwise we
  -- throw the exception after modifying state
  card <- justOrThrow CardDoesNotExist $ s ^? #hands . handFor p . cardFor cix
  _ <- #deck %= drop 1
  #hands . handFor p %= deleteAt cix
  broadcast @p $ RemovedFromHand p cix
  whenJust (s ^? #deck . ix 0) $ \nextCard -> do
    informExcept p $ CardSatisfies p 0 $ cardIs nextCard
    #hands . handFor p %= (nextCard :)
  pure card

play
  :: forall p r.
     ( Members [PlayerIO p, HasGameState p] r
     , Throws [CardDoesNotExist, GameOver] r
     , Ord p, Enum p, Bounded p
     )
  => p -> CardIx -> Sem r ()
play p cix = do
  c <- takeCard p cix
  s <- get @(GameState p)
  s <- justOrThrow GameOver $ mapMOf #board (playB c) s
  put @(GameState p) s

discard
  :: forall p r.
     ( Members [PlayerIO p, HasGameState p] r
     , Throws [CardDoesNotExist, GameOver] r
     , Ord p, Enum p, Bounded p
     )
  => p -> CardIx -> Sem r ()
discard p cix = absorbState @(GameState p) $ do
  c <- takeCard p cix
  #board %= discardB c

hint
  :: forall p r.
     ( Members [PlayerIO p, HasGameState p] r
     , Throws '[GameOver] r
     , Enum p, Bounded p
     )
  => p -> Hint -> Sem r ()
hint p h = for_ (informationFromHint p h) broadcast

dealHands
  :: forall p. (Ord p, Enum p, Bounded p)
  => Deck -> (Deck, Hands p)
dealHands startingDeck = (remainingDeck, makePlayerMap (players `zip` hs)) where
  players = [minBound :: p .. maxBound]
  startingCards = startingCardsInHand @p
  (remainingDeck, hs) = run . runState startingDeck $ playerHands
  playerHands = replicateM (length players) dealHand
  dealHand :: Member (State Deck) r => Sem r Hand
  dealHand = do
    d <- get @Deck
    let (hand, rest) = splitAt startingCards d
    put @Deck rest
    pure hand

hintIsValidFor :: Hint -> Hand -> Bool
hintIsValidFor hint hand =
  let
    allIxs = setFromList [0 .. length hand - 1]
    testPos, testNeg :: Eq a => a -> Lens' Card a -> Set CardIx -> Bool
    testPos x l = all (\i -> has (ix i . l . only x) hand)
    testNeg x l ixs =
      none (\i -> has (ix i . l . only x) hand) (allIxs `difference` ixs)
   in case hint of
        AreColor c ixs -> testPos c #color ixs && testNeg c #color ixs
        AreNumber n ixs -> testPos n #number ixs && testNeg n #number ixs

validateAction
  :: forall p r.
     ( Member (HasGameState p) r
     , Enum p, Bounded p, Ord p
     )
  => Action p -> Sem r (Maybe (Action p))
validateAction a = do
  hs <- view #hands <$> get @(GameState p)
  pure $ case a of
    a'@(Hint p hint)
      | hint `hintIsValidFor` view (handFor p) hs -> Just a'
      | otherwise -> Nothing
    a' -> Just a'

gameLoop
  :: forall p r.
     ( Members [PlayerIO p, HasGameState p, Output (Turn p) ] r
     , Throws [CardDoesNotExist, GameOver] r
     , Ord p, Enum p, Bounded p
     )
  => p -- ^ the player whose turn it is
  -> Sem r Void
gameLoop currentPlayer = do
  s <- get @(GameState p)
  let
    b = view #board s
    ds = view (#deck . to length) s
    promptCurrentPlayer = prompt currentPlayer b ds
  a <- untilJust (promptCurrentPlayer >>= validateAction)
  output @(Turn p) (Turn currentPlayer a)
  case a of
    Play cix -> play currentPlayer cix
    Discard cix -> discard currentPlayer cix
    Hint p h -> hint p h
  gameLoop (next currentPlayer)

-- | Does precondition verification + sets up GameState/interprets HasGameState
runGame
  :: forall p r.
     ( HasCallStack
     , Members [PlayerIO p, Output (Turn p)] r
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
