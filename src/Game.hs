{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Game
  ( module Game,
  )
where

-- @concurrency: TODO: replace State with AtomicState

import Data.Coerce (coerce)
import Data.Generics.Labels ()
import Data.Random
import MyPrelude
import Player
import Polysemy.ConstraintAbsorber.MonadState
import Polysemy.Error
import Polysemy.Input
import Polysemy.Output
import Polysemy.State
import Refined
import Refined.Unsafe as Unsafe
import Text.Read (readsPrec)

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

newtype Number = Number {unNumber :: Refined IsNumber Int}
  deriving (Generic, Eq, Ord)

makeWrapped ''Number

instance Show Number where
  show = show . unrefine . unNumber

instance Read Number where
  readsPrec _ n@[_] = toList $ do
    n <- readMay n
    n <- refineThrow n
    pure (Number n, "")
  readsPrec _ _ = []

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

instance Read Color where
  readsPrec _ s
    | s == "R" = [(Red, "")]
    | s == "B" = [(Blue, "")]
    | s == "G" = [(Green, "")]
    | s == "Y" = [(Yellow, "")]
    | s == "W" = [(White, "")]
    | otherwise = []

-- | needed for Enum Card instance + Bounded Instance
colorCount :: Int
colorCount = length [minBound :: Color .. maxBound]

data Card = Card
  { color :: Color,
    number :: Number
  }
  deriving (Eq, Ord, Generic)

instance Show Card where
  show (Card c n) = show c ++ show (unrefine (unNumber n))

instance Read Card where
  readsPrec _ [c, n] = toList $ do
    c <- readMay [c]
    n <- readMay [n]
    pure (Card c n, "")
  readsPrec _ _ = []

instance Enum Card where
  fromEnum (Card c n) = colorCount * pred (unrefine (unNumber n)) + fromEnum c
  toEnum = helper
    where
      helper :: HasCallStack => Int -> Card
      helper i =
        case over _1 (fmap Number . refineThrow . succ) $ i `divMod` colorCount of
          (Nothing, _) -> error "out of bounds index for Enum Card"
          (Just n, c) -> Card (toEnum c) n

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
allCards = [minBound .. maxBound] >>= \c -> replicate (cardDuplicity c) c

newtype Fireworks = Fireworks {underlyingMap :: Map Color FireworkNumber}
  deriving (Show, Generic)

startingFireworks :: Fireworks
startingFireworks = Fireworks . mapFromList $ [Red .. White] `zip` repeat fn0

numberFor :: Color -> Lens' Fireworks FireworkNumber
numberFor c = lens getter (flip setter')
  where
    getter = fromMaybe fn0 . view (#underlyingMap . at c)
    setter' n = #underlyingMap . at c ?~ n

-- | the board is the state shared between all players
data Board = Board
  { fireworks :: Fireworks,
    time :: Time,
    fuse :: Fuse,
    discarded :: Set Card
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
startingCardsInHand :: HasCallStack => [Player] -> Int
startingCardsInHand players
  | playerCount > 5 = error "too many players"
  | playerCount > 3 = 4
  | playerCount > 1 = 5
  | otherwise = error "too few players"
  where
    playerCount = length players

mkCardIx :: [Player] -> Int -> Maybe CardIx
mkCardIx players x
  | x >= 0 && x < startingCardsInHand players = Just x
  | otherwise = Nothing

allCardIxs :: [Player] -> [CardIx]
allCardIxs players = [0 .. startingCardsInHand players - 1]

-- | A hand is just a list of cards with a couple of invariants, in terms of
-- gameplay.
-- 1. Cards are always inserted at index 0. A corrolary of this is that the
-- oldest card in a players hand is the card at the greatest index.
-- 2. startingCardsInHand p - 1 <= length hand <= startingCardsInHand p
type Hand = [Card]

-- returns an affine traversal (one returning 0 or 1 entries only)
cardFor :: Int -> Traversal' Hand Card
cardFor = ix

-- perhaps it makes sense to implement something like this eventually?
-- deemed not worth it for now leaving here in case I want to pick up where I
-- left up at some point in the future
--
-- -- | Relatively adhoc typeclass encapsulating some utilities for dealing with
-- -- players + flagging a type as intended for use as a player.
-- -- Player types are defined for different player counts in their own modules
-- class (Show p, Ord p) => Player p where
--   firstPlayer :: p
--   firstPlayer = head players
--   nextPlayer :: p -> p
--   nextPlayer =
--   players :: [p]
--   {-# MINIMAL players | firstPlayer, nextPlayer #-}

-- | invariant: Map is total, every entry has a value
-- makePlayerMap checks this invariant; use it over the Hands construtor
-- generally p needs to be Enum, Bounded, and Ord
-- this ensures that we can enumerate all the players and that they can
-- be used as a key in a map
newtype PlayerMap h = PlayerMap {underlyingMap :: Map Player' h}
  deriving (Generic, Show)

-- | This function assumes that the players are valid
makePlayerMap ::
  HasCallStack =>
  -- | the caller promises that this is the complete list of players that will
  -- be used for the game
  [Player] ->
  [h] ->
  PlayerMap h
makePlayerMap players hands =
  checkInvariant (PlayerMap (mapFromList (coerce players `zip` hands)))
  where
    checkInvariant
      | length players == length hands = id
      | otherwise = error "every player must be accounted for in PlayerMap"

handFor ::
  ( HasCallStack,
    Eq h
  ) =>
  Player' ->
  Lens (PlayerMap h) (PlayerMap h) h h
handFor p = lens getter (flip setter')
  where
    err = error "invariant violated: handFor lens getter"
    getter = view (#underlyingMap . at p . non err)
    setter' h = #underlyingMap . at p ?~ h

-- | a god's eye view of the state of the game, used for the core game loop,
-- judging the actions of the players
data GameState = GameState
  { board :: Board,
    deck :: Deck,
    hands :: PlayerMap Hand,
    -- | The players in the current game, this defines what constitutes a
    -- Player'
    players :: [Player]
  }
  deriving (Show, Generic)

firstPlayer :: HasCallStack => GameState -> Player'
firstPlayer = headEx . view players'

nextPlayer :: HasCallStack => GameState -> Player' -> Player'
nextPlayer state p = headEx . takeWhile (/= p) $ cycle (state ^. players')
  where
    cycle xs
      | null xs = error "nextPlayer: empty list of players!"
      | otherwise = xs ++ cycle xs

-- | By definition any player in the game state is a Player' so this is indeed
-- safe.
players' :: Lens' GameState [Player']
players' = #players . coerced

-- | Player but validated against a game state.
-- @enhancement: we could enforce at the type level that this is true, but that
-- is probably somewhat difficult because it would require threading phantom
-- type variables absolutely everywhere which is probably not worth it
newtype Player' = UnsafeMkPlayer' {rawPlayer :: Player}
  deriving (Show, Eq, Ord, Generic)

validatePlayer :: GameState -> Player -> Maybe Player'
validatePlayer state player
  | anyOf (#players . traverse) (== player) state = Just (UnsafeMkPlayer' player)
  | otherwise = Nothing

data Hint
  = AreColor Color (Set CardIx)
  | AreNumber Number (Set CardIx)
  deriving (Show, Generic, Eq, Ord)

makePrisms ''Hint

data RawAction
  = RawPlay CardIx
  | RawDiscard CardIx
  | RawHint Player Hint
  deriving (Show, Eq, Generic)

data Action
  = Play CardIx
  | Discard CardIx
  | -- | the player specifies the player being given the hint
    Hint Player' Hint
  deriving (Show, Eq, Generic)

data RawTurn = RawTurn Player RawAction
  deriving (Show, Eq, Generic)

data Turn = Turn Player' Action
  deriving (Show, Eq, Generic)

-- | the possibilities  of what a card can be in a players hand
data CardPossibilities = CardPossibilities
  { colors :: Set Color,
    numbers :: Set Number
  }
  deriving (Show, Generic, Eq, Ord)

-- | operation for this instance is intersection or merging of information
instance Semigroup CardPossibilities where
  cp1 <> cp2 =
    CardPossibilities
      (view #colors cp1 `intersect` view #colors cp2)
      (view #numbers cp1 `intersect` view #numbers cp2)

-- | unit is Top, i.e. no information
instance Monoid CardPossibilities where
  mempty = CardPossibilities setAny setAny

cardIs :: Card -> CardPossibilities
cardIs c = CardPossibilities color number
  where
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

data Information
  = RemovedFromHand Player' CardIx
  | CardSatisfies Player' CardIx CardPossibilities
  deriving (Show, Generic)

aboutPlayer :: Lens' Information Player'
aboutPlayer = lens get set
  where
    get = \case
      RemovedFromHand p _ -> p
      CardSatisfies p _ _ -> p
    set i q = case i of
      RemovedFromHand _ cix -> RemovedFromHand q cix
      CardSatisfies _ cix cps -> CardSatisfies q cix cps

-- | information that is gained by a hint given to a specific player
informationFromHint ::
  -- | the player the hint was given to
  GameState ->
  Player' ->
  Hint ->
  [Information]
informationFromHint state p = \case
  AreColor c cixs ->
    [ CardSatisfies p cix $ cardIsColor c
      | cix <- setToList cixs
    ]
      ++ [ CardSatisfies p cix $ cardIsNotColor c
           | cix <- filter (not . (`elem` cixs)) (allCardIxs (state ^. #players))
         ]
  AreNumber n cixs ->
    [ CardSatisfies p cix $ cardIsNumber n
      | cix <- setToList cixs
    ]
      ++ [ CardSatisfies p cix $ cardIsNotNumber n
           | cix <- filter (not . (`elem` cixs)) (allCardIxs (state ^. #players))
         ]

data CardDoesNotExist = CardDoesNotExist
  deriving (Show)

data GameOver = GameOver
  deriving (Show)

-- | Player input/output, the way the game interacts with the players
data PlayerIO m a where
  Prompt ::
    -- | the player to prompt
    Player' ->
    PlayerIO m RawAction
  Inform :: Player' -> Information -> PlayerIO m ()

makeSem ''PlayerIO

-- | giveInfo === flip inform
-- For use with for_ to broadcast info to multiple players simultaneously
giveInfo :: Member PlayerIO r => Information -> Player' -> Sem r ()
giveInfo = flip inform

-- | only semi-lawful, see @filtered@ in Control.Lens for cases where it isn't
playersExcept :: Player' -> Traversal' GameState Player'
playersExcept p = players' . traverse . filtered (/= p)

-- | takes an input stream of turns, projects out the turns so that
-- turns are only received in the order that the game desires
-- For allowing input from several players at the same time, the Input acts
-- like a channel that receives turns, this makes sure to ignore actions
-- that are taken out of turn.
runPlayerIOToInputOutput ::
  forall r a.
  (Members [Input RawTurn, Output (Player', Information)] r) =>
  Sem (PlayerIO : r) a ->
  Sem r a
runPlayerIOToInputOutput = interpret $ \case
  Prompt p' -> untilJust $ do
    RawTurn p action <- input @RawTurn
    pure $ guard (p == rawPlayer p') *> Just action
  Inform p info -> output @(Player', Information) (p, info)

-- | take a card out of a players hand, replacing that card with a new card
-- from the deck. returns Nothing if there are no cards left in the deck
takeCard ::
  ( Members [PlayerIO, State GameState] r,
    Throws '[CardDoesNotExist] r
  ) =>
  Player' ->
  CardIx ->
  Sem r Card
takeCard p cix = absorbState @GameState $ do
  -- we need to reference the original state after we modify it so save a copy
  s <- get @GameState
  -- this needs to be here and not computed at the end because otherwise we
  -- throw the exception after modifying state
  card <- justOrThrow CardDoesNotExist $ s ^? #hands . handFor p . cardFor cix
  _ <- #deck %= drop 1
  #hands . handFor p %= deleteAt cix
  for_ (s ^. players') $ giveInfo $ RemovedFromHand p cix
  whenJust (s ^? #deck . ix 0) $ \nextCard -> do
    for_ (s ^.. playersExcept p) $ giveInfo $ CardSatisfies p 0 $ cardIs nextCard
    #hands . handFor p %= (nextCard :)
  pure card

play ::
  forall r.
  ( Members [PlayerIO, State GameState] r,
    Throws [CardDoesNotExist, GameOver] r
  ) =>
  Player' ->
  CardIx ->
  Sem r ()
play p cix = do
  c <- takeCard p cix
  s <- get @GameState
  s <- justOrThrow GameOver $ mapMOf #board (playB c) s
  put @GameState s

discard ::
  forall r.
  ( Members [PlayerIO, State GameState] r,
    Throws [CardDoesNotExist, GameOver] r
  ) =>
  Player' ->
  CardIx ->
  Sem r ()
discard p cix = absorbState @GameState $ do
  c <- takeCard p cix
  #board %= discardB c

hint ::
  ( Members [PlayerIO, State GameState] r,
    Throws '[GameOver] r
  ) =>
  Player' ->
  Hint ->
  Sem r ()
hint p h =
  get @GameState >>= \s ->
    for_
      (informationFromHint s p h)
      (for_ (s ^. players') . giveInfo)

dealHands ::
  [Player] ->
  Deck ->
  (Deck, PlayerMap Hand)
dealHands players startingDeck = (remainingDeck, makePlayerMap players hs)
  where
    startingCards = startingCardsInHand players
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
  let allIxs = setFromList [0 .. length hand - 1]
      testPos, testNeg :: Eq a => a -> Lens' Card a -> Set CardIx -> Bool
      testPos x l = all (\i -> has (ix i . l . only x) hand)
      testNeg x l ixs =
        none (\i -> has (ix i . l . only x) hand) (allIxs `difference` ixs)
   in case hint of
        AreColor c ixs -> testPos c #color ixs && testNeg c #color ixs
        AreNumber n ixs -> testPos n #number ixs && testNeg n #number ixs

-- | TODO: add more information to these hints to enable making stuff more user
-- friendly in the future
data ActionValidationError
  = InvalidHint
  | CardIxOutOfBounds
  | PlayerDoesNotExist

validateAction ::
  (Member (State GameState) r) =>
  RawAction ->
  Sem r (Either ActionValidationError Action)
validateAction a = do
  s <- get @GameState
  let ps = view #players s
      hs = view #hands s
  pure $ case a of
    RawHint p hint -> do
      p' <- maybeToRight PlayerDoesNotExist (validatePlayer s p)
      if hint `hintIsValidFor` view (handFor p') hs
        then Right $ Hint p' hint
        else Left InvalidHint
    RawPlay cix -> Play <$> maybeToRight CardIxOutOfBounds (mkCardIx ps cix)
    RawDiscard cix -> Discard <$> maybeToRight CardIxOutOfBounds (mkCardIx ps cix)

stepGame ::
  ( Members [PlayerIO, State GameState, Output Turn] r,
    Throws [CardDoesNotExist, GameOver] r
  ) =>
  Player' ->
  Sem r Player'
stepGame currentPlayer = do
  a <- untilJust (prompt currentPlayer >>= fmap rightToMaybe . validateAction)
  output @Turn (Turn currentPlayer a)
  case a of
    Play cix -> play currentPlayer cix
    Discard cix -> discard currentPlayer cix
    Hint p h -> hint p h
  s <- get @GameState
  pure $ nextPlayer s currentPlayer

gameLoop ::
  ( Members [PlayerIO, State GameState, Output Turn] r,
    Throws [CardDoesNotExist, GameOver] r
  ) =>
  -- | the player whose turn it is
  Player' ->
  Sem r Void
gameLoop currentPlayer = do
  nextPlayer <- stepGame currentPlayer
  gameLoop nextPlayer

-- | takes care of giving info about initial cards to players
fullGameLoop ::
  ( Members [PlayerIO, State GameState, Output Turn] r,
    Throws [CardDoesNotExist, GameOver] r
  ) =>
  Sem r Void
fullGameLoop = do
  s <- get @GameState
  for_ (s ^@.. #hands . #underlyingMap . (itraversed <.> itraversed)) $
    \((p, cix), c) ->
      for_ (s ^.. playersExcept p) $ giveInfo $ CardSatisfies p cix $ cardIs c
  gameLoop (firstPlayer s)

-- | A deck is only valid if it is a permutation of all of the cards
startingDeckValid :: Deck -> Bool
startingDeckValid = (`isPermutationOf` allCards)

startingStateFromDeck ::
  [Player] ->
  Deck ->
  GameState
startingStateFromDeck players deck =
  GameState startingBoard startingDeck startingHands players
  where
    (startingDeck, startingHands) = dealHands players deck

startingDeck :: RVar Deck
startingDeck = shuffle allCards

startingState ::
  [Player] ->
  RVar GameState
startingState players = startingStateFromDeck players <$> startingDeck

runGame ::
  GameState ->
  Sem (Error GameOver : State GameState : r) Void ->
  -- | result is the final game state
  Sem r GameState
runGame s = fmap fst . runState s . runError'
  where
    runError' :: forall e r. Sem (Error e : r) Void -> Sem r e
    runError' = fmap (either id absurd) . runError
