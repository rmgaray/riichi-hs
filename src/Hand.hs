{-# LANGUAGE TemplateHaskell #-}

module Hand
  ( Hand,
    HandShape,
    mkHand,
    hasPairs,
    hasMelds,
    hasFourMelds,
    hasPartialSeqs,
    hasSeqs,
    hasSingles,
    isSequence,
    isMeld,
  )
where

import Control.Lens.TH
import Data.Function ((&))
import Data.List (group, sort)
import GHC.Generics (Generic)
import Kanren.Core
import Kanren.Goal
import Kanren.Match
import Kanren.TH
import Tile
import Utils

{-- | A hand of Mahjong containing 13 tiles.

      Hands are ALWAYS ordered. Because of this, hands with the same tiles can
      always be compared with each other for equality.
--}
data Hand = Hand
  { -- | Pairs of identical tiles
    pairs :: [Tile],
    -- | Groups of three identical tiles
    melds :: [Tile],
    -- | Groups of four identical tiles
    fourMelds :: [Tile],
    -- | Pairs of numbered tiles, of the same suit,
    --   that can be made into a sequence with one additional tile
    partialSequences :: [Tile],
    -- | Groups of three numbered tiles, of the same suit, with ascending numbers
    sequences :: [Tile],
    -- | Single tiles
    singles :: [Tile]
  }
  deriving stock (Generic, Show, Eq)

makeLogicals [''Hand]
makePrisms ''LogicHand

deriving stock instance Show LogicHand

-- | An error while constructing a hand with 'mkHand'.
data InvalidHand
  = -- | More than 13 tiles were provided
    ExcessTiles
      -- | Tiles in excess
      [Tile]
  | -- | Less than 13 tiles were provided
    MissingTiles
      -- | How many tiles are missing
      Int
  | -- | More than 4 copies of the given tiles were used
    MoreThanFour
      -- | Tiles with more than 4 copies
      [Tile]
  deriving stock (Show)

-- | The hand shape describes a hand in terms of how many pairs, melds,
-- etc. it contains.
--
-- It is assumed that the number of tiles add up to 13, but not enforced.
data HandShape = HandShape
  { nPairs :: Int,
    nMelds :: Int,
    nFourMelds :: Int,
    nPartialSequences :: Int,
    nSequences :: Int,
    nSingles :: Int
  }
  deriving stock (Generic, Show, Eq)

makeLogicals [''HandShape]
makePrisms ''LogicHandShape

-- In japanese Mahjong there are many winning hands, but all winning
-- hands have one of the following shapes. Also, they all have 13 tiles.
-- TODO: Add the weirder winning conditions
-- >>> all (==13) $ fmap (\h -> sum $ fmap ($ h) [(2*) . nPairs, (3*) . nMelds, (4*) . nFourMelds, (2*) . nPartialSequences, (3*) . nSequences, nSingles]) winningShapes
-- True
winningShapes :: [HandShape]
winningShapes =
  [ -- Waiting on a single tile to complete the pair
    HandShape
      { nPairs = 0,
        nMelds = m,
        nFourMelds = 0,
        nPartialSequences = 0,
        nSequences = 4 - m,
        nSingles = 1
      }
    | m <- [1 .. 4]
  ]
    ++
    -- Waiting on a single tile to complete a meld or sequence
    [ HandShape
        { nPairs = 1 + m', -- 1 pair OR 1 pair + 1 unfinished meld
          nMelds = m,
          nFourMelds = 0,
          nPartialSequences = 1 - m',
          nSequences = 3 - m,
          nSingles = 0
        }
      | m' <- [0, 1],
        m <- [1 .. 3]
    ]

-- | Constructs a hand in canonical order or fails with an `InvalidHand` error.
mkHand :: [Tile] -> Either InvalidHand Hand
mkHand tiles@(_1 : _2 : _3 : _4 : _5 : _6 : _7 : _8 : _9 : _10 : _11 : _12 : _13 : ts) =
  case ts of
    [] -> case moreThan4In tiles of
      Just repeatedTiles -> Left $ MoreThanFour repeatedTiles
      Nothing ->
        Right $
          Hand
            { pairs = [],
              melds = [],
              fourMelds = [],
              partialSequences = [],
              sequences = [],
              singles = sort tiles
            }
    excess -> Left $ ExcessTiles excess
  where
    moreThan4In :: [Tile] -> Maybe [Tile]
    moreThan4In =
      traverse
        ( \case
            (x : _ : _ : _ : _ : _) -> Just x
            _ -> Nothing
        )
        . group
mkHand missing = Left $ MissingTiles (13 - length missing)

-- Utils for pattern matching parts of the hand
hasPairs :: Term [Tile] -> Term Hand -> Goal ()
hasPairs pairs = matche & on _LogicHand (\(pairs', _, _, _, _, _) -> pairs' === pairs)

hasMelds :: Term [Tile] -> Term Hand -> Goal ()
hasMelds melds = matche & on _LogicHand (\(_, melds', _, _, _, _) -> melds' === melds)

hasFourMelds :: Term [Tile] -> Term Hand -> Goal ()
hasFourMelds fourMelds = matche & on _LogicHand (\(_, _, fourMelds', _, _, _) -> fourMelds' === fourMelds)

hasPartialSeqs :: Term [Tile] -> Term Hand -> Goal ()
hasPartialSeqs partialSeqs = matche & on _LogicHand (\(_, _, _, partialSeqs', _, _) -> partialSeqs' === partialSeqs)

hasSeqs :: Term [Tile] -> Term Hand -> Goal ()
hasSeqs seqs = matche & on _LogicHand (\(_, _, _, _, seqs', _) -> seqs' === seqs)

hasSingles :: Term [Tile] -> Term Hand -> Goal ()
hasSingles singles = matche & on _LogicHand (\(_, _, _, _, _, singles') -> singles' === singles)

hasSets :: (Term [Tile], Term [Tile], Term [Tile], Term [Tile], Term [Tile], Term [Tile]) -> Term Hand -> Goal ()
hasSets (pairs', melds', fourMelds', partialSeqs', seqs', singles') =
  matche & on _LogicHand \(pairs, melds, fourMelds, partialSeqs, seqs, singles) -> do
    pairs === pairs'
    melds === melds'
    fourMelds === fourMelds'
    partialSeqs === partialSeqs'
    seqs === seqs'
    singles === singles'

isSequence :: Unary -> Term [Tile] -> Goal ()
-- ^ The provided tiles form a sequence of the given length.
--
-- __Example: which are the sequences of suit Pin?__
-- >>> import Data.Maybe (mapMaybe)
-- >>> mapMaybe extract' $ run $ \sq -> do; sq & isSequence 3; sq & (hasHead .& hasSuit (inject' Pin))
-- [[P1,P2,P3],[P2,P3,P4],[P3,P4,P5],[P4,P5,P6],[P5,P6,P7],[P6,P7,P8],[P7,P8,P9]]
--
-- __Example: which sequences start with 4?__
-- >>> mapMaybe extract' $ run $ \sq -> do; sq & isSequence 3; suit <- fresh; suit & isAny; sq & (hasHead .& hasNumberSuit (inject' N4) suit)
-- [[P4,P5,P6],[S4,S5,S6],[M4,M5,M6]]
--
-- __Example: which tiles complete the sequence?__
-- >>> let sq' = sequenceList $ inject' <$> [M2, M3]
-- >>> mapMaybe extract' $ run $ \x -> do; sq <- fresh; sq & isSequence 3; select x sq sq'
-- [M1,M4]
isSequence l ts = do
  let len = inject' l
  (ns, s) <- fresh
  ts & hasLength len
  ns & strictlyIncreases
  ts & mapsToWith ns (`hasNumberSuit` s)

isMeld :: Unary -> Term [Tile] -> Goal ()
-- ^ All the provided tiles form a meld of the given length
--
-- __Example: is [P3, P3, P3] a meld?__
-- >>> run \() -> sequenceList (inject' <$> replicate 3 P3) & isMeld 3
-- [()]
--
-- __Example: what is the meld of length 4 containing an eastern wind?__
-- >>> import Data.Maybe (mapMaybe)
-- >>> mapMaybe extract' $ run $ \ls -> do; ls & isMeld 4; ls & hasHead (inject' We)
-- [[We,We,We,We]]
isMeld l ts = do
  disj
    do
      inject' l & isZero
      ts & isNil
    do
      (h, t, l') <- fresh
      inject' l & isSuccOf l'
      ts & hasHeadTail h t
      t & hasLength l'
      t & allSatisfy (=== h)

-- Sorts the hand into its canonical shape
-- >>> let (Right hand) = mkHand [ M1, M3, M2, M5, M6, P1, P3, We, We, We, Dw, S8, S5 ]
-- >>> hand
-- >>> run $ \() -> inject' hand & isOrderedAs (inject' hand)
-- Hand {pairs = [], melds = [], fourMelds = [], partialSequences = [], sequences = [], singles = [P1,P3,S5,S8,M1,M2,M3,M5,M6,Dw,We,We,We]}
-- []
isOrderedAs :: Term Hand -> Term Hand -> Goal ()
isOrderedAs ordHand hand =
  hand
    & ( matche
          & on
            _LogicHand
            ( \(pairs, melds, fourMelds, partialSeqs, seqs, singles) ->
                ordHand
                  & ( matche
                        & on
                          _LogicHand
                          ( \(oPairs, oMelds, oFourMelds, oPartialSeqs, oSeqs, oSingles) -> do
                              conjMany $
                                uncurry (insertSort compareP)
                                  <$> [ (pairs, oPairs),
                                        (melds, oMelds),
                                        (fourMelds, oFourMelds),
                                        (partialSeqs, oPartialSeqs),
                                        (seqs, oSeqs),
                                        (singles, oSingles)
                                      ]
                          )
                    )
            )
      )

{--
  Re-shaping rules

 These rules allow us to convert between equivalent hands that have different
 shapes, but are otherwise equivalent (i.e: they have the same tiles). In general,
 a hand can be located in a spectrum between two extremes:

  * A meld-heavy form
  * A sequence-heavy form

 The first form has the maximum number of melds possible, while the second has
 the maximum number of sequences possible. It is possible to move in one direction
 or the other by applying these re-shaping rules in one direction or the other.

 The most neutral form is the the one possessing 13 single tiles, since there are
 no melds or sequences.

--}

-- | The first hand is related to the second if a pair of identical singles
-- from the first are converted into a pair in the second.
--
-- >>> let (Right h1) = mkHand [P1, P2, P3, P4, P5, P6, P7, P8, P9, M1, We, We, Ww ]
-- >>> run $ \() -> do; h' <- fresh; singlesPair (inject' h1) h'
-- [()]
singlesPair :: Term Hand -> Term Hand -> Goal ()
singlesPair singlesHand pairHand = do
  (singles, singles', pairs, pairs') <- fresh
  -- Match singles and pairs from each hand
  singlesHand & hasSingles singles
  singlesHand & hasPairs pairs
  pairHand & hasSingles singles'
  pairHand & hasPairs pairs'
  -- Two identical tiles are removed
  (t, singlesMinus1) <- fresh
  select t singles singlesMinus1
  select t singlesMinus1 singles'
  -- One pair is added
  insertOrd compareP t pairs pairs'

-- The first hand is related to the second if a pair of identical singles
-- from the first are converted into a pair in the second.
--
-- >>> let (Right h1) = mkHand [P1, P2, P3, P4, P5, P6, P7, P8, P9, M1, We, We, Dw ]
-- >>> length $ run $ \() -> do; h' <- fresh; singlesPartialSeq (inject' h1) h'
-- 8
singlesPartialSeq :: Term Hand -> Term Hand -> Goal ()
singlesPartialSeq singlesHand seqHand = do
  (singles, singles', partialSeqs, partialSeqs') <- fresh
  singlesHand & hasSingles singles
  singlesHand & hasPartialSeqs partialSeqs
  seqHand & hasSingles singles'
  seqHand & hasPartialSeqs partialSeqs'
  (t1, t2, suit) <- fresh
  -- t1 and t2 must be numbered tiles and consecutive
  t1 & hasSuit suit
  t2 & hasSuit suit
  selectSuccessivePair t1 t2 singles
  -- t1 and t2 must be removed from singles
  singlesMinus1 <- fresh
  select t1 singles singlesMinus1
  select t2 singles singles'
  -- A partial sequence is added
  insertOrd compareP t1 partialSeqs partialSeqs'
  where
    selectSuccessivePair :: Term Tile -> Term Tile -> Term [Tile] -> Goal ()
    selectSuccessivePair t1 t2 ts = do
      (x, xs, y, ys) <- fresh
      ts & hasHeadTail x xs
      xs & hasHeadTail y ys
      disj
        do
          x === t1
          y === t2
        do
          selectSuccessivePair t1 t2 xs

-- -- The hand is one tile away from the other
-- --
-- -- We say a hand is one tile away from the other, if discarding one tile from
-- -- the first hand and picking up another from the wall results in the second hand
-- isOneAwayFrom ::
--   Term Hand     -- ^ Result hand
--   -> Term Hand  -- ^ Original hand
--   -> Goal ()
-- isOneAwayFrom result original = do
--   rSets@(pairs, melds, fourMelds, partialSeqs, seqs, singles) <- fresh; result & hasSets rSets
--   oSets@(pairs', melds', fourMelds', partialSeqs', seqs', singles') <- fresh; original & hasSets oSets
