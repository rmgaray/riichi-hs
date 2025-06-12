{-# LANGUAGE TemplateHaskell #-}

module Hand
  ( Hand,
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

-- | A hand of Mahjong containing 13 tiles.
--
-- Hands are ALWAYS ordered. Because of this, hands with the same tiles can
-- always be compared with each other for equality.
data Hand = Hand
  { pairs :: [Tile],
    melds :: [Tile],
    fourMelds :: [Tile],
    partialSequences :: [Tile],
    sequences :: [Tile],
    singles :: [Tile]
  }
  deriving stock (Generic, Show, Eq)

-- | An error while constructing a hand with 'mkhand'.
data InvalidHand
  = -- | More than 13 tiles were provided, these are in excess
    ExcessTiles [Tile]
  | -- | Less than 13 tiles were provided, this is how many are missing
    MissingTiles Int
  | -- | More than 4 copies of the given tiles were used
    MoreThanFour [Tile]
  deriving stock (Show)

makeLogicals [''Hand]
makePrisms ''LogicHand

deriving stock instance Show LogicHand

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
