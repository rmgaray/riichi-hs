{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Tile
  ( -- | Tile types
    Number (..),
    Suit (..),
    Dragon (..),
    Wind (..),
    Tile,
    -- Logic tile types
    LogicNumber (..),
    LogicSuit (..),
    LogicNumbered (..),
    LogicDragon (..),
    LogicWind (..),
    LogicTile (..),
    -- Predicates
    hasNumberSuit,
    hasSuit,
    hasNumber,
    isDragon,
    isWind,
    compareP,
    -- Patterns
    pattern P1,
    pattern P2,
    pattern P3,
    pattern P4,
    pattern P5,
    pattern P6,
    pattern P7,
    pattern P8,
    pattern P9,
    pattern S1,
    pattern S2,
    pattern S3,
    pattern S4,
    pattern S5,
    pattern S6,
    pattern S7,
    pattern S8,
    pattern S9,
    pattern M1,
    pattern M2,
    pattern M3,
    pattern M4,
    pattern M5,
    pattern M6,
    pattern M7,
    pattern M8,
    pattern M9,
    pattern Px,
    pattern Sx,
    pattern Mx,
    pattern NN,
    pattern Dw,
    pattern Dr,
    pattern Dg,
    pattern DD,
    pattern We,
    pattern Ws,
    pattern Ww,
    pattern Wn,
    pattern WW,
  )
where

import Control.Lens
import GHC.Generics (Generic)
import Kanren.Core
import Kanren.Goal
import Kanren.Match
import Kanren.TH
import Utils (beforeOrEqualOrd)

data Number = N1 | N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9
  deriving stock (Generic, Show, Eq, Enum, Bounded, Ord)

data Suit = Pin | Sou | Man
  deriving stock (Generic, Show, Eq, Enum, Bounded, Ord)

data Numbered = Numbered Suit Number
  deriving stock (Generic, Show, Eq, Ord)

data Wind = East | South | West | North
  deriving stock (Generic, Show, Eq, Enum, Bounded, Ord)

data Dragon = White | Green | Red
  deriving stock (Generic, Show, Eq, Enum, Bounded, Ord)

data Tile
  = NumberTile Numbered
  | WindTile Wind
  | DragonTile Dragon
  deriving stock (Generic, Eq)

-- Make logical version of all types and their prisms

makeLogicals [''Number, ''Suit, ''Numbered, ''Wind, ''Dragon, ''Tile]
makePrisms ''LogicNumber
makePrisms ''LogicSuit
makePrisms ''LogicNumbered
makePrisms ''LogicWind
makePrisms ''LogicDragon
makePrisms ''LogicTile

deriving stock instance Show LogicNumber

deriving stock instance Show LogicSuit

deriving stock instance Show LogicNumbered

deriving stock instance Show LogicWind

deriving stock instance Show LogicDragon

deriving stock instance Show LogicTile

-- Replacements for relational pattern matching...
hasNumberSuit :: Term Number -> Term Suit -> Term Tile -> Goal ()
hasNumberSuit n s t =
  t
    & ( matche
          & on
            _LogicNumberTile
            ( \nn ->
                nn & (matche & on _LogicNumbered (\(s', n') -> n === n' >> s === s'))
            )
      )

hasSuit :: Term Suit -> Term Tile -> Goal ()
hasSuit s nn = do n' <- fresh; nn & hasNumberSuit n' s

hasNumber :: Term Number -> Term Tile -> Goal ()
hasNumber n nn = do s' <- fresh; nn & hasNumberSuit n s'

isDragon :: Term Dragon -> Term Tile -> Goal ()
isDragon d t = t & (matche & on _LogicDragonTile (=== d))

isWind :: Term Wind -> Term Tile -> Goal ()
isWind w t = t & (matche & on _LogicWindTile (=== w))

-- Now we define pattern synonyms for all the tiles...

-- All matched explicitly
{-# COMPLETE P1, P2, P3, P4, P5, P6, P7, P8, P9, S1, S2, S3, S4, S5, S6, S7, S8, S9, M1, M2, M3, M4, M5, M6, M7, M8, M9, Dw, Dg, Dr, We, Ws, Ww, Wn #-}

-- Numbers not matched
{-# COMPLETE Px, Sx, Mx, Dw, Dg, Dr, We, Ws, Ww, Wn #-}

-- Numbers and suits not matched
{-# COMPLETE NN, Dw, Dg, Dr, We, Ws, Ww, Wn #-}

-- Numbers not matched, dragons not matched
{-# COMPLETE NN, DD, We, Ws, Ww, Wn #-}

-- Numbers not matched, winds not matched
{-# COMPLETE NN, Dw, Dg, Dr, WW #-}

-- Neither numbers, nor dragons, nor winds are matched
{-# COMPLETE NN, DD, WW #-}

-- (There are more, I will add them if they are needed)

-----------------------------
-- Numbered tiles synonyms --
-----------------------------

-- Abbreviations --
pattern P1, P2, P3, P4, P5, P6, P7, P8, P9 :: Tile
pattern P1 = NumberTile (Numbered Pin N1)
pattern P2 = NumberTile (Numbered Pin N2)
pattern P3 = NumberTile (Numbered Pin N3)
pattern P4 = NumberTile (Numbered Pin N4)
pattern P5 = NumberTile (Numbered Pin N5)
pattern P6 = NumberTile (Numbered Pin N6)
pattern P7 = NumberTile (Numbered Pin N7)
pattern P8 = NumberTile (Numbered Pin N8)
pattern P9 = NumberTile (Numbered Pin N9)

pattern S1, S2, S3, S4, S5, S6, S7, S8, S9 :: Tile
pattern S1 = NumberTile (Numbered Sou N1)
pattern S2 = NumberTile (Numbered Sou N2)
pattern S3 = NumberTile (Numbered Sou N3)
pattern S4 = NumberTile (Numbered Sou N4)
pattern S5 = NumberTile (Numbered Sou N5)
pattern S6 = NumberTile (Numbered Sou N6)
pattern S7 = NumberTile (Numbered Sou N7)
pattern S8 = NumberTile (Numbered Sou N8)
pattern S9 = NumberTile (Numbered Sou N9)

pattern M1, M2, M3, M4, M5, M6, M7, M8, M9 :: Tile
pattern M1 = NumberTile (Numbered Man N1)
pattern M2 = NumberTile (Numbered Man N2)
pattern M3 = NumberTile (Numbered Man N3)
pattern M4 = NumberTile (Numbered Man N4)
pattern M5 = NumberTile (Numbered Man N5)
pattern M6 = NumberTile (Numbered Man N6)
pattern M7 = NumberTile (Numbered Man N7)
pattern M8 = NumberTile (Numbered Man N8)
pattern M9 = NumberTile (Numbered Man N9)

-- Number matchers --
pattern Px, Sx, Mx :: Number -> Tile
pattern Px n = NumberTile (Numbered Pin n)
pattern Sx n = NumberTile (Numbered Sou n)
pattern Mx n = NumberTile (Numbered Man n)

-- (suit matchers not very useful, so we omit them)

-- Number and suit matched
pattern NN :: Number -> Suit -> Tile
pattern NN n s = NumberTile (Numbered s n)

--------------------------
-- Honor tiles synonyms --
--------------------------

-- Abbreviations --
pattern Dw, Dg, Dr, We, Ws, Ww, Wn :: Tile
pattern Dw = DragonTile White
pattern Dg = DragonTile Green
pattern Dr = DragonTile Red
pattern We = WindTile East
pattern Ws = WindTile South
pattern Ww = WindTile West
pattern Wn = WindTile North

-- Dragon matched
pattern DD :: Dragon -> Tile
pattern DD d = DragonTile d

-- Wind matched
pattern WW :: Wind -> Tile
pattern WW w = WindTile w

-- | This is the canonical ordering for tiles.
--
-- The ordering between tiles of different categories is:
--
-- Numbered tiles < Dragon tiles < Wind tiles
--
-- The order inside each category is the stock derived 'Ord' instance for
-- 'Suit', 'Number', 'Dragon' and 'Wind'.
--
-- In the case of numbered tiles, they are first compared by suit, then
-- by number.
--
-- Examples:
-- >>> compare P3 M1
-- >>> compare P4 P9
-- >>> compare Dw S9
-- >>> compare Dw We
-- LT
-- LT
-- GT
-- LT
instance Ord Tile where
  compare :: Tile -> Tile -> Ordering
  compare (NN n1 s1) (NN n2 s2)
    | s1 == s2 = compare n1 n2
    | otherwise = compare s1 s2
  compare (NN _ _) _ = LT
  compare (DD d1) (DD d2) = compare d1 d2
  compare (DD _) (WW _) = LT
  compare (DD _) (NN _ _) = GT
  compare (WW w1) (WW w2) = compare w1 w2
  compare (WW _) _ = GT

-- | Relational equivalent of the 'Ord' instance for 'Tile'
compareP :: Term Tile -> Term Tile -> Goal ()
compareP t1 t2 =
  disjMany
    [ -- First is numbered
      do
        (n1, s1) <- fresh
        t1 & hasNumberSuit n1 s1
        disjMany
          [ do
              -- Numbered tiles go first
              dd <- fresh
              t2 & isDragon dd,
            do
              -- Numbered tiles go first
              ww <- fresh
              t2 & isWind ww,
            do
              -- Suit goes first, number goes later
              (s2, n2) <- fresh
              t2 & hasNumberSuit n2 s2
              s1 `beforeOrEqualOrd` s2
              n1 `beforeOrEqualOrd` n2
          ],
      -- First is dragon
      do
        d1 <- fresh
        t1 & isDragon d1
        disj
          do
            -- Dragon comes before wind
            w <- fresh
            t2 & isWind w
          do
            d2 <- fresh
            t2 & isDragon d2
            d1 `beforeOrEqualOrd` d2,
      -- Both are winds
      do
        (w1, w2) <- fresh
        t1 & isWind w1
        t2 & isWind w2
        w1 `beforeOrEqualOrd` w2
        -- Otherwise, it is not
    ]

instance Show Tile where
  show = \case
    P1 -> "P1"
    P2 -> "P2"
    P3 -> "P3"
    P4 -> "P4"
    P5 -> "P5"
    P6 -> "P6"
    P7 -> "P7"
    P8 -> "P8"
    P9 -> "P9"
    S1 -> "S1"
    S2 -> "S2"
    S3 -> "S3"
    S4 -> "S4"
    S5 -> "S5"
    S6 -> "S6"
    S7 -> "S7"
    S8 -> "S8"
    S9 -> "S9"
    M1 -> "M1"
    M2 -> "M2"
    M3 -> "M3"
    M4 -> "M4"
    M5 -> "M5"
    M6 -> "M6"
    M7 -> "M7"
    M8 -> "M8"
    M9 -> "M9"
    Dw -> "Dw"
    Dg -> "Dg"
    Dr -> "Dr"
    We -> "We"
    Ws -> "Ws"
    Ww -> "Ww"
    Wn -> "Wn"
