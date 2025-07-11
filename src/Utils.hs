{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Utils
  ( -- Unary type
    Unary,
    unary,
    ununary,
    isSuccOf,
    isZero,
    -- List predicates
    isNil,
    isSingleton,
    hasHeadTail,
    hasTail,
    hasHead,
    hasLength,
    freshListN,
    sequenceList,
    allSatisfy,
    append,
    insert,
    insertOrd,
    insertSort,
    member,
    select,
    selectOrd,
    mapsToWith,
    -- Order predicates
    beforeOrEqual,
    beforeOrEqualOrd,
    beforeOrEqualL,
    before,
    beforeOrd,
    beforeL,
    isSortedBy,
    strictlyIncreases,
    doesNotDecrease,
    -- Maybe predicates
    isJust,
    isNothing,
    -- Others
    (.&.),
    (.&),
    (&.),
    isAny,
  )
where

import Control.Applicative ((<|>))
import Data.Coerce (Coercible, coerce)
import Data.Function ((&))
import Data.List (unfoldr)
import Kanren.Core
import Kanren.Goal
import Kanren.LogicalBase
  ( LogicList (LogicCons),
    _LogicCons,
    _LogicJust,
    _LogicNil,
    _LogicNothing,
  )
import Kanren.Match
import Kanren.TH

instance Logical () where
  type Logic () = ()
  unify _ _ = Just
  walk _ _ = ()
  occursCheck _ _ _ = False
  inject _ = ()
  extract _ = Just ()

-- Unary representation
newtype Unary = Unary [()]
  deriving stock (Eq)

makeLogicals [''Unary]

instance Num Unary where
  fromInteger 0 = Unary []
  fromInteger x | x > 0 = Unary $ () : coerce (fromInteger @Unary (x - 1))
  fromInteger _ = error "Unary numbers cannot be negative"

  (+) = undefined
  (*) = undefined
  abs = id
  signum = undefined
  negate = undefined

instance Show Unary where
  show u = show (ununary u)

instance Enum Unary where
  toEnum = Unary . flip replicate ()
  fromEnum (Unary ls) = length ls

deriving instance Show LogicUnary

unary :: Int -> Term [()]
unary n = inject' (replicate n ())

ununary :: Unary -> Int
ununary (Unary ls) = length ls

isZero :: Term Unary -> Goal ()
isZero = isNil . coerceTerm

isSuccOf :: Term Unary -> Term Unary -> Goal ()
isSuccOf y x = coerceTerm x & hasTail (coerceTerm y)

coerceTerm :: (Coercible a b, Coercible (Logic a) (Logic b)) => Term a -> Term b
coerceTerm (Var i) = Var $ coerce i
coerceTerm (Value lv) = Value $ coerce lv

-- List predicates

-- | Creates a list of length n
freshListN :: forall a. (Logical a) => Int -> Goal (Term [a])
freshListN n =
  if n == 0
    then pure $ inject' []
    else do
      x <- fresh
      xs <- freshListN (n - 1)
      pure $ Value $ LogicCons x xs

sequenceList :: forall a. (Logical a) => [Term a] -> Term [a]
sequenceList = foldr (\t ts -> Value $ LogicCons t ts) (inject' [])

isNil :: forall a. (Logical a) => Term [a] -> Goal ()
isNil l = l & (matche & on _LogicNil pure)

isSingleton :: forall a. (Logical a) => Term a -> Term [a] -> Goal ()
isSingleton x xs = sequenceList [x] === xs

hasHeadTail :: forall a. (Logical a) => Term a -> Term [a] -> Term [a] -> Goal ()
hasHeadTail h t l = l & (matche & on _LogicCons (\(h', t') -> h === h' >> t === t'))

hasHead :: forall a. (Logical a) => Term a -> Term [a] -> Goal ()
hasHead h ls = do t <- fresh; ls & hasHeadTail h t

hasTail :: forall a. (Logical a) => Term [a] -> Term [a] -> Goal ()
hasTail t ls = do h <- fresh; ls & hasHeadTail h t

hasLength :: forall a. (Logical a) => Term Unary -> Term [a] -> Goal ()
hasLength n ls =
  disj
    do
      isZero n
      ls & isNil
    do
      (n', t) <- fresh
      n & isSuccOf n'
      ls & hasTail t
      t & hasLength n'

-- | All elements in `xs` satisfy `p`
allSatisfy :: forall a. (Logical a) => (Term a -> Goal ()) -> Term [a] -> Goal ()
allSatisfy p xs =
  disj
    do xs & isNil
    do
      (x, xs') <- fresh
      xs & hasHeadTail x xs'
      p x
      xs' & allSatisfy p

-- | All elements in `xs` are related pointwise to the elements of `ys` with `p`.
mapsToWith :: forall a b. (Logical a, Logical b) => Term [b] -> (Term b -> Term a -> Goal ()) -> Term [a] -> Goal ()
mapsToWith ys p xs =
  disj
    do
      xs & isNil
      ys & isNil
    do
      (x, y, xs', ys') <- fresh
      xs & hasHeadTail x xs'
      ys & hasHeadTail y ys'
      p y x
      xs' & mapsToWith ys' p

append :: forall a. (Logical a) => Term [a] -> Term [a] -> Term [a] -> Goal ()
append xs ys zs =
  disj
    do
      xs & isNil
      ys === zs
    do
      (x, xs', zs') <- fresh
      xs & hasHeadTail x xs'
      zs & hasHeadTail x zs'
      append xs' ys zs'

insert :: forall a. (Logical a) => Term a -> Term [a] -> Term [a] -> Goal ()
insert z xs ys = do
  disj
    do
      ys & hasHeadTail z xs
    do
      (x, xs', ys') <- fresh
      xs & hasHeadTail x xs'
      ys & hasHeadTail x ys'
      z =/= x
      insert z xs' ys'

select :: forall a. (Logical a) => Term a -> Term [a] -> Term [a] -> Goal ()
select z xs ys = insert z ys xs

selectOrd :: (Logical a) => (Term a -> Term a -> Goal ()) -> Term a -> Term [a] -> Term [a] -> Goal ()
selectOrd ord z xs ys = insertOrd ord z ys xs

member :: forall a. (Logical a) => Term a -> Term [a] -> Goal ()
member z xs = do
  (x, xs') <- fresh
  xs & hasHeadTail x xs'
  disj
    do
      x === z
    do
      x =/= z
      member z xs'

-- | A list is sorted if, for each pair (`x`, `y`) of successive items of the
-- | list, `x lt y` holds.
-- | NOTE: `lt` does not need to be a partial or total order. See `strictlyIncreases`
-- | and `doesNotDecrease`.
isSortedBy :: forall a. (Logical a) => (Term a -> Term a -> Goal ()) -> Term [a] -> Goal ()
isSortedBy lt ls =
  disj
    do
      isNil ls -- empty lists are sorted
    do
      (x, xs) <- fresh
      ls & hasHeadTail x xs
      disj
        do xs & isNil -- singleton lists are sorted
        do
          y <- fresh
          xs & hasHead y
          x `lt` y
          isSortedBy lt xs

-- | A ternary relation (x, ys, zs), where `zs` is the list `ys` with `x` inserted
-- while preserving the order `ord`. The list `ys` is assumed to be ordered.
insertOrd :: forall a. (Logical a) => (Term a -> Term a -> Goal ()) -> Term a -> Term [a] -> Term [a] -> Goal ()
insertOrd ord x ys zs =
  disj
    do
      ys & isNil
      zs & isSingleton x
    do
      (y, ys') <- fresh
      ys & hasHeadTail y ys'
      disj
        do
          ord x y
          zs & hasHeadTail x ys
        do
          y =/= x
          ord y x
          zs' <- fresh
          insertOrd ord x ys' zs'
          zs & hasHeadTail y zs'

-- TODO: Maybe use a more efficient sort?
insertSort :: forall a. (Logical a) => (Term a -> Term a -> Goal ()) -> Term [a] -> Term [a] -> Goal ()
insertSort ord xs ys =
  disj
    do
      xs & isNil; ys & isNil
    do
      (x, xs', sortedXs') <- fresh
      xs & hasHeadTail x xs'
      insertSort ord xs' sortedXs'
      insertOrd ord x sortedXs' ys

-- ex = run \ls -> insertSort beforeOrEqualOrd ls (i [N1, N2, N3, N5, N8])

-- Order predicates

-- | Checks that `x` is the predecessor of `y`, or is equal to it.
-- | NOTE: This is not a partial order, it is not transitive!
beforeOrEqual :: forall a. (Logical a, Eq a, Enum a, Bounded a) => Term a -> Term a -> Goal ()
x `beforeOrEqual` y = x `before` y <|> x === y

-- | Checks that `x` comes before `y`, or is equal to it.
-- | This is a total order.
beforeOrEqualOrd :: forall a. (Logical a, Eq a, Enum a, Bounded a) => Term a -> Term a -> Goal ()
x `beforeOrEqualOrd` y = x `beforeOrd` y <|> x === y

-- | Checks that `x` comes before `y`.
-- | This is a partial order.
beforeOrd :: forall a. (Logical a, Eq a, Enum a, Bounded a) => Term a -> Term a -> Goal ()
x `beforeOrd` y = disjMany $ (\(x', y') -> x === inject' x' >> y === inject' y') <$> orderedPairs
  where
    orderedPairs :: [(a, a)]
    orderedPairs =
      concat $
        unfoldr
          ( \(l, h) ->
              if l == h
                then Nothing
                else Just ((l,) <$> enumFromTo (succ l) h, (succ l, h))
          )
          (minBound @a, maxBound @a)

-- | Checks that `x` is the predecessor of `y`
-- | NOTE: This is not a partial order, it is not transitive!
before :: forall a. (Logical a, Eq a, Enum a, Bounded a) => Term a -> Term a -> Goal ()
x `before` y = disjMany $ (\(x', y') -> x === inject' x' >> y === inject' y') <$> orderedPairs
  where
    orderedPairs :: [(a, a)]
    orderedPairs =
      unfoldr
        ( \(l, h) ->
            if l == h
              then Nothing
              else Just ((l, succ l), (succ l, h))
        )
        (minBound, maxBound)

-- | Checks that `x` comes before `y`, but it also loops around.
beforeL :: forall a. (Logical a, Eq a, Enum a, Bounded a) => Term a -> Term a -> Goal ()
beforeL x y =
  inject' (maxBound @a) === x
    >> inject' (minBound @a) === y
      <|> x `before` y

-- | Checks that `x` comes before or is equal to `y`, but it also loops around.
beforeOrEqualL :: forall a. (Logical a, Eq a, Enum a, Bounded a) => Term a -> Term a -> Goal ()
beforeOrEqualL x y =
  inject' (maxBound @a) === x
    >> inject' (minBound @a) === y
      <|> x `beforeOrEqual` y

-- | Each element of the list is the successor of the previous one
strictlyIncreases :: forall a. (Logical a, Eq a, Enum a, Bounded a) => Term [a] -> Goal ()
strictlyIncreases = isSortedBy before

-- | Each element of the list is the successor of OR equals the previous one
doesNotDecrease :: forall a. (Logical a, Eq a, Enum a, Bounded a) => Term [a] -> Goal ()
doesNotDecrease = isSortedBy beforeOrEqual

-- Maybe predicates

isNothing :: forall a. (Logical a) => Term (Maybe a) -> Goal ()
isNothing m = m & (matche & on _LogicNothing pure)

isJust :: forall a. (Logical a) => Term a -> Term (Maybe a) -> Goal ()
isJust v m = m & (matche & on _LogicJust (=== v))

-- Predicate helpers

-- | For two predicates `p` and `q`, `(p .&. q) c a` is satisfied if some
-- | `b` exists such that `p b a` and `q c b` hold at the same time.
(.&.) ::
  forall a b c.
  (Logical a, Logical b, Logical c) =>
  (Term b -> Term a -> Goal ()) ->
  (Term c -> Term b -> Goal ()) ->
  Term c ->
  Term a ->
  Goal ()

infixl 1 .&.

p .&. q = \c a -> do
  b <- fresh
  p b a
  q c b

-- | For two predicates `p` and `q`, `(p .&. q) a` is satisfied if some
-- | `b` exists such that `p b a` and `q b` hold at the same time.
(.&) ::
  forall a b.
  (Logical a, Logical b) =>
  (Term b -> Term a -> Goal ()) ->
  (Term b -> Goal ()) ->
  Term a ->
  Goal ()
p .& q = \a -> do
  b <- fresh
  p b a
  q b

-- | Like (.&), with the order of parameters reversed.
(&.) ::
  forall a b.
  (Logical a, Logical b) =>
  (Term b -> Goal ()) ->
  (Term b -> Term a -> Goal ()) ->
  Term a ->
  Goal ()
q &. p = \a -> do
  b <- fresh
  p b a
  q b

-- | Unifies with all possible values
isAny :: forall a. (Enum a, Bounded a, Logical a) => Term a -> Goal ()
isAny a = disjMany $ (a ===) . inject' <$> enumFrom (minBound @a)
