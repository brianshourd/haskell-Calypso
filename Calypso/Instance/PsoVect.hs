{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}

{- |
Defines the instances for @PsoVect@ and @PsoSized@. In particular,
tuples of @Float@, @Double@, and @Rational@ are all instances.
-}

module Calypso.Instance.PsoVect 
    (
    PsoList,
    toList,
    fromList
    ) where

import Calypso.Core

import Data.Ratio
import System.Random

{- |
We already have instances of PsoVect for tuples (up to 5-tuples,
including sub-tuples), but sometimes there are more data points than is
convenient to keep track of in a tuple. You want, instead, a list.

We'd like for the type system to keep track of our lists size for us,
but that is just a tuple, and we lose a lot of the freedom of working
with lists. So instead, we use this wrapper, which contains both the
list and the length of the list, ensuring that new @PsoList@s are
created at the same size.

Since the compiler won't check to make sure that all of your @PsoList@s
are the same size, the burden is on the programmer. I'd like to throw
runtime errors when list size mismatches are found, but this interferes
with the constant @pZero@. Thus the @EPsoList@ option, for an empty
list.

Now, when you use PsoList, it will throw errors when the declared sizes
do not match. However, for performance reasons, it never actually checks
to make sure that the size you say is the size of the list. This may or
may not lead to unexpected behavior - be careful.
-}
data PsoList a = EPsoList | PsoList [a] Int deriving Eq

{- |
We hide the constructor for @PsoList@s, so this is the primary way to
get a list from a @PsoList@.
-}
fromList :: [a] -> PsoList a
fromList [] = EPsoList
fromList xs = PsoList xs (length xs)

{- |
We can get a regular list from a @PsoList@ using this function.
-}
toList :: PsoList a -> [a]
toList EPsoList = []
toList (PsoList xs _) = xs

instance (Show a) => Show (PsoList a)
  where
    show EPsoList       = "EPsoList"
    show (PsoList xs _) = show xs

instance Functor PsoList
  where
    fmap f EPsoList = EPsoList
    fmap f (PsoList xs n) = PsoList (fmap f xs) n

{-
This helper function is used to convert an EPsoList to a list of zeros
of the proper size.
-}
zeroList :: (PsoVect a) => Int -> PsoList a
zeroList x = PsoList (replicate x pZero) x

{-
This declaration is really long, but I'm trying hard to catch all the
cases. Hopefully I can reduce it somehow, but for now it works.

Note that we need @a@ to be an instance of @PsoVect@, and not just
@Between@, since we require @pZero@ when dealing with the @EPsoList@. In
practice, I see no reason for instancing a type with the @Between@ type
without instancing a @PsoVect@ type, so this should really never even be
noticed.
-}
instance (PsoVect a) => Between (PsoList a)
  where
    isBetween EPsoList (EPsoList, EPsoList) = True
    isBetween EPsoList (EPsoList, q2@(PsoList _ n)) = True
    isBetween EPsoList (q1@(PsoList _ n), EPsoList) = True
    isBetween EPsoList (q1@(PsoList _ n), q2) = 
        isBetween (zeroList n) (q1, q2)
    isBetween p@(PsoList _ n) (EPsoList, EPsoList) = 
        isBetween p (zeroList n, zeroList n)
    isBetween p@(PsoList _ n) (q1, EPsoList) = 
        isBetween p (q1, zeroList n)
    isBetween p@(PsoList _ n) (EPsoList, q2) = 
        isBetween p (zeroList n, q2)
    isBetween p@(PsoList xs n) (q1@(PsoList ys m1), q2@(PsoList zs m2)) 
        | n == m1 && n == m2 = isBetween' xs (ys, zs)
        | otherwise = error $ "Size mismatch: " ++ show n ++ ", "
            ++ show m1 ++ ", and " ++ show m2 ++ " are not equal."
      where
        isBetween' [] ([], []) = True
        isBetween' (xs) ((ys), (zs)) =
            and $ zipWith3 (\x y z -> isBetween x (y,z)) xs ys zs
    randBetween (EPsoList, EPsoList) g = (EPsoList, g)
    randBetween (q1@(PsoList _ n), EPsoList) g = 
        randBetween (q1, zeroList n) g
    randBetween (EPsoList, q2@(PsoList _ n)) g = 
        randBetween (zeroList n, q2) g
    randBetween (q1@(PsoList xs n), q2@(PsoList ys m)) g
        | n == m = (\(xs, g') -> 
            (PsoList xs n, g')) . foldr go ([], g) $ zip xs ys
        | otherwise = error $ "Size mismatch: " ++ show n ++ " /= " ++
            show m
      where
        go b (acc, gen) = let (z, gen') = randBetween b gen in 
            (z:acc, gen')

instance (PsoVect a) => PsoVect (PsoList a)
  where
    pAdd EPsoList p = p
    pAdd p EPsoList = p
    pAdd (PsoList xs n) (PsoList ys m)
        | m == n = PsoList (zipWith pAdd xs ys) n
        | otherwise = error "Size mismatch in pAdd"
    pScale r x = fmap (pScale r) x
    pZero = EPsoList

instance (PsoSized a) => PsoSized (PsoList a)
  where
    EPsoList `dot` p = 0
    p `dot` EPsoList = 0
    (PsoList xs _) `dot` (PsoList ys _) = sum $ zipWith dot xs ys

{-
Declarations for fractionals e.g Double, Rational
-}
instance Between Double
  where
    isBetween x (a, b) = a <= x && x <= b
    randBetween = randomR

instance PsoVect Double 
  where
    pAdd x y = x + y
    pScale r x = r * x
    pZero = 0
    pSubtract x y = x - y

instance PsoSized Double 
  where
    dot x y = x * y

instance Between Float
  where
    isBetween x (a, b) = a <= x && x <= b
    randBetween = randomR

instance PsoVect Float
  where
    pAdd = (+)
    pScale r = (*) (fromRational . toRational $ r)
    pZero = 0
    pSubtract = (-)

instance PsoSized Float
  where
    dot x y = fromRational . toRational $ x * y

instance Between Rational
  where
{-    random g = (toRational r, g1)
      where
        rg = random g
        r = fst rg :: Double
        g1 = snd rg -}
    isBetween x (a, b) = a <= x && x <= b
    randBetween (a, b) g = (toRational r, g1)
      where
        (r, g1) = randomR (aDouble, bDouble) g
        aDouble = fromRational a :: Double
        bDouble = fromRational b :: Double

instance PsoVect Rational 
  where
    pAdd = (+)
    pScale r = (*) (toRational r)
    pZero = 0
    pSubtract = (-)

instance PsoSized Rational 
  where
    dot x y = (fromRational) (x * y)

{- 
Declaration for pairs of PsoVects e.g. (Double, Double), (Rational, Rational), etc.

Mathematically, this is the direct sum of vector spaces.
-}
instance (Between a, Between b) => Between (a, b)
  where
    isBetween (x, y) ((a1, b1), (a2, b2)) = 
        (isBetween x (a1, a2)) && (isBetween y (b1, b2))
    randBetween ((a1, b1), (a2, b2)) g = ((x, y), g2) 
      where
        (x, g1) = randBetween (a1, a2) g
        (y, g2) = randBetween (b1, b2) g1

instance (PsoVect a, PsoVect b) => PsoVect (a, b) 
  where
    pAdd (x1, y1) (x2, y2) = (pAdd x1 x2, pAdd y1 y2)
    pScale r (x,y) = (pScale r x, pScale r y)
    pZero = (pZero, pZero)

instance (PsoSized a, PsoSized b) => PsoSized (a, b) 
  where
    (x1, y1) `dot` (x2, y2) = x1 `dot` x2 + y1 `dot` y2

{- 
Declaration for triples of PsoVects

Mathematically, this is the direct sum of vector spaces.
-}
instance (Between a, Between b, Between c) => Between (a, b, c)
  where
    isBetween (x, y, z) ((a1, b1, c1), (a2, b2, c2)) = 
        (isBetween x (a1, a2)) 
        && (isBetween y (b1, b2))
        && (isBetween z (c1, c2))
    randBetween ((a1, b1, c1), (a2, b2, c2)) g = ((x, y, z), g3) 
      where
        (x, g1) = randBetween (a1, a2) g
        (y, g2) = randBetween (b1, b2) g1
        (z, g3) = randBetween (c1, c2) g2

instance (PsoVect a, PsoVect b, PsoVect c) => PsoVect (a, b, c) 
  where
    pAdd (x1, y1, z1) (x2, y2, z2) = (pAdd x1 x2, pAdd y1 y2, pAdd z1 z2)
    pScale r (x, y, z) = (pScale r x, pScale r y, pScale r z)
    pZero = (pZero, pZero, pZero)

instance (PsoSized a, PsoSized b, PsoSized c) => PsoSized (a, b, c) 
  where
    (x1, y1, z1) `dot` (x2, y2, z2) = x1 `dot` x2 + y1 `dot` y2 + z1 `dot` z2

{- 
Declaration for quadruples of PsoVects

Mathematically, this is the direct sum of vector spaces.
-}
instance (Between a, Between b, Between c, Between d) => Between (a, b, c, d)
  where
    isBetween (w, x, y, z) ((a1, b1, c1, d1), (a2, b2, c2, d2)) = 
        (isBetween w (a1, a2)) 
        && (isBetween x (b1, b2))
        && (isBetween y (c1, c2))
        && (isBetween z (d1, d2))
    randBetween ((a1, b1, c1, d1), (a2, b2, c2, d2)) g = ((w, x, y, z), g4) 
      where
        (w, g1) = randBetween (a1, a2) g
        (x, g2) = randBetween (b1, b2) g1
        (y, g3) = randBetween (c1, c2) g2
        (z, g4) = randBetween (d1, d2) g3

instance (PsoVect a, PsoVect b, PsoVect c, PsoVect d) => PsoVect (a, b, c, d) 
  where
    pAdd (w1, x1, y1, z1) (w2, x2, y2, z2) = (pAdd w1 w2, pAdd x1 x2, pAdd y1 y2, pAdd z1 z2)
    pScale r (w, x, y, z) = (pScale r w, pScale r x, pScale r y, pScale r z)
    pZero = (pZero, pZero, pZero, pZero)

instance (PsoSized a, PsoSized b, PsoSized c, PsoSized d) => PsoSized (a, b, c, d) 
  where
    (w1, x1, y1, z1) `dot` (w2, x2, y2, z2) = w1 `dot` w2 + x1 `dot` x2 + y1 `dot` y2 + z1 `dot` z2

{- 
Declaration for quadruples of PsoVects

Mathematically, this is the direct sum of vector spaces.
-}
instance (Between a, Between b, Between c, Between d, Between e) => Between (a, b, c, d, e)
  where
    isBetween (v, w, x, y, z) ((a1, b1, c1, d1, e1), (a2, b2, c2, d2, e2)) = 
        (isBetween v (a1, a2)) 
        && (isBetween w (b1, b2))
        && (isBetween x (c1, c2))
        && (isBetween y (d1, d2))
        && (isBetween z (e1, e2))
    randBetween ((a1, b1, c1, d1, e1), (a2, b2, c2, d2, e2)) g = ((v, w, x, y, z), g5) 
      where
        (v, g1) = randBetween (a1, a2) g
        (w, g2) = randBetween (b1, b2) g1
        (x, g3) = randBetween (c1, c2) g2
        (y, g4) = randBetween (d1, d2) g3
        (z, g5) = randBetween (e1, e2) g4

instance (PsoVect a, PsoVect b, PsoVect c, PsoVect d, PsoVect e) => PsoVect (a, b, c, d, e) 
  where
    pAdd (v1, w1, x1, y1, z1) (v2, w2, x2, y2, z2) = (pAdd v1 v2, pAdd w1 w2, pAdd x1 x2, pAdd y1 y2, pAdd z1 z2)
    pScale r (v, w, x, y, z) = (pScale r v, pScale r w, pScale r x, pScale r y, pScale r z)
    pZero = (pZero, pZero, pZero, pZero, pZero)

instance (PsoSized a, PsoSized b, PsoSized c, PsoSized d, PsoSized e) => PsoSized (a, b, c, d, e)
  where
    (v1, w1, x1, y1, z1) `dot` (v2, w2, x2, y2, z2) = v1 `dot` v2 + w1 `dot` w2 + x1 `dot` x2 + y1 `dot` y2 + z1 `dot` z2
