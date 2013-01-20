{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Pso.Instances where

import Pso.Core

import Data.Ratio
import System.Random

{-
Declarations for fractionals e.g Double, Rational
-}

instance PsoVect Double 
  where
    pAdd = (+)
    pScale = (*)
    pZero = 0
    pSubtract = (-)

instance PsoSized Double 
  where
    dot = (*)

instance Random Rational
  where
    random g = (toRational r, g1)
      where
        rg = random g
        r = fst rg :: Double
        g1 = snd rg
    randomR (a, b) g = (toRational r, g1)
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
instance (PsoVect a, PsoVect b) => Random (a, b) 
  where
    random g = ((x, y), g2) 
      where
        (x, g1) = random g
        (y, g2) = random g1
    randomR ((a1, b1), (a2, b2)) g = ((x, y), g2) 
      where
        (x, g1) = randomR (a1, a2) g
        (y, g2) = randomR (b1, b2) g1

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
instance (PsoVect a, PsoVect b, PsoVect c) => Random (a, b, c) 
  where
    random g = ((x, y, z), g3) 
      where
        (x, g1) = random g
        (y, g2) = random g1
        (z, g3) = random g2
    randomR ((a1, b1, c1), (a2, b2, c2)) g = ((x, y, z), g3) 
      where
        (x, g1) = randomR (a1, a2) g
        (y, g2) = randomR (b1, b2) g1
        (z, g3) = randomR (c1, c2) g2

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
instance (PsoVect a, PsoVect b, PsoVect c, PsoVect d) => Random (a, b, c, d)
  where
    random g = ((w, x, y, z), g4) 
      where
        (w, g1) = random g
        (x, g2) = random g1
        (y, g3) = random g2
        (z, g4) = random g3
    randomR ((a1, b1, c1, d1), (a2, b2, c2, d2)) g = ((w, x, y, z), g4) 
      where
        (w, g1) = randomR (a1, a2) g
        (x, g2) = randomR (b1, b2) g1
        (y, g3) = randomR (c1, c2) g2
        (z, g4) = randomR (d1, d2) g3

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
instance (PsoVect a, PsoVect b, PsoVect c, PsoVect d, PsoVect e) => Random (a, b, c, d, e)
  where
    random g = ((v, w, x, y, z), g5) 
      where
        (v, g1) = random g
        (w, g2) = random g1
        (x, g3) = random g2
        (y, g4) = random g3
        (z, g5) = random g4
    randomR ((a1, b1, c1, d1, e1), (a2, b2, c2, d2, e2)) g = ((v, w, x, y, z), g5) 
      where
        (v, g1) = randomR (a1, a2) g
        (w, g2) = randomR (b1, b2) g1
        (x, g3) = randomR (c1, c2) g2
        (y, g4) = randomR (d1, d2) g3
        (z, g5) = randomR (e1, e2) g4

instance (PsoVect a, PsoVect b, PsoVect c, PsoVect d, PsoVect e) => PsoVect (a, b, c, d, e) 
  where
    pAdd (v1, w1, x1, y1, z1) (v2, w2, x2, y2, z2) = (pAdd v1 v2, pAdd w1 w2, pAdd x1 x2, pAdd y1 y2, pAdd z1 z2)
    pScale r (v, w, x, y, z) = (pScale r v, pScale r w, pScale r x, pScale r y, pScale r z)
    pZero = (pZero, pZero, pZero, pZero, pZero)

instance (PsoSized a, PsoSized b, PsoSized c, PsoSized d, PsoSized e) => PsoSized (a, b, c, d, e)
  where
    (v1, w1, x1, y1, z1) `dot` (v2, w2, x2, y2, z2) = v1 `dot` v2 + w1 `dot` w2 + x1 `dot` x2 + y1 `dot` y2 + z1 `dot` z2

-- Grades
instance Grade Double 
  where
    betterThan = (<)

instance Grade Rational
  where
    betterThan = (<)

instance Grade Int 
  where
    betterThan = (<)

instance Grade Integer 
  where
    betterThan = (<)

instance Grade Char 
  where
    betterThan = (<)

-- In this case, it turns out that @Nothing@ is worse than @Nothing@,
-- but that should matter little.
instance (Grade a) => Grade (Maybe a) 
  where
    Nothing  `betterThan` _        = False
    _        `betterThan` Nothing  = True
    (Just x) `betterThan` (Just y) = x `betterThan` y

