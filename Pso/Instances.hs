module Pso.Instances where

import Pso.Core

import System.Random

-- ===============
-- Instances
-- ===============

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

{-
instance PsoVect Rational 
  where
    pAdd = (+)
    pScale r = (*) (toRational r)
    pZero = 0
    pSubtract = (-)
-}

{- 
Declaration for pairs of PsoVects e.g. (Double, Double), (Float,
Float), (Rational, Rational), etc.

Mathematically, this is the direct sum of vector spaces.
-}
instance (PsoVect a, PsoVect b, Random a, Random b) => Random (a, b) 
  where
    random g = ((x, y), g'') 
      where
        (x, g')  = random g
        (y, g'') = random g'
    randomR ((a1, b1), (a2, b2)) g = ((x, y), g'') 
      where
        (x, g')  = randomR (a1, a2) g
        (y, g'') = randomR (b1, b2) g'

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
instance (PsoVect a, PsoVect b, PsoVect c, Random a, Random b, Random c) => Random (a, b, c) 
  where
    random g = ((x, y, z), g''') 
      where
        (x, g')   = random g
        (y, g'')  = random g'
        (z, g''') = random g''
    randomR ((a1, b1, c1), (a2, b2, c2)) g = ((x, y, z), g''') 
      where
        (x, g')   = randomR (a1, a2) g
        (y, g'')  = randomR (b1, b2) g'
        (z, g''') = randomR (c1, c2) g''

instance (PsoVect a, PsoVect b, PsoVect c) => PsoVect (a, b, c) 
  where
    pAdd (x1, y1, z1) (x2, y2, z2) = (pAdd x1 x2, pAdd y1 y2, pAdd z1 z2)
    pScale r (x,y,z) = (pScale r x, pScale r y, pScale r z)
    pZero = (pZero,pZero,pZero)

instance (PsoSized a, PsoSized b, PsoSized c) => PsoSized (a, b, c) 
  where
    (x1, y1, z1) `dot` (x2, y2, z2) = x1 `dot` x2 + y1 `dot` y2 + z1 `dot` z2

-- Grades
instance Grade Double 
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

