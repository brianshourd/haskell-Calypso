{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

{- |
This module defines instances for the typeclass @Grade@. The types

    * Double
    * Float
    * Rational
    * Int
    * Integer
    * Char

are all graded according to @(<)@ (smaller is better).

The type @Maybe a@ is graded so that @Nothing@ is worse than @Just x@
for any @x@.
-}

module Pso.Instance.Grade where

import Pso.Core

import Data.Ratio

instance Grade Double 
  where
    betterThan = (<)

instance Grade Float
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

