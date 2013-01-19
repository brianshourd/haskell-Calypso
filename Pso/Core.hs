{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

{- | 
A small module to perform optimization using Particle Swarm Optimization
(PSO), based on the so-called /Standard PSO/ defined in
  
Bratton, Daniel, and James Kennedy. \"Defining a standard for particle
swarm optimization.\" Swarm Intelligence Symposium, 2007. SIS 2007. IEEE.
IEEE, 2007.
 
I highly recommend that you find and read this paper if you are
interested in PSO at all. However, the wikipedia entry is also quite
good: <http://en.wikipedia.org/wiki/Particle_swarm_optimization>
Note that we are using a method with /constriction parameters/, while
the wikipedia article puts forward a method using /inertia weight/. The
two are similar, but not identical.
-}

module Pso.Core
    (
    -- * Main Classes
    -- $typeclasses
    PsoVect(..), 
    Grade(..),
    PsoSized(..),
    -- * Easy Method
    -- $easy
    easyOptimize,
    -- * Updaters
    Updater(..),
    -- ** Common Updaters
    -- $updaters
    upDefault,
    upStandard,
    upOriginal,
    upInertiaWeight,
    upInertiaWeightDynamic,
    -- ** Building Blocks
    -- $buildingUpdaters
    upAddLocal,
    upAddLocalDynamic,
    upAddPrivate,
    upAddPrivateDynamic,
    upScale,
    upScaleDynamic,
    upVMax,
    upVMaxDynamic,
    -- * Swarm Operations
    defaultSwarm,
    randomSwarm, 
    createSwarm, 
    updateSwarm, 
    iterateSwarm,
    -- * Data Structures
    PsoGuide(..), 
    Particle(..), 
    Swarm(..), 
    -- * Analysis
    center,
--    avgScore,
    posVariance,
--    scoreVariance
    ) where
import Data.Function (on)
import Data.List (foldl', genericIndex)
import Data.Monoid
import System.Random

{- $typeclasses
In general, to use this library, you will be optimizing a function @f ::
a -> b@. This library will work so long as

1. @a@ is a member of the @'PsoVect'@ type class, and
2. @b@ is a member of the @'Grade'@ type class.

Ex. You want to minimize the function @f (x,y) = x^2 + y^2@. Then @f@
has type @f :: (Double, Double) -> Double@, where lower scores are
better. Then you need to make sure that @(Double, Double)@ is an
instance of @PsoVect@ and that @Double@ is an instance of @Grade@ (with
@'betterThan'@ given by @(<)@). Good news - these are already included,
so you don't need to create these instances yourself.

For more examples, see the examples included with the code (which should
be available at <https://github.com/brianshourd/haskell-PSO>).
-}

{- | 
Represents the position and velocity of a particle. 

Minimal complete definition: @'pAdd'@, @'pZero'@, @'pScale'@

Theoretically, a type belonging to the class @PsoVect@ should form a
/real vector space/
(<http://en.wikipedia.org/wiki/Vector_space#Definition>). That is, it
should satisfy the vector space laws: 

Suppose that the type @a@ is an instance of @'PsoVect'@. for all @u@,
@v@, and @w@ of type @a@, and all @r@ and @s@ of type @Double@, we
should have:

    1. @pAdd (pAdd u v) w = pAdd u (pAdd v w)@ [pAdd is associative]

    2. @pAdd v pZero = pAdd pZero v = v@ [pZero is additive identity]

    3. For every @v@ there is an additive inverse @v'@ such that @pAdd v
    v' = pAdd v' v = pZero@

    4. @pAdd v w = pAdd w v@ [pAdd is commutative]

    5. @pScale r (pAdd v w) = pAdd (pScale r v) (pScale r w)@
        [Distributivity]

    6. @pScale (r + s) v = pAdd (pScale r v) (pScale s v)@
        [Distributivity]

    7. @pScale (r * s) v = pScale r (pScale s v)@ [Compatibility]

    8. @pScale 1 v = v@ [Identity scalar]

Notice that the first 2 laws are the Monoid laws (mAppend = pAdd, mempty
= pZero) and the first 4 are the laws of an abelian group.

If these laws are satisfied, we can prove that the additive inverse of
@v@ is @pScale (-1) v@, so we can automatically define @'pSubtract'@.
However, you may provide a faster implementation if you wish.

Random is a requirement of this typeclass, as well. Especially randomR,
which should work on a component-by-component basis. See the examples
for more information on how/why Random is required.
-}
class (Random a) => PsoVect a 
  where
    pAdd :: a -> a -> a
    pZero :: a
    pScale :: Double -> a -> a
    pSubtract :: a -> a -> a
    pSubtract v1 v2 = pAdd v1 $ pScale (-1) v2

{- |
The function that you wish to optimize should be of the form @f ::
(PsoVect a, Grade b) => a -> b@. That is, it should associate to each
point a grade.

Many different data structures could be considered grades. In
particular, the types of @Double@ and @Maybe Double@, both of which are
instanced below. In the case of @Double@, lower scores are better
(including negative numbers) - this is the usual scoring system for PSO.

In the @Maybe Double@ case, lower scores are better, but a score of
@Nothing@ is worst of all. This is principally used to create grading 
functions which look only within given bounds. For example, if we wish 
to find the lowest value of @f x y = 2 * x + 3 * y - 4@ within the
bounds @0 <= x <= 3@ and @0 <= y <= 4@, we would use as our grading
function

    g :: (Double, Double) -> Maybe Double
    g p\@(x,y) 
        | and [0 <= x, x <= 2, 0 <= y, y <= 3] = Just $ 2 * x + 3 * y - 4
        | otherwise                            = Nothing

In practice, I don't think that anyone will need to instantiate this
typeclass, since I've already made instances for what I think are the
common situations. However, if you needed to, a minimal definition would
include either @betterThan@ or @worseThan@.
-}

class Grade a 
  where
    betterThan :: a -> a -> Bool
    x `betterThan` y = not $ x `worseThan` y
    worseThan :: a -> a -> Bool
    x `worseThan` y = not $ x `betterThan` y

-- Convenience function to determine the best grade from a list of
-- grades
bestGrade :: (Grade a) => [a] -> a
bestGrade (x:xs) = foldr go x xs
  where
    go y z
        | y `betterThan` z = y
        | otherwise        = z

{- |
If @PsoVect@ means /real vector space/, then @PsoSized@ means /real
inner product space/
(<http://en.wikipedia.org/wiki/Inner_product_space>).  In particular, we
should satisfy the following three axioms:

1. @x `dot` y == y `dot` x@ for all @x@ and @y@ (symmetric).
2. @(x `pAdd` (a `pScale` z)) `dot` y == (x `dot` y) + a * (z `dot y)@
    for all @x@, @y@, @z@, and @a@ (linear in the first argument).
3. @x `dot` x >= 0@ for all @x@, with equality if and only if @x ==
    pZero@ (positive-definite).

In practice, a @PsoVect@ is just an ordered collection of real numbers,
and we can define `dot` analagously to:

    dot :: (Double, Double) -> (Double, Double) -> Double
    (x1, y1) `dot` (x2, y2) = x1 * x2 + y1 * y2

In many cases, it is not necessary to make your data an instance of
@PsoSized@. Generally, this is only used if you wish to use @'upVMax'@.
-}
class (PsoVect a) => PsoSized a 
  where
    dot :: a -> a -> Double

{-
If we have a @PsoSized@ element, then it has a length. This is the
square of that length.
-}
pSqMag :: (PsoSized a) => a -> Double
pSqMag x = x `dot` x

{- |
A guide for a particle. The term /guide/ originates from (as far as I
can tell) this paper on velocity adaptation.

Helwig, Sabine, Frank Neumann, and Rolf Wanka. \"Particle swarm
optimization with velocity adaptation.\" Adaptive and Intelligent
Systems, 2009. ICAIS'09. International Conference on. IEEE, 2009.

It stores both the location of the possible minimum, and the value of
the function to minimize at that point.

In use, the type @a@ should belong to the @'PsoVect'@ class and the type
@b@ should belong to the @'Grade'@ class.
-}
data PsoGuide a b = PsoGuide {
    pt :: a, 
    val :: b
    } deriving (Show)

bestGuide :: (Grade b) => [PsoGuide a b] -> PsoGuide a b
bestGuide (x:xs) = foldr go x xs
  where
    go y@(PsoGuide _ valy) z@(PsoGuide _ valz)
        | valy `betterThan` valz = y
        | otherwise              = z

{- | 
@Particles@ know their location, velocity, and the best location/value
they've visited. Individual @Particles@ do not know the global minimum,
but the @Swarm@ they belong to does

In use, the type @a@ should belong to the @'PsoVect'@ class and the type
@b@ should belong to the @'Grade'@ class.
-}
data Particle a b = Particle {
    pos    :: a,            -- ^ Position of particle
    vel    :: a,            -- ^ Velocity of particle
    pGuide :: PsoGuide a b  -- ^ Private guide
    } deriving (Show)

{- |
Updater is the function which takes as arguments:

1. A @StdGen@, to do any necessary random calculations
2. A @Particle a b@, from which it can get position, velocity, and th
    private guide
3. A @PsoGuide a b@, the local guide for the particle
4. An @Int@, the current iteration of the swarm. Useful for parameters
    that adjust over time

It returns the new velocity of the particle.

Normally, updaters are not created through this constructor, but through
one of the @up*@ functions (e.g. @'upStandard'@,
@'upOriginal'@, etc.), or by combining some of the builder functions
through @<>@.
-}
data Updater a b = Updater {
    newVel :: StdGen -> Particle a b -> PsoGuide a b -> Integer -> (a, StdGen)
    }

{- |
Updaters form a @Monoid@ under (essentially) composition. An @Updater@
is just a wrapper on a function @f :: StdGen -> Particle a b -> PsoGuide
a b -> Integer -> (a, StdGen)@. A @Particle a b@ is really just a
position (type @a@), a velocity (type @a@), and a
private guide (type @PsoGuide a b@). If we regard the position, private
guide, local guide, and iteration to be constant, then an @Updater@ just
is just a function @StdGen -> a -> (a, StdGen)@ which takes a velocity
and a standard generator to produce a new velocity and a new standard
generator.  In this way, we can think about composing @Updaters@ - this
composition forms a @Monoid@.
-}
instance (PsoVect a, Grade b) => Monoid (Updater a b)
  where
    mempty = Updater (\g (Particle _ v _) _ _ -> (v, g))
    mappend (Updater f) (Updater g) = Updater h
      where
        h gen part@(Particle p v pg) lg i = f gen' (Particle p v' pg) lg i
          where
            (v', gen') = g gen part lg i

{- $buildingUpdaters
The following Updaters are simple building blocks for building more
complex Updaters. Ex. If you have an Updater @u@ and you wish to change
it so that it has a maximum velocity, you can use @upMaxVel <> u@.
-}

{- |
Add to the velocity a random vector between the particle's current
position and the location of the local guide, scaled by the given
@Double@.
-}
upAddLocal :: (PsoVect a, Grade b) => Double -> Updater a b
upAddLocal c = upAddLocalDynamic $ const c

{- |
Add to the velocity a random vector between the particle's current
position and the location of the local guide, scaled by the given
@Integer -> Double@ (fed by the current iteration).
-}
upAddLocalDynamic :: (PsoVect a, Grade b) => (Integer -> Double) -> Updater a b
upAddLocalDynamic c = Updater f
  where
    f gen (Particle p v _) (PsoGuide lg _) i = (v', gen')
      where
        v' = pAdd v $ pScale (c i) dl
        (dl, gen') = randomR (pZero, pSubtract lg p) gen

{- |
Add to the velocity a random vector between the particle's current
position and the location of the private guide, scaled by the given
@Double@.
-}
upAddPrivate :: (PsoVect a, Grade b) => Double -> Updater a b
upAddPrivate c = upAddPrivateDynamic $ const c

{- |
Add to the velocity a random vector between the particle's current
position and the location of the private guide, scaled by the given
@Integer -> Double@ (fed by the current iteration).
-}
upAddPrivateDynamic :: (PsoVect a, Grade b) => (Integer -> Double) -> Updater a b
upAddPrivateDynamic c = Updater f
  where
    f gen (Particle p v (PsoGuide pg _)) _ i = (v', gen')
      where
        v' = pAdd v $ pScale (c i) dp
        (dp, gen') = randomR (pZero,  pSubtract pg p) gen

{- |
Scale the velocity by the given @Double@.
-}
upScale :: (PsoVect a, Grade b) => Double -> Updater a b
upScale c = upScaleDynamic $ const c

{- |
Scale the velocity by the given @Integer -> Double@ (fed by the current
iteration).
-}
upScaleDynamic :: (PsoVect a, Grade b) => (Integer -> Double) -> Updater a b
upScaleDynamic c = Updater f
  where
    f gen (Particle _ v _) _ i = (pScale (c i) v, gen)

{- |
Cap the velocity at the magnitude of the given @a@.
-}
upVMax :: (PsoSized a, Grade b) => a -> Updater a b
upVMax max = upVMaxDynamic $ const max

{- |
Cap the velocity at the magnitude of the given @Integer -> a@ (fed by
the current iteration).
-}
upVMaxDynamic :: (PsoSized a, Grade b) => (Integer -> a) -> Updater a b
upVMaxDynamic max = Updater f
  where
    f gen (Particle _ v _) _ i = case (compare `on` pSqMag) v (max i) of
        GT -> (max i, gen)
        _  -> (v, gen)

{- $updaters
These updaters are some of the updaters that I could find in papers on
PSO. In particular, the @'upStandard'@ updater is recent and performs
well in a myriad of situations.
-}

{- |
Create an @'Updater'@ using the so-called /standard PSO/ parameters,
given in

Bratton, Daniel, and James Kennedy. \"Defining a standard for particle
swarm optimization.\" Swarm Intelligence Symposium, 2007. SIS 2007. IEEE.
IEEE, 2007.

If in doubt, the paper suggests that the constriction parameter be given
by the formula chi = 2 / abs(2 - phi - sqrt(phi^2 - 4 * phi)) where phi
= c1 + c2 and phi > 4.
-}
upStandard :: (PsoVect a, Grade b)
    => Double  -- ^ Constriction parameter (chi)
    -> Double  -- ^ Tendancy toward private guide (c1)
    -> Double  -- ^ Tendancy toward local guide (c2)
    -> Updater a b
upStandard chi c1 c2 = (upScale chi) <> (upAddLocal c2) <> (upAddPrivate c1)

{- |
The updater with parameters suggested as a starting point in

Bratton, Daniel, and James Kennedy. \"Defining a standard for particle
swarm optimization.\" Swarm Intelligence Symposium, 2007. SIS 2007. IEEE.
IEEE, 2007.

Normally, one should search for better parameters, since parameter
choice dramatically influences algorithm performance.
-}
upDefault :: (PsoVect a, Grade b) => Updater a b
upDefault = upStandard 0.72984 2.05 2.05

{- |
The original updater function, defined in

Kennedy, James, and Russell Eberhart. \"Particle swarm optimization.\"
Neural Networks, 1995. Proceedings., IEEE International Conference on.
Vol. 4. IEEE, 1995.

This is (in the words of James Kennedy) obsolete now. However, it is still convenient for e.g. testing new versions of updaters. Plus, it's historical.
-}
upOriginal ::(PsoVect a, Grade b)
    => Double  -- ^ Tendancy toward private guide (c1)
    -> Double  -- ^ Tendancy toward local guide (c2)
    -> Updater a b
upOriginal c1 c2 = (upAddLocal c2) <> (upAddPrivate c1)

{- |
An updater using a constant inertia weight factor, as can be found in

Shi, Yuhui, and Russell Eberhart. \"Parameter selection in particle swarm
optimization.\" Evolutionary Programming VII. Springer Berlin/Heidelberg,
1998.
-}

upInertiaWeight :: (PsoVect a, Grade b)
    => Double  -- ^ Inertia weight (omega)
    -> Double  -- ^ Tendancy toward private guide (c1)
    -> Double  -- ^ Tendancy toward local guide (c2)
    -> Updater a b
upInertiaWeight omega c1 c2 = (upAddLocal c2) <> (upAddPrivate c1) <> (upScale omega)

{- |
An updater using a dynamic inertia weight factor, as can be found in

Shi, Yuhui, and Russell Eberhart. \"Parameter selection in particle swarm
optimization.\" Evolutionary Programming VII. Springer Berlin/Heidelberg,
1998.
-}

upInertiaWeightDynamic :: (PsoVect a, Grade b)
    => (Integer -> Double)  -- ^ Inertia weight (omega)
    -> Double               -- ^ Tendancy toward private guide (c1)
    -> Double               -- ^ Tendancy toward local guide (c2)
    -> Updater a b
upInertiaWeightDynamic omega c1 c2 = (upAddLocal c2) <> (upAddPrivate c1) <> (upScaleDynamic omega)

{- | 
A @Swarm@ keeps track of all the particles in the swarm, the function
that the swarm seeks to minimize, the updater, the current iteration
(for dynamic updaters), and the best location/value found so far

In use, the type @a@ should belong to the @'PsoVect'@ class and the type
@b@ should belong to the @'Grade'@ class.
-}
data Swarm a b = Swarm {
    parts     :: [Particle a b],  -- ^ Particles in the swarm
    gGuide    :: PsoGuide a b,    -- ^ Global guide
    func      :: a -> b,          -- ^ Function to minimize
    updater   :: Updater a b,     -- ^ Updater
    iteration :: Integer          -- ^ Current iteration
    }

instance (Show a, Show b) => Show (Swarm a b) 
  where
    show (Swarm ps b _ _ _) = show ( map pGuide ps) ++ show b

defaultSwarm :: (PsoVect a, Random a, Grade b)
    => (a -> b)
    -> (a, a)
    -> StdGen
    -> (Swarm a b, StdGen)
defaultSwarm f bounds gen = randomSwarm gen 50 bounds f upDefault

{- | 
Create a swarm by randomly generating n points within the bounds, making
all the particles start at these points with velocity zero.
-}
randomSwarm :: (PsoVect a, Random a, Grade b) 
    => StdGen   -- ^ A random seed
    -> Int      -- ^ Number of particles
    -> (a,a)    -- ^ Bounds to create particles in
    -> (a -> b)             -- ^ Function to minimize
    -> Updater a b          -- ^ Updater
    -> (Swarm a b, StdGen)  -- ^ (Swarm returned, new seed)
randomSwarm g n bounds f up = (createSwarm ps f up, g') 
  where
    (ps, g') = getSomeRands n g []
    getSomeRands 0 gen acc = (acc,gen)
    getSomeRands m gen acc = getSomeRands (m-1) gen' (next:acc) 
      where
        (next, gen') = randomR bounds gen

{- | 
Create a swarm in initial state based on the positions of the particles.
Initial velocities are all zero.
-}
createSwarm :: (PsoVect a, Grade b)
    => [a]          -- ^ Positions of of particles
    -> (a -> b)     -- ^ Function to minimize
    -> Updater a b  -- ^ Updater to use
    -> Swarm a b
createSwarm ps f up = Swarm qs b f up 0 
  where
    qs = map (createParticle f) ps
    b = bestGuide $ map (pGuide) qs
    createParticle f' p = Particle p pZero (PsoGuide p (f' p))

{- | 
Update the swarm one step, updating every particle's position and
velocity, and the best values found so far. Returns the updated swarm as
well as a new generator to use.

Arguments ordered to allow @iterate (uncurry updateSwarm)@
-}
updateSwarm :: (PsoVect a, Grade b) => Swarm a b -> StdGen -> (Swarm a b, StdGen)
updateSwarm s@(Swarm ps b f up i) g = (Swarm qs b' f up (i+1), g') 
  where
    (qs, g', b') = foldl' helper ([], g, b) ps
    helper (acc, gen, best) p = (p':acc, gen', minBest) 
      where
        (p',gen') = updateParticle p s gen
        minBest = case (val best) `betterThan` (val $ pGuide p') of
            True -> best
            _    -> pGuide p'
{- |
Update a swarm repeatedly. Absorbs a @StdGen@.
-}

iterateSwarm :: (PsoVect a, Grade b) => Swarm a b -> StdGen -> [Swarm a b]
iterateSwarm s g = map fst $ iterate (uncurry updateSwarm) (s,g)

{- |
If you don't want to think about all of this stuff, don't worry. Just use this function to get a nice, easy optimization for a give function - none of this nonsense about creating swarms or what-not.

It returns a @PsoGuide@, which contains both the optimal value and the point at which that optimal value is achieved.
-}
easyOptimize :: (PsoVect a, Random a, Grade b) 
    => (a -> b)  -- ^ Function to optimize
    -> (a, a)    -- ^ Bounds to create particles within
    -> Integer   -- ^ Number of iterations
    -> StdGen    -- ^ Generator to use
    -> PsoGuide a b 
easyOptimize f bounds n gen = gGuide . (`genericIndex` n) . iterateSwarm swarm $ gen'
  where
    (swarm, gen') = randomSwarm gen 50 bounds f upDefault

{- | 
Update a particle one step. Called by updateSwarm and requires the swarm
that the particle belongs to as a parameter
-}
updateParticle :: (PsoVect a, Grade b) => Particle a b -> Swarm a b -> StdGen -> (Particle a b, StdGen)
updateParticle part@(Particle p v pg) (Swarm ps lg f up i) g = (Particle p' v' pg', g') 
  where
    p' = pAdd p v'
    {-
    dp = pSubtract (pt bp) p
        -- ^ Difference between point and private guide best
    dg = pSubtract (pt b) p
        -- ^ Difference between point and local guide best
    (r1, g') = randomR (pZero,dp) g
    (r2, g'') = randomR (pZero,dg) g'
    -}
    (v',g') = (newVel up) g part lg i
    {-
    newVel (Updater f) = pScale chi $ 
        pAdd v $ 
        pAdd (pScale c1 dp) $
        pScale c2 dg
    -}
    pg' = case (val pg) `betterThan` (f p') of
        False  -> PsoGuide p' (f p')
        _      -> pg

-- ===============
-- Analysis
-- ===============

{- |
Find the center of the swarm.
-}
center :: (PsoVect a) => Swarm a b -> a
center (Swarm ps _ _ _ _) = avg sumPoints
  where
    avg = pScale (1 / (fromIntegral $ length ps))
    sumPoints = foldr pAdd pZero $ map pos ps

{-
Each point has a personal best, what is the average?
-}
{-
avgScore :: (PsoVect a) => Swarm a -> Double
avgScore (Swarm ps _ f _ _) = avg $ map (val . pGuide) ps
  where
    avg xs = (sum xs) / (fromIntegral $ length xs)
-}

{- |
Total variance of the distance points are from the center of the swarm.
Measures how close the swarm is to converging, and can be used to
determine if a swarm has converged on a point or not.
-}
posVariance :: (PsoSized a) => Swarm a b -> Double
posVariance s@(Swarm ps _ _ _ _) = sum . map (pSqMag . pSubtract cen . pos) $ ps
  where
    cen = center s

{-
Total variance of the scores of the private guides. Measures how close
the swarm is to converging upon a score, even if it cannot decide on a
single best location for that score. Good if, say, your problem is
multi-modal.
-}
{-
scoreVariance :: (PsoVect a) => Swarm a -> Double
scoreVariance s@(Swarm ps b f _ _) = sum . map ((^2) . (-) avg . val . pGuide) $ ps
  where
    avg = avgScore s
-}


