{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

{- | 
A small module to perform optimization using Particle Swarm Optimization
(PSO), based on the paper by James Kennedy and Russell Eberhart:
  
Kennedy, James, and Russell Eberhart. \"Particle swarm
optimization.\" /Neural Networks, 1995. Proceedings., IEEE/
/International Conference on./ Vol. 4. IEEE, 1995.
  
I highly recommend that you find and read this paper (it's only 7
pages) if you are interested in PSO at all. However, the wikipedia
entry is also quite good:
<http://en.wikipedia.org/wiki/Particle_swarm_optimization>
-}

module Pso 
    (
    PsoVect(..), 
    PsoGuide(..), 
    Particle(..), 
    Swarm(..), 
    PsoParams(..), 
    randomSwarm, 
    createSwarm, 
    updateSwarm, 
    iterateSwarm
    ) where
import Data.Function (on)
import Data.List (foldl', minimumBy)
import System.Random

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
class (Random a) => PsoVect a where
    pAdd :: a -> a -> a
    pZero :: a
    pScale :: Double -> a -> a
    pSubtract :: a -> a -> a
    pSubtract v1 v2 = pAdd v1 $ pScale (-1) v2

{- |
A guide for a particle. The term "guide" originates from (as far as I
can tell) this paper on velocity adaptation.

Helwig, Sabine, Frank Neumann, and Rolf Wanka. "Particle swarm
optimization with velocity adaptation." Adaptive and Intelligent
Systems, 2009. ICAIS'09. International Conference on. IEEE, 2009.

It stores both the location of the possible minimum, and the value of
the function to minimize at that point.

In use, the type @a@ should belong to the @'PsoVect'@ class.
-}
data PsoGuide a = PsoGuide {
    pt :: a, 
    val :: Double
    } deriving (Show)

{- | 
@Particles@ know their location, velocity, and the best location/value
they've visited. Individual @Particles@ do not know the global minimum,
but the @Swarm@ they belong to does

In use, the type @a@ should belong to the @'PsoVect'@ class.
-}
data Particle a = Particle {
    pos :: a,       -- ^ position of particle
    vel :: a,       -- ^ velocity of particle
    pGuide :: PsoGuide a  -- ^ private guide
    } deriving (Show)

{- | 
Holds the parameters used to update particle velocities, see 

Shi, Yuhui, and Russell Eberhart. \"Parameter selection in particle swarm
optimization.\" /Evolutionary Programming VII./ Springer
Berlin/Heidelberg, 1998.
-}
data PsoParams = PsoParamsStatic
    Double  -- inertia weight
    Double  -- tendancy toward local
    Double  -- tendancy toward global
    | PsoParamsDynamic 
    (Integer -> Double)  -- inertia weight
    (Integer -> Double)  -- tendancy toward local
    (Integer -> Double)  -- tendancy toward global

{- | 
The original parameters given in the 1995 paper \"Particle Swarm
Optimization\" by James Kennedy and Russell Eberhart

Normally, one should search for better parameters, since parameter
choice dramatically influences algorithm performance.
-}
defaultPsoParams :: PsoParams
defaultPsoParams = PsoParamsStatic 1 2 2

{- | 
A @Swarm@ keeps track of all the particles in the swarm, the function
that the swarm seeks to minimize, the parameters, the current iteration
(for parameters), and the best location/value found so far

In use, the type @a@ should belong to the @'PsoVect'@ class.
-}
data Swarm a = Swarm {
    parts :: [Particle a],  -- ^ particles in the swarm
    gGuide :: PsoGuide a,   -- ^ global guide
    func :: a -> Double,    -- ^ funtion to minimize
    params :: PsoParams,    -- ^ parameters
    iteration :: Integer    -- ^ current iteration
    }

instance Show a => Show (Swarm a) where
    show (Swarm ps b _ _ _) = show ( map pGuide ps) ++ show b

{- | 
Create a swarm by randomly generating n points within the bounds, making
all the particles start at these points with velocity zero.
-}
randomSwarm :: (PsoVect a, Random a, RandomGen b) 
    => b        -- ^ A random seed
    -> Int      -- ^ Number of particles
    -> (a,a)    -- ^ Bounds to create particles in
    -> (a -> Double)    -- ^ Function to minimize
    -> PsoParams    -- ^ Parameters
    -> (Swarm a, b) -- ^ (Swarm returned, new seed)
randomSwarm g n bounds f params = (createSwarm ps f params, g') where
    (ps, g') = getSomeRands n g []
    getSomeRands 0 gen acc = (acc,gen)
    getSomeRands m gen acc = getSomeRands (m-1) gen' (next:acc) where
        (next, gen') = randomR bounds gen

{- | 
Create a swarm in initial state based on the positions of the particles.
Initial velocities are all zero.
-}
createSwarm :: (PsoVect a) 
    => [a]          -- ^ Positions of of particles
    -> (a -> Double)    -- ^ Function to minimize
    -> PsoParams    -- ^ Parameters to use
    -> Swarm a
createSwarm ps f pars = Swarm qs b f pars 0 where
    qs = map (createParticle f) ps
    b = minimumBy (compare `on` val) $ map pGuide qs
    createParticle f' p = Particle p pZero (PsoGuide p (f' p))

{- | 
Update the swarm one step, updating every particle's position and
velocity, and the best values found so far. Returns the updated swarm as
well as a new generator to use.

Arguments ordered to allow @iterate (uncurry updateSwarm)@
-}
updateSwarm :: (PsoVect a, RandomGen b) => Swarm a -> b -> (Swarm a, b)
updateSwarm s@(Swarm ps b f pars i) g = (Swarm qs b' f pars (i+1), g') where
    (qs, g', b') = foldl' helper ([], g, b) ps
    helper (acc, gen, best) p = (p':acc, gen', minBest) where
        (p',gen') = updateParticle p s gen
        minBest = case compare (val best) (val $ pGuide p') of
            LT -> best
            _  -> pGuide p'
{- |
Update a swarm repeatedly. Absorbs a @RandomGen@.
-}

iterateSwarm :: (PsoVect a, RandomGen b) => Swarm a -> b -> [Swarm a]
iterateSwarm s g = map fst $ iterate (uncurry updateSwarm) (s,g)

{- | 
Update a particle one step. Called by updateSwarm and requires the swarm
that the particle belongs to as a parameter
-}
updateParticle :: (PsoVect a, RandomGen b) => Particle a -> Swarm a -> b -> (Particle a, b)
updateParticle (Particle p v bp) (Swarm ps b f pars i) g = (Particle p' v' bp', g'') where
    p' = pAdd p v'
    dp = pSubtract (pt bp) p
        -- ^ Difference between point and local best
    dg = pSubtract (pt b) p
        -- ^ Difference between point and global best
    (r1, g') = randomR (pZero,dp) g
    (r2, g'') = randomR (pZero,dg) g'
    v' = newVel pars
    newVel (PsoParamsStatic omega c1 c2) = pAdd (pScale omega v) $ 
        pAdd (pScale c1 dp) $
        pScale c2 dg
    newVel (PsoParamsDynamic omega c1 c2) = pAdd (pScale (omega i) v) $ 
         pAdd (pScale (c1 i) dp) $
         pScale (c2 i) dg
    bp' = case compare (val bp) (f p') of
        LT -> PsoGuide p' (f p')
        _  -> bp

-- ===============
-- Instances
-- ===============

{-
Declarations for fractionals e.g Double, Rational
-}

instance PsoVect Double where
    pAdd = (+)
    pScale = (*)
    pZero = 0
    pSubtract = (-)

{-
instance PsoVect Rational where
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
instance (PsoVect a, PsoVect b, Random a, Random b) => Random (a, b) where
    random g = ((x, y), g'') where
        (x, g')  = random g
        (y, g'') = random g'
    randomR ((a1, b1), (a2, b2)) g = ((x, y), g'') where
        (x, g')  = randomR (a1, a2) g
        (y, g'') = randomR (b1, b2) g'

instance (PsoVect a, PsoVect b) => PsoVect (a, b) where
    pAdd (x1, y1) (x2, y2) = (pAdd x1 x2, pAdd y1 y2)
    pScale r (x,y) = (pScale r x, pScale r y)
    pZero = (pZero, pZero)

{- 
Declaration for triples of PsoVects

Mathematically, this is the direct sum of vector spaces.
-}
instance (PsoVect a, PsoVect b, PsoVect c, Random a, Random b, Random c) => Random (a, b, c) where
    random g = ((x, y, z), g''') where
        (x, g')   = random g
        (y, g'')  = random g'
        (z, g''') = random g''
    randomR ((a1, b1, c1), (a2, b2, c2)) g = ((x, y, z), g''') where
        (x, g')   = randomR (a1, a2) g
        (y, g'')  = randomR (b1, b2) g'
        (z, g''') = randomR (c1, c2) g''

instance (PsoVect a, PsoVect b, PsoVect c) => PsoVect (a, b, c) where
    pAdd (x1, y1, z1) (x2, y2, z2) = (pAdd x1 x2, pAdd y1 y2, pAdd z1 z2)
    pScale r (x,y,z) = (pScale r x, pScale r y, pScale r z)
    pZero = (pZero,pZero,pZero)

