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

module PSO (PSOVect(..), PSOCand(..), Particle(..), Swarm(..), PSOParams(..), randomSwarm, createSwarm, updateSwarm) where
import System.Random
import Data.List (foldl')
import Data.Function (on)

{- | 
Represents the position and velocity of a particle. 

Minimal complete definition: @'pAdd'@, @'pZero'@, @'pScale'@

Theoretically, a type belonging to the class @PSOVect@ should form a
/real vector space/
(<http://en.wikipedia.org/wiki/Vector_space#Definition>). That is, it
should satisfy the vector space laws: 

Suppose that the type @a@ is an instance of @'PSOVect'@. for all @u@,
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
-}
class (Eq a) => PSOVect a where
    pAdd :: a -> a -> a
    pZero :: a
    pScale :: Double -> a -> a
    pSubtract :: a -> a -> a
    pSubtract v1 v2 = pAdd v1 $ pScale (-1) v2

{- |
A candidate for a global minimum. Stores both the location of the
possible minimum, and the value of the function to minimize at that
point.

In use, the type @a@ should belong to the @'PSOVect'@ class.
-}
data PSOCand a = PSOCand {
    pt :: a, 
    val :: Double
    } deriving (Show, Eq)

{- | 
Better/worse @PSOCands@ are determined by their values. Need @Eq a@ in
order to assure that @PSOCand@ a is an instance of @Eq@, of which @Ord@
is a superclass
-}
instance (Eq a) => Ord (PSOCand a) where
    compare = compare `on` val

{- | 
@Particles@ know their location, velocity, and the best location/value
they've visited. Individual @Particles@ do not know the global minimum,
but the @Swarm@ they belong to does

In use, the type @a@ should belong to the @'PSOVect'@ class.
-}
data Particle a = Particle {
    pos :: a,       -- ^ position of particle
    vel :: a,       -- ^ velocity of particle
    pBest :: PSOCand a  -- ^ best position this particle has found
    } deriving (Eq, Show)

{- | 
Compare points based on their best value.  Need @Eq a@ in order to
assure that @Particle a@ is an instance of @Eq@, of which @Ord@ is a
superclass
-}
instance (Eq a) => Ord (Particle a) where
    compare = compare `on` pBest

{- | 
Holds the parameters used to update particle velocities, see 

Shi, Yuhui, and Russell Eberhart. \"Parameter selection in particle swarm
optimization.\" /Evolutionary Programming VII./ Springer
Berlin/Heidelberg, 1998.
-}
data PSOParams = PSOParamsStatic
    Double  -- inertia weight
    Double  -- tendancy toward local
    Double  -- tendancy toward global
    | PSOParamsDynamic 
    (Integer -> Double)  -- inertia weight
    (Integer -> Double)  -- tendancy toward local
    (Integer -> Double)  -- tendancy toward global

{- | 
The original parameters given in the 1995 paper \"Particle Swarm
Optimization\" by James Kennedy and Russell Eberhart

Normally, one should search for better parameters, since parameter
choice dramatically influences algorithm performance.
-}
defaultPSOParams :: PSOParams
defaultPSOParams = PSOParamsStatic 1 2 2

{- | 
A @Swarm@ keeps track of all the particles in the swarm, the function
that the swarm seeks to minimize, the parameters, the current iteration
(for parameters), and the best location/value found so far

In use, the type @a@ should belong to the @'PSOVect'@ class.
-}
data Swarm a = Swarm {
    parts :: [Particle a],  -- ^ particles in the swarm
    gBest :: PSOCand a,     -- ^ best position found
    func :: a -> Double,    -- ^ funtion to minimize
    params :: PSOParams,    -- ^ parameters
    iteration :: Integer    -- ^ current iteration
    }

instance Show a => Show (Swarm a) where
    show (Swarm ps b _ _ _) = show ( map pBest ps) ++ show b

{- | 
Create a swarm by randomly generating n points within the bounds, making
all the particles start at these points with velocity zero.
-}
randomSwarm :: (PSOVect a, Random a, RandomGen b) 
    => b        -- ^ A random seed
    -> Int      -- ^ Number of particles
    -> (a,a)    -- ^ Bounds to create particles in
    -> (a -> Double)    -- ^ Function to minimize
    -> PSOParams    -- ^ Parameters
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
createSwarm :: (PSOVect a) 
    => [a]          -- ^ Positions of of particles
    -> (a -> Double)    -- ^ Function to minimize
    -> PSOParams    -- ^ Parameters to use
    -> Swarm a
createSwarm ps f pars = Swarm qs b f pars 0 where
    qs = map (createParticle f) ps
    b = pBest . minimum $ qs
    createParticle f' p = Particle p pZero (PSOCand p (f' p))

{- | 
Update the swarm one step, updating every particle's position and
velocity, and the best values found so far. Returns the updated swarm as
well as a new generator to use.

Arguments ordered to allow @iterate (uncurry updateSwarm)@
-}
updateSwarm :: (PSOVect a, RandomGen b) => Swarm a -> b -> (Swarm a, b)
updateSwarm s@(Swarm ps b f pars i) g = (Swarm qs b' f pars (i+1), g') where
    (qs, g', b') = foldl' helper ([], g, b) ps
    helper (acc, gen, best) p = (p':acc, gen', min best (pBest p')) where
        (p',gen') = updateParticle p s gen

{- |
Update a swarm repeatedly. Absorbs a @RandomGen@.
-}

iterateSwarm :: (PSOVect a, RandomGen b) => Swarm a -> b -> [Swarm a]
iterateSwarm s g = map fst $ iterate (uncurry updateSwarm) (s,g)

{- | 
Update a particle one step. Called by updateSwarm and requires the swarm
that the particle belongs to as a parameter
-}
updateParticle :: (PSOVect a, RandomGen b) => Particle a -> Swarm a -> b -> (Particle a, b)
updateParticle (Particle p v bp) (Swarm ps b f pars i) g = (Particle p' v' bp', g'') where
    p' = pAdd p v'
    (r1, g') = randomR (0,1) g
    (r2, g'') = randomR (0,1) g'
    dp = pSubtract (pt bp) p
    dg = pSubtract (pt b) p
    v' = newVel pars
    newVel (PSOParamsStatic omega c1 c2) = pAdd (pScale omega v) $ 
         pAdd (pScale (c1 * r1) dp) $
         pScale (c2 * r2) dg
    newVel (PSOParamsDynamic omega c1 c2) = pAdd (pScale (omega i) v) $ 
         pAdd (pScale (c1 i * r1) dp) $
         pScale (c2 i * r2) dg
    bp' = min bp $ PSOCand p' (f p')

-- ===============
-- Instances
-- ===============

{-
Declarations for fractionals e.g Double, Rational
-}

instance PSOVect Double where
    pAdd x y = x + y
    pScale r x = r * x
    pZero = 0
    pSubtract x y = x - y

instance PSOVect Rational where
    pAdd x y = x + y
    pScale r x = toRational r * x
    pZero = 0
    pSubtract x y = x - y

{- 
Declaration for pairs of PSOVects e.g. (Double, Double), (Float,
Float), (Rational, Rational), etc.

Mathematically, this is the direct sum of vector spaces.
-}
instance (PSOVect a, PSOVect b, Random a, Random b) => Random (a, b) where
    random g = ((x, y), g'') where
        (x, g')  = random g
        (y, g'') = random g'
    randomR ((a1, b1), (a2, b2)) g = ((x, y), g'') where
        (x, g')  = randomR (a1, a2) g
        (y, g'') = randomR (b1, b2) g'

instance (PSOVect a, PSOVect b) => PSOVect (a, b) where
    pAdd (x1, y1) (x2, y2) = (pAdd x1 x2, pAdd y1 y2)
    pScale r (x,y) = (pScale r x, pScale r y)
    pZero = (pZero, pZero)

{- 
Declaration for triples of fractionals e.g. (Double, Double), (Float,
Float), (Rational, Rational), etc.

Mathematically, this is the direct sum of vector spaces.
-}
instance (PSOVect a, PSOVect b, PSOVect c, Random a, Random b, Random c) => Random (a, b, c) where
    random g = ((x, y, z), g''') where
        (x, g')   = random g
        (y, g'')  = random g'
        (z, g''') = random g''
    randomR ((a1, b1, c1), (a2, b2, c2)) g = ((x, y, z), g''') where
        (x, g')   = randomR (a1, a2) g
        (y, g'')  = randomR (b1, b2) g'
        (z, g''') = randomR (c1, c2) g''

instance (PSOVect a, PSOVect b, PSOVect c) => PSOVect (a, b, c) where
    pAdd (x1, y1, z1) (x2, y2, z2) = (pAdd x1 x2, pAdd y1 y2, pAdd z1 z2)
    pScale r (x,y,z) = (pScale r x, pScale r y, pScale r z)
    pZero = (pZero,pZero,pZero)
