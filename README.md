haskell-ParticleSwarmOptimization
=================================

This is a small library for doing Particle Swarm Optimization (PSO) in
Haskell. For an overview of what PSO is, see the paper "Particle Swarm
Optimization" by James Kennedy and Russel Eberhart. Alternately,
[wikipedia's
entry](http://en.wikipedia.org/wiki/Particle_swarm_optimization) is
pretty good.

Essentially, we have a function `f :: a -> Double` which we want to
optimize. That is, we want to find some `a` which minimizes `f`. To do
this, we will create a swarm of particles, then let those particles run
free until a good solution is found. To do this, just make your `a`
data (the input data of the function to minimize) an instance of the
`PSOVect` class.

## Make a PSOVect instance

Ex: Suppose we want to minimize the function `f (x,y) = x^2 + y^2`. The
type signature of `f` is `f :: (Double, Double) -> Double`. We'll need
to make `(Double, Double)` an instance of the `PSOVect` typeclass. In
particular, we'll need to define a way to add together two `(Double,
Double)`s, a way to multiply a `(Double, Double)` by a `Double` scalar,
and a zero for `(Double, Double)`. That instance looks like:

    instance PSOVect (Double, Double) where
        pAdd (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
        pScale r (x,y) = (r * x, r * y)
        pZero = (0,0)

That's it. Next, we'll want to create a swarm.

## Create a swarm

We can create a swarm in three ways. The first, and most
common, way is via `randomSwarm`.

    randomSwarm :: (PSOVect a, Random a, RandomGen b) => b -> Int -> (a,a) -> (a -> Double) -> PSOParams -> (Swarm a, b)

Notice that `randomSwarm` requires that our `a` (in this example,
`(Double, Double)`) be an instance of `PSOVect`, but also of `Random`.
So we declare an instance

    instance Random (Double, Double) where
        random g = ((x, y), g'') where
            (x, g')  = random g
            (y, g'') = random g'
        randomR ((a1, b1), (a2, b2)) g = ((x, y), g'') where
            (x, g')  = randomR (a1, a2) g
            (y, g'') = randomR (b1, b2) g'

Now we can call `randomSwarm`:

    main = do
        gen <- getStdGen
        let f = (\(x,y) -> x^2 + y^2) :: (Double, Double) -> Double
            (s,g) = randomSwarm
                gen             -- Seed for randomness
                20              -- number of particles
                ((-5,-5),(5,5)) -- range to look
                f               -- function to optimize
                defaultPSOParams    -- default parameters

The second way to create a swarm is through `createSwarm`.

    createSwarm :: (PSOVect a) => [a] -> (a -> Double) -> PSOParams -> Swarm a

For this, you supply the initial points, instead of them being generated
randomly. This is nice if, say, you wanted to begin with a grid of
particles or something. It initializes the swarm so that every particle
has initial velocity zero (technically `pZero`).

The last way is directly, through the constructor.

    data Swarm a = Swarm {
        parts :: [Particle a],  -- particles in the swarm
        gBest :: PSOCand a,     -- best position found
        func :: a -> Double,    -- funtion to minimize
        params :: PSOParams,    -- parameters
        iteration :: Integer    -- current iteration
        }

I won't talk more about this. It offers the finest control over your
swarm, but you'll need to read the code to use it. One of the other two
options should suit your needs.

## Using a swarm

All that remains is to update your swarm and check for answers. The
function `updateSwarm` helps you here.

    updateSwarm :: (PSOVect a, RandomGen b) => Swarm a -> b -> (Swarm a, b)

The particles move semi-randomly, so we need a `RandomGen` to update the
swarm. Fortunately, we get a new one back when we're done. If we want to
just continually update over and over, we can call

    let ss = fst . unzip $ iterate (uncurry updateSwarm) (s,g)

Then `ss` is of type `[Swarm (Double, Double)]`, and each element is the
next generation/iteration of the swarm. Hopefully, this swarm has the
answer buried in it somewhere.

## Getting an answer

Right now, the library has no methods for evaluating a swarm to
determine whether it has found an answer or what that answer may be. I
plan to rectify this. In the mean time, you'll have to do your own
simple analysis. Now that we have all successive swarms available in a
list, we can find data about them just by using `map` and some built in
functions. For example, if we just want to know what the best value of
`f` the swarm has found with each iteration, we can 

    let bs = map (val . gBest) ss

In this case, we might get e.g.

    > take 5 bs
    [2.4934653863021836,0.2093513968956252,5.310530725404583e-3,5.310530725404583e-3,5.310530725404583e-3]

Our swarm rapidly converged on 0.00531 as the lowest value. It occurs at

    > pt . gBest $ ss !! 2
    (-6.063899754881441e-2,4.0415871902997e-2)

Indeed, this is pretty good. Maybe good enough, maybe not. We can always
adjust parameters to try to make our swarm behave better.

## Parameter adjustment


