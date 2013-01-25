Calypso
=======

## A library for PSO

This is a small library for doing Particle Swarm Optimization (PSO) in
Haskell. For an overview of what PSO is, see the paper "Particle Swarm
Optimization" by James Kennedy and Russel Eberhart. Alternately,
[wikipedia's
entry](http://en.wikipedia.org/wiki/Particle_swarm_optimization) is
pretty good.

If you want to know the specific flavor of algorithm that this library
is based on, consult

Bratton, Daniel, and James Kennedy. "Defining a standard for particle
swarm optimization." Swarm Intelligence Symposium, 2007. SIS 2007. IEEE.
IEEE, 2007.

Note that while updating based on the so-called "local swarm" is
desired, it is not yet implemented. This library uses the slightly-less
effective "global swarm". Otherwise, this library conforms perfectly to
the standard PSO given in the paper above (although other PSO models are
supported as well).

## Example

Let's start with an example, and then enumerate all of the ways in which
your needs might differ from the example. We want to find the minimum
value of the function

    f :: (Double, Double) -> Double
    f (x,y) = x^2 + y^2

First, we'll load the module

    import Pso

If we have no strong feelings, we can just use `easyOptimize`

    easyOptimize :: (PsoVect a, Random a, Grade b) 
        => (a -> b)  -- Function to optimize
        -> (a, a)    -- Bounds to create particles within
        -> Integer   -- Number of iterations
        -> StdGen    -- Generator to use
        -> PsoGuide a b

So if we wish to look at points `(x, y)` with `-5 <= x <= 5` and `-5 <=
y <= 5`, we can call

    main = do
        gen <- newStdGen
        let guide = easyOptimize f ((-5, -5), (5, 5)) 1000 gen
        putStrLn $ "The minimum appears to be: " ++ (show $ val guide)
        putStrLn $ "It occurs at: " ++ (show $ pt guide)

We can see that the object returned (`PsoGuide (Double, Double) Double`)
contains both the minimum value of `f` and the point at which it occurs.

The function `easyOptimize` is fun, but doesn't give us a lot of
control. In general, we might want to see _all_ attempts at a solution,
not just the 1000th. Then we can better decide what the minimum might
be.

To do this, we'll need to create a `Swarm (Double, Double) Double`. We
can use the function `defaultSwarm`.

    defaultSwarm :: (PsoVect a, Random a, Grade b)
        => (a -> b)  -- ^ Function to optimize
        -> (a, a)    -- ^ Bounds to begin search
        -> StdGen    -- ^ Random generator
        -> (Swarm a b, StdGen)

Once we've created a swarm, we'll want to update it. In fact, it would
be best if we could just update it repeatedly, obtaining an infinite
list of better and better swarms.

    iterateSwarm :: (PsoVect a, Grade b) => Swarm a b -> StdGen -> [Swarm a b]

Because `defaultSwarm` returns a `Swarm` and a `StdGen`, we can call
both of these in order with

    let ss = (uncurry iterateSwarm) $ defaultSwarm f ((-5, -5), (5, 5)) gen

This gives us a list of swarms, each one (hopefully) better than the
last. There are a number of things that we can do with this - we can
look to see when we stop seeing improvements for, say, 20 steps.

    putStrLn . show . head . head . dropWhile ((<=20) . length) . group . map (val . gGuide) $ ss

Or, we could look to see when all of our particles have started to
cluster. There is a built-in function for this.

    posVariance :: (PsoSized a) => Swarm a b -> Double

`posVariance` measures the variance of the distances of the particles
(actually, the squares of the distances).  We could find the value once
this variance drops below 0.000001.

    putStrLn . show . gGuide . head . dropWhile ((> 0.000001) . posVariance) $ ss

Our modified `main` looks like

    main = do
        gen <- newStdGen
        let ss = (uncurry iterateSwarm) $ defaultSwarm f ((-5, -5), (5, 5)) gen
        putStrLn "No improvements for 20 steps:"
        putStrLn . show . head . head . dropWhile ((<=20) . length) . group . map (val . gGuide) $ ss
        putStrLn "Variance of particle positions below 0.000001:"
        putStrLn . show . gGuide . head . dropWhile ((> 0.000001) . posVariance) $ ss

Looks like there is a minimum of 0 near (0,0). At least, at first
glance.

# My function is different!

We don't always want to minimize a function `f :: (Double, Double) ->
Double`, and we don't always want to perform the default search. Here
are some ways that this library can handle optimization of other
functions.

## Different input

Maybe you would rather minimize a function

    f :: (Double, Double, Double) -> Double

Or, more generally, just some function

    f :: a -> Double

Short story: you need `a` to be an instance of `PsoVect`. Built in
instances (in `Calypso.Instance.Grade` and `Calypso.Instance.PsoVect`)
include:

    Double
    (Double, Double)
    (Double, Double, Double)
    (Double, Double, Double, Double)
    (Double, Double, Double, Double, Double)

Replace any of the above `Double` values with `Float` or `Rational`, and
you are still just fine. In, fact, you can replace any of the above
`Double` with any other instance of `PsoVect` and you are still fine.
Ex:

    (Double, (Float, Double), (Float, Float, Rational))
    (Double, (Double, (Double, Double)))

But I only go up to five. Why five? Because if you have more than that,
you probably want a `PsoList`. It's a wrapper for a list that knows how
long it is.

    fromList :: [a] -> PsoList a
    toList :: PsoList a -> [a]

It works so long as `a` is an instance of `PsoVect`. Why do I need to
wrap my lists? Because of reasons (if you are interested, check the code
or email me).

If none of these are what you want, then you have two options:

1. Construct isomorphism functions to one of these data types.
2. Make your data type an instance of `PsoVect`.

The former is much easier, but the latter may be cleaner. I don't have
an example of these yet, but I'm working on one.

## Different output

Maybe you want to minimize a function 

    f :: (Double, Double) -> Int

or, more generally,
    
    f :: (Double, Double) -> b

That's fine. You need `b` to be an instance of the type `Grade`. In
`Calypso.Instance.Grade`, I include instances for

    Double
    Rational
    Float
    Int
    Integer
    Char

All are built so that `<` means "better than". If you want to make your
own type an instance of `Grade`, all you need to do is define either
`betterThan` or `worseThan`.

    class Grade b
      where
        betterThan :: b -> b -> Bool
        worseThan  :: b -> b -> Bool

There is also an instance for `Maybe b`, where `b` is one of the grades
above. See the section *Bounded Searching* for more details.

## I'd rather maximize

Maybe you want to *maximize* a function `f :: (Double, Double) ->
Double`. In that case, it's probably easiest to just minimize `negate .
f`.

However, if you'd rather not confuse yourself with extra `negate`s all
over, all you need to do is create a new instance of `Grade Double` with

    instance Grade Double
      where
        betterThan = (>)

You'll need to omit the module where the default instance is loaded, so
your import statements will look more like

    import Calypso.Core
    -- import Calypso.Instances.Grade
    import Calypso.Instances.PsoVect

## Bounded searching

Sometimes, we don't want to find the *global* minimum, just the minimum
subject to certain restrictions. For instance, suppose we want to
minimize
    
    f :: (Double, Double) -> Double
    f (x, y) = 4 * x + 2 * y - 3

subject to the constraint `1 <= x <= 3` and `0 <= y <= 2`. One way to
think about this is to modify our function:

    f' :: (Double, Double) -> Maybe Double
    f' (x, y)
        | withinBounds = Just $ 4 * x + 2 * y - 3
        | otherwise    = Nothing
      where
        withinBounds = 1 <= x &&
                       x <= 3 &&
                       0 <= y &&
                       y <= 2

We can read this as: outside of the bounds, our function is so bad that
you shouldn't even consider it. Just don't. Our particles are allowed to
leave the bounds, the function is just so bad out there that they will
eventually be drawn back in.

Side note: in the paper "Defining a standard for particle swarm
optimization" by Daniel Bratton and James Kennedy (2007), they discover
that this method of letting the particles "fly free", rather than
constraining them, performs very well in a variety of situations.

## More swarm control

Suppose that you want to have a bit more control of your swarm. You'd
like to choose a non-default update method, or a number of particles
other than 50 (the default number).

You'll want to call either `randomSwarm` or `createSwarm`.

    randomSwarm :: (PsoVect a, Random a, Grade b) 
        => StdGen   -- ^ A random seed
        -> Int      -- ^ Number of particles
        -> (a,a)    -- ^ Bounds to create particles in
        -> (a -> b)             -- ^ Function to optimize
        -> Updater a b          -- ^ Updater
        -> (Swarm a b, StdGen)  -- ^ (Swarm returned, new seed)
    createSwarm :: (PsoVect a, Grade b)
        => [a]          -- ^ Positions of of particles
        -> (a -> b)     -- ^ Function to optimize
        -> Updater a b  -- ^ Updater to use
        -> Swarm a b

Read more about `Updater`s in the next section. The only difference is
whether you have to supply the positions yourself or whether they will
be randomly generated. In general, you want this, so you'll probably be
using `randomSwarm`.

## Alternate parameters

In general, each step we update each particle's velocity, and then use
that velocity to update it's position. But there are a lot of ways to
update the particle's velocity.

All of them involve two things:

1. Find a pseudo-random vector guiding the particle towards its "private
   guide" (the best location it has found so far). Call it `vp`.
2. Find a pseudo-random vector guiding the particle towards its "local
   guide" (the best location it's local swarm has found). Call it `vl`

Now, take these two things, multiply them by some parameters, and add
them to the existing velocity (perhaps also multiplied by a parameter).
Multiply the whole thing by a parameter (maybe), and then, if you feel
like it, artificially reduce this to a maximum velocity. Also, some of
these parameters might by dynamic, and adjust over time.

That's a lot of options.

All of this is handled via the `Updater` data type. Here is it's
constructor:

    data Updater a b = Updater {
        newVel :: StdGen -> Particle a b -> PsoGuide a b -> Integer -> (a, StdGen)
        }

So an `Updater` is just a wrapper for a function that creates a new
velocity for a particle using

1. A `StdGen`, to do any necessary random calculations
2. A `Particle a b`, from which it can get position, velocity, and th
    private guide
3. A `PsoGuide a b`, the local guide for the particle
4. An `Int`, the current iteration of the swarm. Useful for parameters
    that adjust over time

But really, you probably don't want to create one in this way. It's just
useful to see what it is, and take the mystery out of it. Normally,
you'll use one of

    upDefault :: (PsoVect a, Grade b) => Updater a b
    upStandard :: (PsoVect a, Grade b)
        => Double  -- ^ Constriction parameter (chi)
        -> Double  -- ^ Tendancy toward private guide (c1)
        -> Double  -- ^ Tendancy toward local guide (c2)
        -> Updater a b
    upOriginal ::(PsoVect a, Grade b)
        => Double  -- ^ Tendancy toward private guide (c1)
        -> Double  -- ^ Tendancy toward local guide (c2)
        -> Updater a b
    upInertiaWeight :: (PsoVect a, Grade b)
        => Double  -- ^ Inertia weight (omega)
        -> Double  -- ^ Tendancy toward private guide (c1)
        -> Double  -- ^ Tendancy toward local guide (c2)
        -> Updater a b
    upInertiaWeightDynamic :: (PsoVect a, Grade b)
        => (Integer -> Double)  -- ^ Inertia weight (omega)
        -> Double               -- ^ Tendancy toward private guide (c1)
        -> Double               -- ^ Tendancy toward local guide (c2)
        -> Updater a b

These are all of the Updating methods that I found in the literature,
except for those that reduce to a maximum velocity (which may be used
with any of the above).

    upVMax :: (PsoSized a, Grade b) => Double -> Updater a b

If you want to add a maximum velocity to one of these other updaters,
you just combine them: `Updater a b` is an instance of `Monoid`! So

    upVMax 5000 <> upOriginal 2 2

is an updater using the methods of the original paper (from 1995), plus
a maximum velocity of 5000. You can actually use this `Monoid` structure
to create loads of interesting `Updater`s very easily:

    upStandard chi c1 c2 = (upScale chi) <> (upAddLocal c2) <> (upAddPrivate c1)

This is actually the definition of `upStandard` - it's just built from
three little `Updater`s. For more info, consult the documentation.

## Stack Overflow!

If you are using big swarms over a large-dimensional vector space for
many iteration, and `iterateSwarm` eventually gives you a stack
overflow, it may be because you are keeping track of every past version
of your swarm all at once.

If you aren't interested in all this data (and let's be honest, unless
you are examining the *process* of PSO, you aren't - you only care about
the result), you can use `iterateWhile`

    iterateWhile :: (PsoVect a, Grade b) 
        => (Swarm a b -> Bool)  -- ^ Condition to meet
        -> Swarm a b            -- ^ Swarm to update
        -> StdGen               -- ^ Random seed
        -> Swarm a b

It will update your swarm for as long as the condition specfied is met,
ignoring the past and proceeding blindly ahead. My tests indicate that
this doesn't build up the stack at all, but I do not yet understand
haskell profiling, so I make no guarantees. You want examples?

Iterate until variance of particles is below 0.001:

    iterateWhile ((> 0.001) . posVariance) s gen

Iterate until the grade of at least `good` is reached:

    iterateWhile ((`worseThan` good) . val . gGuide) s gen

Iterate `n` times:

    iterateWhile ((<n) . iteration) s gen

## More Questions?

Consult the documentation! It can be built with

    haddock -h -o docs Calypso.hs

Soon, I will have the library up on Hackage, and then you can browse the
docs there.

# Things to do

* Improve documentation
* Write example programs
* Write test suite
* Submit to Hackage

