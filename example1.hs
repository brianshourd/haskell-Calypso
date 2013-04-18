import Calypso

import Data.Function (on)
import Data.List (groupBy)
import System.Random (mkStdGen)

{-
In this example, we'd like to use Calypso to find the minimum of the
function f (x,y) = x^2 + y^2.

This is a basic overview of Calypso usage.
-}

f :: (Double, Double) -> Double
f (x,y) = x^2 + y^2

{-
We see from the type signature of f that we will need (Double, Double)
to be an instance of PSOVect. Fortunately, this is one of the instances
that is included with Calypso, so we don't actually need to do anything
about that.

So the next thing that we must do is create a swarm. We don't care much about specifics yet, so we'll just use the defaultSwarm. We'll start looking with
particles in the range ((-5,-5)) to ((5,5)).
-}
main = do
    -- We need a standard generator to call defaultSwarm - this always
    -- gets the same one (since this is an example, it's nice that the
    -- result doesn't depend on chance). You could use newStdGen to get
    -- one from IO (see docs on System.Random for more info)
    let gen = mkStdGen 0

    -- When we call defaultSwarm, we get a new standard generator back,
    -- so that we can use it for more random things (so it behaves like
    -- random and randomR).
    let (s, gen') = defaultSwarm f ((-5, -5), (5, 5)) gen
    
    -- Now we'll repeatedly update the swarm, getting a list of
    -- progressivly better swarms. iterateSwarm absorbs a stdGen
    -- (behaving like randoms and randomRs).
    let ss = iterateSwarm s gen'

    -- Our answer is probably somewhere in there: gGuide gets the
    -- PsoGuide from a swarm, which contains both the best value found
    -- and the point at which it was found
    let bs = map gGuide ss

    printExplanation
    
    -- Let's print some of these guides, just to see how things are
    -- going
    putStrLn "Some guides at iteration i:"
    sequence_ $ printTheseItems bs [1, 5, 10, 50, 100, 1000]

    -- Ok, how can we tell if we've got an answer or not?
    -- One method is to wait until we stop seeing improvements.
    -- But this is highly variable, and doesn't necessarily indicate
    -- that we won't see improvements later.
    putStrLn "\nNo improvements for 20 steps:"
    putStrLn . show . head . head . dropWhile ((<=20) . length) . groupBy ((==) `on` val) $ bs

    -- Another method is to wait until all the particles are really
    -- close together. This is a good indicator that we can stop
    -- spending CPU cycles, since even if this is the wrong answer, we
    -- aren't likely to do better unless we start over. We can use the
    -- posVariance function for this.
    putStrLn "\nVariance of particle positions below 0.000001:"
    putStrLn . show . gGuide . head . dropWhile ((> 0.000001) . posVariance) $ ss

-- A helper function, useful for printing out selections from a list
printTheseItems :: (Show a) => [a] -> [Int] -> [IO ()]
printTheseItems xs = map (\i -> putStrLn $ (show i) ++ ": " ++ (show $ xs !! i))

-- Print out an explanation when the program is run
printExplanation :: IO ()
printExplanation = putStrLn "This program calculates the minimum of the function x^2 + y^2 using particle swarm optimization, giving helpful status updates along the way."
