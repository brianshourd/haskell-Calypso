import Calypso

import System.Random (StdGen, mkStdGen)

{-
In this example, we'll put Calypso through its paces, trying out lot's
of test functions, all from the paper

Molga, Marcin, and Czesław Smutnicki. "Test functions for optimization
needs." Test functions for optimization needs (2005).

Warning: this may take a little while to run.
-}

-- We'll need to write these a lot
type Point = (Double, Double, Double)
type Bound = (Point, Point)

-- =====================
-- Functions to minimize
-- =====================

-- Rastrigin's function
-- Global minimum: 0 at (0, 0, 0)
-- Bounds: -5.12 <= x, y, z <= 5.12
rast :: Bound -> Point -> Maybe Double
rast b = rast' `boundTo` b
  where
    rast' (x, y, z) = 30 + (sum $ map term [x, y, z])
    term u = u^2 - 10 * cos (2 * pi * u)

-- Schwefel's function
-- Global minimum: -1256.9487 at (420.9787, 420.9787, 420.9787):
-- Bounds: -500 <= x, y, z <= 500
schw :: Bound -> Point -> Maybe Double
schw b = schw' `boundTo` b
  where
    schw' (x, y, z) = sum . map term $ [x, y, z]
    term u = (-u) * (sin . sqrt . abs $ u)

-- Griewangk's function
-- Global minimum: 0 at (0, 0, 0)
-- Bounds: -600 <= x, y, z <= 600
grie :: Bound -> Point -> Maybe Double
grie b  = grie' `boundTo` b
  where
    grie' (x, y, z) = term1 - term2 + 1
      where
        term1 = (1.0 / 4000.0) * (x^2 + y^2 + z^2)
        term2 = (cos x) * (cos (y / (sqrt 2))) * (cos (z / (sqrt 3)))

-- Rosenbrock's valley
-- Global minimum: 0 
-- Bounds: -2.048 <= xi <= 2.048
rose :: Bound -> Point -> Maybe Double
rose b = rose' `boundTo` b 
  where
    rose' (x, y, z) = 100 * (y - x^2)^2 + (1 - x)^2 + 100 * (z - y^2)^2
        + (1 - y)^2

-- DeJong's function
-- Global minimum: 0
-- Bounds: -5.12 <= xi <= 5.12
dejo :: (PsoList Double, PsoList Double) -> PsoList Double -> Maybe Double
dejo b = dejo' `boundTo` b
  where
    dejo' = sum . map (^2) . toList

-- Ackley's function
-- Global minimum: 0 at (0, 0, 0)
-- Bounds: -32.768 <= x <= 32.768
ackl :: Bound -> Point -> Maybe Double
ackl b (x, y, z) = (ackl' `boundTo` b) (x, y, z)
  where
    ackl' (x, y, z) = -20 * exp (-0.2 * sqrt ((1.0 / 3.0) * term1)) -
        exp ((1.0 / 3.0) * term2) + 20 + exp 1.0
    term1 = x^2 + y^2 + z^2
    term2 = sum . map (cos . (* (2 * pi))) $ [x, y, z]

-- Michalewicz's function
-- Global minimum: approx. -9.66 (10 dimensions)
-- Bounds: 0 <= xi <= pi
mich :: (PsoList Double, PsoList Double) -> PsoList Double -> Maybe Double
mich b = mich' `boundTo` b
  where
    mich' xs = negate . sum . map term . zip (toList xs) $ [1..]
    term (x, i) = (sin x) * (sin ((i * x^2) / pi))^20

-- Branins's function
-- Global minimum: 0.397887 at three locations
-- No specified search bounds ((-100,-100), (100,100))?
bran :: ((Double, Double), (Double, Double)) -> (Double, Double) -> Maybe Double
bran bounds = bran' `boundTo` bounds
  where
    bran' (x, y) = (y - b * x^2 + c * x - d)^2 + e * (1 - f) * (cos x) + e
    b = 5.1 / (4 * pi^2)
    c = 5.0 / pi
    d = 6.0
    e = 10.0
    f = 1.0 / (8 * pi)

-- Easom's function
-- Global minimum: -1 at (pi, pi)
-- Bounds: -100 <= x, y <= 100
easo :: ((Double, Double), (Double, Double)) -> (Double, Double) -> Maybe Double
easo bounds = easo' `boundTo` bounds
  where
    easo' (x, y) = (-(cos x)) * (cos y) * exp ((-(x - pi)^2) - (y - pi)^2)

-- Six-hump camel back function
-- Global minimum: -1.0316 at two points
-- Bounds: ((-3,-2), (3,2))
sixh :: ((Double, Double), (Double, Double)) -> (Double, Double) -> Maybe Double
sixh bounds = sixh' `boundTo` bounds
  where
    sixh' (x, y) = (4 - 2.1 * x^2 + (x^4) / 3.0) * x^2 + x * y + 
        (-4 + 4 * y^2) * y^2

-- Drop wave function
-- Global minimum: not given
-- Bounds: -5.12 <= x, y <= 5.12
drpw :: ((Double, Double), (Double, Double)) -> (Double, Double) -> Maybe Double
drpw bounds = drpw' `boundTo` bounds
  where
    drpw' (x, y) = -((1 + (cos (12 * sqrt (x^2 + y^2)))) / ((x^2 + y^2) / 2.0 + 2)) 

-- ==================
-- IO helper function
-- ==================

-- Calculate and print results for when the particles have converged to
-- a total variance of less than epsilon
printResults f bounds gen epsilon = putStrLn $ "Converges after " ++
    (show $ iteration s') ++ " iterations:\n" ++ (show $ gGuide s')
  where
    (s, gen') = defaultSwarm (f bounds) bounds gen
    s' = iterateWhile ((> epsilon) . posVariance) s gen'

main = do
    putStrLn $ "We're putting the library through it's paces here. This"
        ++ " is using exclusively default settings, so some answers are"
        ++ " incorrect, but you can see a bit how the library performs"
        ++ " without any human judgement.\n"

    let gen = mkStdGen 0

    putStrLn "Rastrigin's function, minimum 0 at (0, 0, 0):"
    printResults rast ((-5.12,-5.12,-5.12), (5.12,5.12,5.12)) gen 0.001

    putStrLn "\nSchwefel's function, minimum -1256.9487 at (420.9787, 420.9787, 420.9787):"
    printResults schw ((-500,-500,-500), (500,500,500)) gen 0.001

    putStrLn "\nGriewangk's function, minimum 0 at (0, 0, 0):"
    printResults grie ((-600,-600,-600), (600,600,600)) gen 0.001

    putStrLn "\nRosenbrock's function, minimum 0 at (1, 1, 1):"
    printResults rose ((-2.048,-2.048,-2.048), (2.048,2.048,2.048)) gen 0.001

    putStrLn "\nDeJong's function (50 variables), minimum 0 at [0, 0,..., 0]:"
    printResults dejo (fromList $ replicate 50 (-5.12), fromList $ replicate 50 5.12) gen 0.001

    putStrLn "\nAckley's function, minimum 0 at (0, 0, 0):"
    printResults ackl ((-32.768, -32.768, -32.768), (32.768, 32.768, 32.768)) gen 0.001

    putStrLn "\nMichalewicz's function, minimum approx. -4.687:"
    printResults mich ((fromList $ replicate 5 0), (fromList $ replicate 5 pi)) gen 0.001

    putStrLn "\nBranins's function, minimum 0.397887 at three locations:"
    printResults bran ((-100,-100), (100,100)) gen 0.001

    putStrLn "\nEasom's function, minimum -1 at (pi, pi):"
    printResults easo ((-100,-100), (100,100)) gen 0.001

    putStrLn "\nSix hump function, minimum -1.0316 at two points:"
    printResults sixh ((-3,-2), (3,2)) gen 0.001

    putStrLn "\nDrop wave function, minimum not given:"
    printResults drpw ((-5.12,-5.12), (5.12,5.12)) gen 0.001
