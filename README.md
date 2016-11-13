Very simple Haskell binding to CMA-ES C++ library. Optimize annoying
non-smooth, non-differentiable objective functions.

Exports just one function:

    cmaesOptimize :: Traversable f => f Double -> Double -> Int -> (f Double -> IO Double) -> IO ()

`cmaesOptimize` returns when it decides it cannot improve on the solution
anymore.

Example:

    optimizeSphereFunction :: IO ()
    optimizeSphereFunction =
      cmaesOptimize [3.0, 4.0]    -- starting point
                    1.0           -- sigma (refer to CMA-ES)
                    5             -- lambda (population size, refer to CMA-ES)
                    (\[x, y] -> print (x, y) >> return (sqrt $ (x*x) + (y*y))) -- cost function

This library requires libcmaes to be installed to work.

