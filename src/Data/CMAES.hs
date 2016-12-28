module Data.CMAES
  ( cmaesOptimize
  , randomOptimize )
  where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.Coerce
import Data.Foldable
import Data.List ( sortBy )
import Data.Ord ( comparing )
import Data.IORef
import Data.Traversable
import Data.Word
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe ( unsafePerformIO )
import System.Random.MWC
import System.Random.MWC.Distributions

foreign import ccall safe cmaes_optimize :: Ptr CDouble -> CDouble -> CInt -> Word64 -> FunPtr CEvaluator -> FunPtr CIterator -> IO ()
foreign import ccall "wrapper" mkEvaluate :: CEvaluator -> IO (FunPtr CEvaluator)
foreign import ccall "wrapper" mkIterate :: CIterator -> IO (FunPtr CIterator)

type CEvaluator = Ptr CDouble -> Ptr CInt -> IO CDouble
type CIterator = IO ()

toBase :: Traversable f => f void -> [a] -> f a
toBase archetype lst = flip evalState lst $ for archetype $ \_ -> do
  (x:rest) <- get
  put rest
  return x
{-# INLINE toBase #-}

-- | Optimizes using CMA-ES algorithm.
cmaesOptimize :: Traversable f
              => f Double  -- ^ Initial model
              -> Double    -- ^ Sigma
              -> Int       -- ^ Lambda
              -> (f Double -> IO Double)  -- ^ Evaluate score of a model
              -> IO ()                    -- ^ Called once after each round
              -> IO (f Double)            -- ^ Returns optimized model.
cmaesOptimize initial_value sigma lambda evaluator iterator = mask $ \restore -> do
  is_dead <- newIORef False
  wrapping <- mkEvaluate $ \test_arr dead_poker -> do
    dead <- readIORef is_dead
    if dead
      then do poke dead_poker 1
              return 0.0
      else do lst <- peekArray num_values test_arr
              fmap CDouble $ evaluator $ toBase initial_value $ coerce lst

  wrapping2 <- mkIterate iterator

  flip finally (writeIORef is_dead True) $ do
    withArray initial_value_list $ \initial_value_arr -> do
      done <- newEmptyMVar
      void $ forkIO $ do
               cmaes_optimize (castPtr initial_value_arr) (CDouble sigma) (fromIntegral lambda) (fromIntegral num_values) wrapping wrapping2
               freeHaskellFunPtr wrapping
               freeHaskellFunPtr wrapping2
               putMVar done ()
      restore $ takeMVar done

      lst <- peekArray num_values initial_value_arr
      return $ toBase initial_value $ coerce lst
 where
  initial_value_list = toList initial_value
  num_values = length initial_value_list

globalRng :: GenIO
globalRng = unsafePerformIO createSystemRandom
{-# NOINLINE globalRng #-}

makePerturbed :: Traversable f => f Double -> Double -> IO (f Double)
makePerturbed model sigma = for model $ \value -> do
  perturbing <- normal 0.0 sigma globalRng
  return $ value + perturbing
{-# INLINE makePerturbed #-}

-- | Optimizes using a very simple random walk algorithm.
--
-- This can be used as a baseline to compare between non-smart but working
-- optimizer against CMA-ES.
--
-- The type signature is deliberately exactly the same as in `cmaesOptimize`.
randomOptimize :: Traversable f
               => f Double
               -> Double
               -> Int
               -> (f Double -> IO Double)
               -> IO ()
               -> IO (f Double)
randomOptimize initial_model sigma lambda evaluator iterator = do
  score <- evaluator initial_model
  loop_it initial_model score
 where
  loop_it initial_model score = do
    descendants <- replicateM lambda $ makePerturbed initial_model sigma
    descendants_score <- for descendants $ \descendant -> do
      score <- evaluator descendant
      return (descendant, score)

    let (best_descendant, best_descendant_score) = head $ sortBy (comparing snd) descendants_score

    iterator

    if best_descendant_score < score
      then loop_it best_descendant best_descendant_score
      else loop_it initial_model score

