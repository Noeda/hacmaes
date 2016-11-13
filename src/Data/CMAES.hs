module Data.CMAES
  ( cmaesOptimize )
  where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.Coerce
import Data.Foldable
import Data.IORef
import Data.Traversable
import Data.Word
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

foreign import ccall safe cmaes_optimize :: Ptr CDouble -> CDouble -> CDouble -> Word64 -> FunPtr CEvaluator -> IO ()
foreign import ccall "wrapper" mkEvaluate :: CEvaluator -> IO (FunPtr CEvaluator)

type CEvaluator = Ptr CDouble -> Ptr CInt -> IO CDouble

toBase :: Traversable f => f void -> [a] -> f a
toBase archetype lst = flip evalState lst $ for archetype $ \_ -> do
  (x:rest) <- get
  put rest
  return x
{-# INLINE toBase #-}

cmaesOptimize :: Traversable f => f Double -> Double -> Int -> (f Double -> IO Double) -> IO ()
cmaesOptimize initial_value sigma lambda evaluator = mask $ \restore -> do
  is_dead <- newIORef False
  wrapping <- mkEvaluate $ \test_arr dead_poker -> do
    dead <- readIORef is_dead
    if dead
      then do poke dead_poker 1
              return 0.0
      else do lst <- peekArray num_values test_arr
              fmap CDouble $ evaluator $ toBase initial_value $ coerce lst

  flip finally (writeIORef is_dead True) $ do
    withArray initial_value_list $ \initial_value_arr -> do
      done <- newEmptyMVar
      void $ forkIO $ do
               cmaes_optimize (castPtr initial_value_arr) (CDouble sigma) (fromIntegral lambda) (fromIntegral num_values) wrapping
               freeHaskellFunPtr wrapping
               putMVar done ()
      restore $ takeMVar done
 where
  initial_value_list = toList initial_value
  num_values = length initial_value_list
