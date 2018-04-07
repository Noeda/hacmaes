{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Data.CMAES
  ( cmaesOptimizeList
  , cmaesOptimizeList'
  , defaultConfiguration
  , defaultInit
  , allAlgorithms
  , lambdaSuggestion
  , pickBest
  , CMAESAlgo(..)
  , CMAESInit(..)
  , CMAESConfiguration(..) )
  where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Primitive
import Control.Monad.Trans.State.Strict
import Data.Coerce
import Data.Data
import Data.Foldable
import Data.Maybe
import Data.IORef
import Data.Traversable
import Data.Word
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics
import System.IO.Unsafe

foreign import ccall safe cmaes_optimize :: CInt -> CInt -> Ptr CDouble -> CDouble -> CInt -> Word64 -> FunPtr CEvaluator -> FunPtr CIterator -> IO ()
foreign import ccall "wrapper" mkEvaluate :: CEvaluator -> IO (FunPtr CEvaluator)
foreign import ccall "wrapper" mkIterate :: CIterator -> IO (FunPtr CIterator)

foreign import ccall safe const_CMAES_DEFAULT :: CInt
foreign import ccall safe const_IPOP_CMAES :: CInt
foreign import ccall safe const_BIPOP_CMAES :: CInt
foreign import ccall safe const_aCMAES :: CInt
foreign import ccall safe const_aIPOP_CMAES :: CInt
foreign import ccall safe const_aBIPOP_CMAES :: CInt
foreign import ccall safe const_sepCMAES :: CInt
foreign import ccall safe const_sepIPOP_CMAES :: CInt
foreign import ccall safe const_sepBIPOP_CMAES :: CInt
foreign import ccall safe const_sepaCMAES :: CInt
foreign import ccall safe const_sepaIPOP_CMAES :: CInt
foreign import ccall safe const_sepaBIPOP_CMAES :: CInt
foreign import ccall safe const_VD_CMAES :: CInt
foreign import ccall safe const_VD_IPOP_CMAES :: CInt
foreign import ccall safe const_VD_BIPOP_CMAES :: CInt

data CMAESAlgo
  = CMAES_DEFAULT
  | IPOP_CMAES
  | BIPOP_CMAES
  | ACMAES
  | AIPOP_CMAES
  | ABIPOP_CMAES
  | SepCMAES
  | SepIPOP_CMAES
  | SepBIPOP_CMAES
  | SepaCMAES
  | SepaIPOP_CMAES
  | SepaBIPOP_CMAES
  | VD_CMAES
  | VD_IPOP_CMAES
  | VD_BIPOP_CMAES
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum )

allAlgorithms :: [CMAESAlgo]
allAlgorithms = enumFromTo CMAES_DEFAULT VD_BIPOP_CMAES

cmaesAlgoToCInt :: CMAESAlgo -> CInt
cmaesAlgoToCInt CMAES_DEFAULT = const_CMAES_DEFAULT
cmaesAlgoToCInt IPOP_CMAES = const_IPOP_CMAES
cmaesAlgoToCInt BIPOP_CMAES = const_BIPOP_CMAES
cmaesAlgoToCInt ACMAES = const_aCMAES
cmaesAlgoToCInt AIPOP_CMAES = const_aIPOP_CMAES
cmaesAlgoToCInt ABIPOP_CMAES = const_aBIPOP_CMAES
cmaesAlgoToCInt SepCMAES = const_sepCMAES
cmaesAlgoToCInt SepIPOP_CMAES = const_sepIPOP_CMAES
cmaesAlgoToCInt SepBIPOP_CMAES = const_sepBIPOP_CMAES
cmaesAlgoToCInt SepaCMAES = const_sepaCMAES
cmaesAlgoToCInt SepaIPOP_CMAES = const_sepaIPOP_CMAES
cmaesAlgoToCInt SepaBIPOP_CMAES = const_sepaBIPOP_CMAES
cmaesAlgoToCInt VD_CMAES = const_VD_CMAES
cmaesAlgoToCInt VD_IPOP_CMAES = const_VD_IPOP_CMAES
cmaesAlgoToCInt VD_BIPOP_CMAES = const_VD_BIPOP_CMAES

type CEvaluator = Ptr CDouble -> Ptr CInt -> IO CDouble
type CIterator = IO ()

toBase :: Traversable f => f void -> [a] -> f a
toBase archetype lst = flip evalState lst $ for archetype $ \_ -> do
  (x:rest) <- get
  put rest
  return x
{-# INLINE toBase #-}

data CMAESConfiguration = CMAESConfiguration
  { sigma         :: !Double
  , lambda        :: !Int
  , algorithm     :: !CMAESAlgo
  , useSurrogates :: !Bool }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

defaultConfiguration :: CMAESConfiguration
defaultConfiguration = CMAESConfiguration
  { sigma = 1
  , lambda = 10
  , algorithm = ACMAES
  , useSurrogates = False }

defaultInit :: f Double -> (f Double -> IO Double) -> CMAESInit f
defaultInit init ev = CMAESInit
  { iteration = return ()
  , initialModel = init
  , evaluator = ev }

data CMAESInit f = CMAESInit
  { initialModel :: !(f Double)
  , iteration    :: !(IO ())
  , evaluator    :: !(f Double -> IO Double) }
  deriving ( Typeable, Generic )

-- | Returns a lazy list of successively tested models.
--
-- The list is not guaranteed (and typically) will not be in increasing order
-- of fitness because of the nature of CMA-ES optimization.
cmaesOptimizeList :: (MonadIO m, Traversable f)
                  => CMAESInit f
                  -> CMAESConfiguration
                  -> m [f Double]
cmaesOptimizeList cmaes_init cmaes_conf = do
  lst <- cmaesOptimizeList' cmaes_init cmaes_conf
  return $ fmap snd lst
{-# INLINE cmaesOptimizeList #-}

-- | Combinator designed to use with cmaesOptimizeList' to pick out the best
-- candidate.
pickBest :: (Monad m, Ord score)
         => m [(score, candidate)]
         -> m candidate
pickBest list_of_candidates = do
  lst <- list_of_candidates
  when (null lst) $
    error "pickBest: no candidates."
  return $ snd $ go (tail lst) (head lst)
 where
  go [] best = best
  go ((candidate_score, candidate):rest) (best_score_yet, best_candidate) =
    if candidate_score < best_score_yet
      then go rest (candidate_score, candidate)
      else go rest (best_score_yet, best_candidate)

-- | Returns a lazy list of successively tested models.
--
-- The list is not guaranteed (and typically) will not be in increasing order
-- of fitness because of the nature of CMA-ES optimization.
--
-- This variation also returns the measured fitnesses of each tested model.
cmaesOptimizeList' :: (MonadIO m, Traversable f)
                   => CMAESInit f
                   -> CMAESConfiguration
                   -> m [(Double, f Double)]
cmaesOptimizeList' cmaes_init cmaes_conf = liftIO $ mask_ $ do
  is_dead <- newIORef False
  lst_mvar <- newTVarIO Nothing
  exc_ref <- newTVarIO Nothing
  last <- newTVarIO False
  wrapping <- mkEvaluate $ \test_arr dead_poker -> do
    dead <- readIORef is_dead
    if dead
      then do poke dead_poker 1
              return 0.0
      else try (do lst <- peekArray num_values test_arr
                   let strut = toBase (initialModel cmaes_init) $ coerce lst
                   value <- evaluator cmaes_init strut

                   atomically $ do
                     old <- readTVar lst_mvar
                     when (isJust old) retry
                     writeTVar lst_mvar (Just (value, strut))

                   return $ CDouble value) >>= \case
             Left (exc :: SomeException) -> do
               poke dead_poker 1
               writeIORef is_dead True
               atomically $ writeTVar exc_ref $ Just exc
               return 0.0
             Right ok -> return ok

  wrapping2 <- mkIterate $
    try checker >>= \case
      Left (exc :: SomeException) -> do
        writeIORef is_dead True
        atomically $ writeTVar exc_ref $ Just exc
      Right _ -> return ()

  farr <- mallocForeignPtrArray (length initial_value_list)
  withForeignPtr farr $ \farr_ptr ->
    withArray initial_value_list $ \initial_value_arr ->
      copyBytes farr_ptr initial_value_arr (length initial_value_list * sizeOf (undefined :: Double))

  finalizer <- newMVar ()
  withForeignPtr farr $ \farr_ptr ->
    void $ forkIOWithUnmask $ \unmask -> unmask $ do
             cmaes_optimize use_surrogates algo (castPtr farr_ptr) (CDouble $ sigma cmaes_conf) (fromIntegral $ lambda cmaes_conf) (fromIntegral num_values) wrapping wrapping2
             freeHaskellFunPtr wrapping
             freeHaskellFunPtr wrapping2
             atomically $ writeTVar last True
  void $ mkWeakMVar finalizer $ do
    writeIORef is_dead True
    atomically $ writeTVar lst_mvar Nothing

  unsafeInterleaveIO $ go exc_ref lst_mvar finalizer farr last
 where
  use_surrogates = if useSurrogates cmaes_conf then 1 else 0
  algo = cmaesAlgoToCInt (algorithm cmaes_conf)
  checker = iteration cmaes_init

  initial_value_list = toList $ initialModel cmaes_init
  num_values = length initial_value_list

  go exc_ref lst_mvar finalizer farr last = do
    next <- atomically $ do
      exc <- readTVar exc_ref
      val <- readTVar lst_mvar
      finished <- readTVar last
      when (isNothing exc && isNothing val && not finished) retry
      case val of
        Just{} -> writeTVar lst_mvar Nothing
        _ -> return ()
      return (exc, val, finished)
    case next of
      (Just exc, _, _) -> throwIO exc
      (Nothing, Just v', _) -> do
        v <- evaluate v'
        withMVar finalizer $ \unit -> touch unit
        touchForeignPtr farr
        (v:) <$> unsafeInterleaveIO (go exc_ref lst_mvar finalizer farr last)
      (_, _, True) -> return []
      (_, _, _) -> error "impossible."

lambdaSuggestion :: Traversable f => f a -> Int
lambdaSuggestion model =
  let num_params = length $ toList model
   in 4 + floor (3 * log (fromIntegral num_params :: Double))

