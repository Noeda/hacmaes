{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Data.CMAES
  (
  -- * Functions that return lazy lists of models that get better
    cmaesOptimizeList
  , cmaesOptimizeList'
  , cmaesOptimizeListWithBatches
  , pickBest
  -- * Configuring how to run CMA-ES
  , defaultConfiguration
  , defaultInit
  -- ** Types
  , CMAESAlgo(..)
  , allAlgorithms
  , CMAESInit(..)
  , CMAESConfiguration(..)
  )
where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Primitive
import           Control.Monad.Trans.State.Strict
import           Data.Coerce
import           Data.Data
import           Data.Foldable
import           Data.Maybe
import qualified Data.Sequence                 as Seq
import           Data.Sequence                  ( Seq )
import           Data.IORef
import           Data.Traversable
import           Data.Word
import           Foreign.C.Types
import           Foreign.Marshal.Array
import           Foreign.Marshal.Utils
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.Storable
import           GHC.Generics
import           System.IO.Unsafe

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

-- | List of all `CMAESAlgo` values.
allAlgorithms :: [CMAESAlgo]
allAlgorithms = enumFromTo CMAES_DEFAULT VD_BIPOP_CMAES

cmaesAlgoToCInt :: CMAESAlgo -> CInt
cmaesAlgoToCInt CMAES_DEFAULT   = const_CMAES_DEFAULT
cmaesAlgoToCInt IPOP_CMAES      = const_IPOP_CMAES
cmaesAlgoToCInt BIPOP_CMAES     = const_BIPOP_CMAES
cmaesAlgoToCInt ACMAES          = const_aCMAES
cmaesAlgoToCInt AIPOP_CMAES     = const_aIPOP_CMAES
cmaesAlgoToCInt ABIPOP_CMAES    = const_aBIPOP_CMAES
cmaesAlgoToCInt SepCMAES        = const_sepCMAES
cmaesAlgoToCInt SepIPOP_CMAES   = const_sepIPOP_CMAES
cmaesAlgoToCInt SepBIPOP_CMAES  = const_sepBIPOP_CMAES
cmaesAlgoToCInt SepaCMAES       = const_sepaCMAES
cmaesAlgoToCInt SepaIPOP_CMAES  = const_sepaIPOP_CMAES
cmaesAlgoToCInt SepaBIPOP_CMAES = const_sepaBIPOP_CMAES
cmaesAlgoToCInt VD_CMAES        = const_VD_CMAES
cmaesAlgoToCInt VD_IPOP_CMAES   = const_VD_IPOP_CMAES
cmaesAlgoToCInt VD_BIPOP_CMAES  = const_VD_BIPOP_CMAES

type CEvaluator = Ptr CDouble -> Ptr CInt -> IO CDouble
type CIterator = IO ()

toBase :: Traversable f => f void -> [a] -> f a
toBase archetype lst = flip evalState lst $ for archetype $ \_ -> do
  result <- get
  case result of
    (x : rest) -> do
      put rest
      return x
    _ ->
      error
        "toBase: size of Traversable and list do not match, ran out of Traversable."
{-# INLINE toBase #-}

-- | Configuration of CMA-ES algorithm.
data CMAESConfiguration = CMAESConfiguration
  { sigma         :: !Double
  , lambda        :: !Int
  , algorithm     :: !CMAESAlgo
  , useSurrogates :: !Bool }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

defaultConfiguration :: CMAESConfiguration
defaultConfiguration = CMAESConfiguration { sigma         = 1
                                          , lambda        = 10
                                          , algorithm     = ACMAES
                                          , useSurrogates = False
                                          }

defaultInit :: f Double -> (f Double -> IO Double) -> CMAESInit f
defaultInit init ev =
  CMAESInit { iteration = return (), initialModel = init, evaluator = ev }

data CMAESInit f = CMAESInit
  { initialModel :: !(f Double)   -- ^ The first model.
  , iteration    :: !(IO ())      -- ^ Called each time a new population is generated.
  , evaluator    :: !(f Double -> IO Double) -- ^ Evaluate fitness of a model with this.
  }
  deriving ( Typeable, Generic )

-- | Returns a lazy list of successively tested models.
--
-- The list is not guaranteed (and typically) will not be in increasing order
-- of fitness because of the nature of CMA-ES optimization.
cmaesOptimizeList
  :: (MonadIO m, Traversable f)
  => CMAESInit f
  -> CMAESConfiguration
  -> m [f Double]
cmaesOptimizeList cmaes_init cmaes_conf = do
  lst <- cmaesOptimizeList' cmaes_init cmaes_conf
  return $ fmap snd lst
{-# INLINE cmaesOptimizeList #-}

data KillSilently = KillSilently
  deriving ( Show, Eq, Ord )

instance Exception KillSilently

-- | Alternative formulation of `cmaesOptimizeList`.
--
-- This function has a concept of batches. Imagine you are training a network
-- on huge amounts of data, more than we can load in the process. You can, at
-- each training batch, load randomly some part of this huge load of data.
-- CMA-ES will evaluate population on the same batch and then rank them. After
-- that, we get a new batch.
--
-- This is implemented in terms of `cmaesOptimizeList`.
--
-- This uses multi-threading and always attempts to keep at least 10 batches in
-- the queue. Additionally, all evaluation functions are run concurrently.
cmaesOptimizeListWithBatches
  :: forall m f training_batch
   . (MonadIO m, Traversable f)
  => f Double                    -- ^ Initial model
  -> IO training_batch           -- ^ Generate a training batch of data.
  -> (training_batch -> f Double -> IO Double)   -- ^ Fitness function.
  -> CMAESConfiguration
  -> m [(Double, f Double)]
cmaesOptimizeListWithBatches initial_model batch_generator evaluate_fitness config
  = liftIO $ mask $ \restore -> do
    -- Queue for getting results out.
    -- The contents: (sequence of (Double, f Double)], is_there_more_values_coming, exc)
    output_queue <- liftIO $ atomically $ newTVar (Seq.empty, True, Nothing)

    -- Launch off thread that will do all the work.
    tid          <- liftIO $ forkIOWithUnmask $ \restore -> do
      -- Handle exceptions, put them in 'output_queue'
      exc <- try $ restore $ worker output_queue
      case exc of
        Left exc ->
          atomically $ modifyTVar output_queue $ \(a, b, _) -> (a, b, Just exc)
        Right{} -> return ()

    -- Create a mule MVar that's only purpose is to attach a finalizer.
    gc_killer_mvar <- liftIO $ newMVar ()
    void $ mkWeakMVar gc_killer_mvar (throwTo tid KillSilently)

    -- Return an unsafeInterleaveIO thing that will consume 'output_queue'
    -- lazily.
    liftIO $ restore $ unsafeInterleaveIO $ exhaust output_queue gc_killer_mvar
 where
  exhaust
    :: TVar (Seq (Double, f Double), Bool, Maybe SomeException)
    -> MVar ()
    -> IO [(Double, f Double)]
  exhaust output_queue gc_killer_mvar = do
    touchMVar gc_killer_mvar
    next_item <- atomically $ do
      (sequence, more_values_coming, exc) <- readTVar output_queue
      case exc of
        Just some_exc -> throwSTM some_exc
        _             -> return ()
      case Seq.viewl sequence of
        Seq.EmptyL -> if more_values_coming then retry else return Nothing
        item Seq.:< rest_of_sequence -> do
          writeTVar output_queue (rest_of_sequence, more_values_coming, exc)
          return (Just item)
    case next_item of
      Nothing        -> return []
      Just next_item -> (next_item :)
        <$> unsafeInterleaveIO (exhaust output_queue gc_killer_mvar)

  worker output_queue = do
    -- Keep training batches in this queue.
    -- Set up queue for batch
    batch_queue <- atomically $ newTBQueue 10

    -- Launch off thread that obtains batches.
    withAsync (obtainBatches batch_queue) $ \batch_obtainer_async -> do
      link batch_obtainer_async

      first_batch <- grabNextBatch batch_queue
      batch_tvar  <- newTVarIO first_batch

      let init = (defaultInit initial_model (evaluator batch_tvar))
            { iteration = grabNextBatch batch_queue
                          >>= atomically
                          .   writeTVar batch_tvar
            }

      lst <- cmaesOptimizeList' init config
      for_ lst $ \(fitness, model) ->
        atomically $ modifyTVar output_queue $ \(seq, more_values, exc) ->
          (seq Seq.|> (fitness, model), more_values, exc)
      atomically $ modifyTVar output_queue $ \(seq, _more_values, exc) ->
        (seq, False, exc)

  grabNextBatch batch_queue = atomically $ readTBQueue batch_queue

  evaluator batch_tvar model = do
    batch <- atomically $ readTVar batch_tvar
    evaluate_fitness batch model

  obtainBatches batch_queue = forever $ do
    batch <- batch_generator
    atomically $ writeTBQueue batch_queue batch


{-# NOINLINE touchMVar #-}
touchMVar :: MVar () -> IO ()
touchMVar !mvar = do
  v <- readMVar mvar
  v `seq` return v

-- | Combinator designed to use with `cmaesOptimizeList'` to pick out the best
-- candidate.
--
-- It will wait until the list exhausts and then returns whatever candidate had
-- highest score.
pickBest :: (Monad m, Ord score) => m [(score, candidate)] -> m candidate
pickBest list_of_candidates = do
  lst <- list_of_candidates
  when (null lst) $ error "pickBest: no candidates."
  return $ snd $ go (tail lst) (head lst)
 where
  go [] best = best
  go ((candidate_score, candidate) : rest) (best_score_yet, best_candidate) =
    if candidate_score < best_score_yet
      then go rest (candidate_score, candidate)
      else go rest (best_score_yet, best_candidate)

-- | Returns a lazy list of successively tested models.
--
-- The list is not guaranteed (and typically) will not be in increasing order
-- of fitness because of the nature of CMA-ES optimization.
--
-- This variation also returns the measured fitnesses of each tested model.
cmaesOptimizeList'
  :: (MonadIO m, Traversable f)
  => CMAESInit f
  -> CMAESConfiguration
  -> m [(Double, f Double)]
cmaesOptimizeList' cmaes_init cmaes_conf = liftIO $ mask_ $ do
  is_dead  <- newIORef False
  lst_mvar <- newTVarIO Nothing
  exc_ref  <- newTVarIO Nothing
  last     <- newTVarIO False
  wrapping <- mkEvaluate $ \test_arr dead_poker -> do
    dead <- readIORef is_dead
    if dead
      then do
        poke dead_poker 1
        return 0.0
      else
        try
            (do
              lst <- peekArray num_values test_arr
              let strut = toBase (initialModel cmaes_init) $ coerce lst
              value <- evaluator cmaes_init strut

              atomically $ do
                old <- readTVar lst_mvar
                when (isJust old) retry
                writeTVar lst_mvar (Just (value, strut))

              return $ CDouble value
            )
          >>= \case
                Left (exc :: SomeException) -> do
                  poke dead_poker 1
                  writeIORef is_dead True
                  atomically $ writeTVar exc_ref $ Just exc
                  return 0.0
                Right ok -> return ok

  wrapping2 <- mkIterate $ try checker >>= \case
    Left (exc :: SomeException) -> do
      writeIORef is_dead True
      atomically $ writeTVar exc_ref $ Just exc
    Right _ -> return ()

  farr <- mallocForeignPtrArray (length initial_value_list)
  withForeignPtr farr $ \farr_ptr ->
    withArray initial_value_list $ \initial_value_arr -> copyBytes
      farr_ptr
      initial_value_arr
      (length initial_value_list * sizeOf (undefined :: Double))

  finalizer <- newMVar ()
  withForeignPtr farr $ \farr_ptr -> void $ forkIOWithUnmask $ \unmask ->
    unmask $ do
      cmaes_optimize use_surrogates
                     algo
                     (castPtr farr_ptr)
                     (CDouble $ sigma cmaes_conf)
                     (fromIntegral $ lambda cmaes_conf)
                     (fromIntegral num_values)
                     wrapping
                     wrapping2
      freeHaskellFunPtr wrapping
      freeHaskellFunPtr wrapping2
      atomically $ writeTVar last True
  void $ mkWeakMVar finalizer $ do
    writeIORef is_dead True
    atomically $ writeTVar lst_mvar Nothing

  unsafeInterleaveIO $ go exc_ref lst_mvar finalizer farr last
 where
  use_surrogates     = if useSurrogates cmaes_conf then 1 else 0
  algo               = cmaesAlgoToCInt (algorithm cmaes_conf)
  checker            = iteration cmaes_init

  initial_value_list = toList $ initialModel cmaes_init
  num_values         = length initial_value_list

  go exc_ref lst_mvar finalizer farr last = do
    next <- atomically $ do
      exc      <- readTVar exc_ref
      val      <- readTVar lst_mvar
      finished <- readTVar last
      when (isNothing exc && isNothing val && not finished) retry
      case val of
        Just{} -> writeTVar lst_mvar Nothing
        _      -> return ()
      return (exc, val, finished)
    case next of
      (Just exc, _      , _) -> throwIO exc
      (Nothing , Just v', _) -> do
        v <- evaluate v'
        withMVar finalizer $ \unit -> touch unit
        touchForeignPtr farr
        (v :) <$> unsafeInterleaveIO (go exc_ref lst_mvar finalizer farr last)
      (_, _, True) -> return []
      (_, _, _   ) -> error "impossible."
