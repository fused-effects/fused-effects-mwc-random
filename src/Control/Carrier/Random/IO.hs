{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators,
             UndecidableInstances #-}

module Control.Carrier.Random.IO
  ( RandomC (..)
  , runRandomSeed
  , runRandom
    -- * Random effect
  , module Control.Effect.Random
  ) where

import           Control.Algebra
import           Control.Carrier.Reader
import           Control.Effect.Random
import           Control.Monad.IO.Class
import           Data.Vector.Generic (Vector)
import           Data.Word
import qualified System.Random.MWC as MWC

newtype RandomC m a = RandomC (ReaderC MWC.GenIO m a)
  deriving (Applicative, Functor, Monad, MonadIO)

instance (Algebra sig m, MonadIO m) => Algebra (Random :+: sig) (RandomC m) where
  alg (L (WithGen fn k)) = do
    gen <- RandomC ask
    liftIO (fn gen) >>= k
  alg (R other) = RandomC (alg (R (handleCoercible other)))
  {-# INLINE alg #-}

runRandomSeed :: (MonadIO m, Vector v Word32) => v Word32 -> RandomC m a -> m a
runRandomSeed seed (RandomC act) = do
  rand <- liftIO $ MWC.initialize seed
  runReader rand act

-- | Run a computation, seeding its random values from the system random number generator.
--
-- This is the de facto standard way to use this carrier. Keep in mind that seeding the RNG
-- may be a computationally intensive process, so if you need to call this function in a tight
-- loop, either use 'runRandomSeed' or explore alternative architectures.
runRandom :: MonadIO m => RandomC m a -> m a
runRandom (RandomC act) = do
  rand <- liftIO MWC.createSystemRandom
  runReader rand act
