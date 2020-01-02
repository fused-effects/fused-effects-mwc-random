{-# LANGUAGE AllowAmbiguousTypes, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, TypeOperators, UndecidableInstances #-}

module Control.Carrier.Random.IO
  ( RandomC (..)
  , runRandom
    -- * Random effect
  , module Control.Effect.Random
  ) where

import           Control.Algebra
import           Control.Carrier.Lift
import           Control.Carrier.Reader
import           Control.Effect.Random
import           Control.Effect.Sum
import           Control.Monad.IO.Class
import           Control.Monad.Primitive
import qualified System.Random.MWC as MWC

newtype RandomC prim m a = RandomC (ReaderC (MWC.Gen (PrimState prim)) m a)
  deriving (Applicative, Functor, Monad, MonadIO)

instance forall m n sig . (Algebra sig m, Member (Lift n) sig, PrimMonad n) => Algebra (Random :+: sig) (RandomC n m) where
  alg (L act) = do
    gen <- RandomC (ask @(MWC.Gen (PrimState n)))
    case act of
      Uniform k -> sendM @n (MWC.uniform gen) >>= k
  alg (R other) = RandomC (alg (R (handleCoercible other)))
  {-# INLINE alg #-}

-- | Run a computation, seeding its random values from the system random number generator.
--
-- This is the de facto standard way to use this carrier. Keep in mind that seeding the RNG
-- may be a computationally intensive process, so if you need to call this function in a tight
-- loop, either use 'runRandomSeed' or explore alternative architectures.
runRandom :: MonadIO m => RandomC IO m a -> m a
runRandom (RandomC act) = do
  rand <- liftIO MWC.createSystemRandom
  runReader rand act
