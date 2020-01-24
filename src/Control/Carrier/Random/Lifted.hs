{-# LANGUAGE AllowAmbiguousTypes, FlexibleContexts, FlexibleInstances, GADTs, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, TypeOperators, UndecidableInstances #-}
-- | This carrier lifts the internals of its random number generation into
-- a 'LiftC' constraint, assuming the parameter to that 'LiftC' implements
-- 'PrimMonad'. In practice, this means that your effect stack must terminate
-- with @LiftC IO@ or @LiftC (ST s)@.
module Control.Carrier.Random.Lifted
  ( RandomC (..)
  , runRandomSystem
  , runRandomSeeded
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
import qualified System.Random.MWC.Distributions as Dist

newtype RandomC prim m a = RandomC (ReaderC (MWC.Gen (PrimState prim)) m a)
  deriving (Applicative, Functor, Monad, MonadFail, MonadIO)

instance (Algebra sig m, Member (Lift n) sig, PrimMonad n) => Algebra (Random :+: sig) (RandomC n m) where
  alg (L (Random dist k)) = do
    gen <- RandomC ask
    let act = case dist of
          Uniform           -> MWC.uniform
          UniformR r        -> MWC.uniformR r
          Normal m d        -> Dist.normal m d
          Standard          -> Dist.standard
          Exponential s     -> Dist.exponential s
          TruncatedExp s r  -> Dist.truncatedExp s r
          Gamma s h         -> Dist.gamma s h
          ChiSquare d       -> Dist.chiSquare d
          Beta a b          -> Dist.beta a b
          Categorical w     -> Dist.categorical w
          LogCategorical lw -> Dist.logCategorical lw
          Geometric0 p      -> Dist.geometric0 p
          Geometric1 p      -> Dist.geometric1 p
          Bernoulli p       -> Dist.bernoulli p
          Dirichlet t       -> Dist.dirichlet t
          Permutation n     -> Dist.uniformPermutation n
          Shuffle v         -> Dist.uniformShuffle v

    sendM @n (act gen) >>= k
  alg (L (Save k)) = do
    gen <- RandomC ask
    sendM @n (MWC.save gen) >>= k
  alg (R other) = RandomC (alg (R (handleCoercible other)))
  {-# INLINE alg #-}

-- | Run a computation, seeding its random values from the system random number generator.
--
-- This is the de facto standard way to use this carrier. Keep in mind that seeding the RNG
-- may be a computationally intensive process.
runRandomSystem :: MonadIO m => RandomC IO m a -> m a
runRandomSystem (RandomC act) = do
  rand <- liftIO MWC.createSystemRandom
  runReader rand act

-- | Run a computation, seeding its random values from an existing 'MWC.Seed'.
runRandomSeeded :: forall m n sig a . (Has (Lift n) sig m, PrimMonad n) => MWC.Seed -> RandomC n m a -> m a
runRandomSeeded s (RandomC act) = do
  rand <- sendM @n (MWC.restore s)
  runReader rand act
