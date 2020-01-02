{-# LANGUAGE DeriveFunctor, ExistentialQuantification, GADTs, RankNTypes, StandaloneDeriving #-}
-- | The @Random@ effect provides access to uniformly distributed random values of
-- user-specified types or from well-known numerical distributions.
--
-- This is the “fancy” syntax that hides most details of randomness
-- behind a nice API.
module Control.Effect.Random
  ( Random (..)
  -- * Uniform generation
  , uniform
  , uniformR
  -- * Continuous distributions
  , normal
  , standard
  -- * Internals
  , Distrib (..)
  -- * Re-exports
  , MWC.Variate
  , Has
  ) where

import           Control.Algebra
import qualified System.Random.MWC as MWC

-- | GADT representing the functions provided by mwc-random.
data Distrib a where
  Uniform  :: MWC.Variate a => Distrib a
  UniformR :: MWC.Variate a => (a, a) -> Distrib a
  Normal   :: Double -> Double -> Distrib Double
  Standard :: Distrib Double

data Random m k
  = forall a . Random (Distrib a) (a -> m k)

deriving instance Functor m => Functor (Random m)

instance HFunctor Random where
  hmap f (Random d k) = Random d (f . k)


-- | Generate a single, uniformly-distributed random value.
-- The type to be generated must implement the 'Variate' typeclass.
uniform :: (MWC.Variate a, Has Random sig m) => m a
uniform = send (Random Uniform pure)
{-# INLINE uniform #-}

-- | Generate a single, uniformly-distributed random value.
-- The type to be generated must implement the 'Variate' typeclass.
uniformR :: (MWC.Variate a, Has Random sig m) => (a, a) -> m a
uniformR r = send (Random (UniformR r) pure)
{-# INLINE uniformR #-}

-- | Generate a normally distributed random variate with given mean and standard deviation.
normal :: Has Random sig m
       => Double -- ^ Mean
       -> Double -- ^ Standard deviation
       -> m Double
normal m d = send (Random (Normal m d) pure)

-- | Generate a normally distributed random variate with zero mean and unit variance.
standard :: Has Random sig m => m Double
standard = send (Random Standard pure)
