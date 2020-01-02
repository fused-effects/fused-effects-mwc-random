{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, GADTs, RankNTypes, StandaloneDeriving #-}
-- | The @Random@ effect provides access to uniformly distributed random values of
-- user-specified types or from well-known numerical distributions.
--
-- This is the “fancy” syntax that hides most details of randomness
-- behind a nice API.
module Control.Effect.Random
  ( Random (..)
  -- * Uniform distributions
  , uniform
  , uniformR
  -- * Continuous distributions
  , normal
  , standard
  , exponential
  , truncatedExp
  , gamma
  , chiSquare
  , beta
  -- * Discrete distributions
  , categorical
  , logCategorical
  , geometric0
  , geometric1
  , bernoulli
  , dirichlet
  -- * Introspection
  , save
  , Distrib (..)
  -- * Re-exports
  , MWC.Variate
  , Has
  ) where

import           Control.Algebra
import           Data.Vector.Generic (Vector)
import qualified System.Random.MWC as MWC

-- | GADT representing the functions provided by mwc-random.
data Distrib a where
  Uniform        :: MWC.Variate a => Distrib a
  UniformR       :: MWC.Variate a => (a, a) -> Distrib a
  Normal         :: Double -> Double -> Distrib Double
  Standard       :: Distrib Double
  Exponential    :: Double -> Distrib Double
  TruncatedExp   :: Double -> (Double, Double) -> Distrib Double
  Gamma          :: Double -> Double -> Distrib Double
  ChiSquare      :: Int -> Distrib Double
  Beta           :: Double -> Double -> Distrib Double
  Categorical    :: Vector v Double => v Double -> Distrib Int
  LogCategorical :: Vector v Double => v Double -> Distrib Int
  Geometric0     :: Double -> Distrib Int
  Geometric1     :: Double -> Distrib Int
  Bernoulli      :: Double -> Distrib Bool
  Dirichlet      :: Traversable t => t Double -> Distrib (t Double)
  Save           :: Distrib MWC.Seed

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

-- | Generate an exponentially distributed random variate.
exponential :: Has Random sig m
            => Double -- ^ Scale parameter
            -> m Double
exponential s = send (Random (Exponential s) pure)

-- | Generate truncated exponentially distributed random variate.
truncatedExp :: Has Random sig m
             => Double            -- ^ Scale parameter
             -> (Double,Double)   -- ^ Range to which distribution is
                                  --   truncated. Values may be negative.
             -> m (Double)
truncatedExp s r = send (Random (TruncatedExp s r) pure)

-- | Random variate generator for gamma distribution.
gamma :: Has Random sig m
      => Double                 -- ^ Shape parameter
      -> Double                 -- ^ Scale parameter
      -> m Double
gamma s d = send (Random (Gamma s d) pure)

-- | Random variate generator for the chi square distribution.
chiSquare :: Has Random sig m
          => Int                -- ^ Number of degrees of freedom
          -> m Double
chiSquare d = send (Random (ChiSquare d) pure)

-- | Random variate generator for the geometric distribution,
-- computing the number of failures before success. Distribution's
-- support is [0..].
geometric0 :: Has Random sig m
           => Double            -- ^ /p/ success probability lies in (0,1]
           -> m Int
geometric0 p = send (Random (Geometric0 p) pure)

-- | Random variate generator for geometric distribution for number of
-- trials. Distribution's support is [1..] (i.e. just 'geometric0'
-- shifted by 1).
geometric1 :: Has Random sig m
           => Double            -- ^ /p/ success probability lies in (0,1]
           -> m Int
geometric1 p = send (Random (Geometric1 p) pure)

-- | Random variate generator for Beta distribution
beta :: Has Random sig m
     => Double            -- ^ alpha (>0)
     -> Double            -- ^ beta  (>0)
     -> m Double
beta a b = send (Random (Beta a b) pure)
{-# INLINE beta #-}

-- | Random variate generator for Dirichlet distribution
dirichlet :: (Has Random sig m, Traversable t)
          => t Double          -- ^ container of parameters
          -> m (t Double)
{-# INLINE dirichlet #-}
dirichlet t = send (Random (Dirichlet t) pure)

-- | Random variate generator for Bernoulli distribution
bernoulli :: Has Random sig m
          => Double            -- ^ Probability of success (returning True)
          -> m Bool
{-# INLINE bernoulli #-}
bernoulli p = send (Random (Bernoulli p) pure)

-- | Random variate generator for categorical distribution.
categorical :: (Has Random sig m, Vector v Double)
            => v Double          -- ^ List of weights [>0]
            -> m Int
{-# INLINE categorical #-}
categorical v = send (Random (Categorical v) pure)

-- | Random variate generator for categorical distribution where the
--   weights are in the log domain. It's implemented in terms of
--   'categorical'.
logCategorical :: (Has Random sig m, Vector v Double)
               => v Double          -- ^ List of logarithms of weights
               -> m Int
logCategorical v = send (Random (LogCategorical v) pure)

-- | Save the state of the random number generator for use with the
-- resumptive powers of 'runRandomSeeded'.
save :: Has Random sig m => m MWC.Seed
save = send (Random Save pure)
