{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | The @Random@ effect provides access to uniformly distributed random values of
-- user-specified types or from well-known numerical distributions.
--
-- This is the “fancy” syntax that hides most details of randomness
-- behind a nice API.
module Control.Effect.Random
  ( Random (..),

    -- * Uniform distributions
    uniform,
    uniformR,

    -- * Continuous distributions
    normal,
    standard,
    exponential,
    truncatedExp,
    gamma,
    chiSquare,
    beta,

    -- * Discrete distributions
    categorical,
    logCategorical,
    geometric0,
    geometric1,
    bernoulli,
    dirichlet,

    -- * Permutations
    uniformPermutation,
    uniformShuffle,

    -- * Introspection
    save,
    Distrib (..),

    -- * Re-exports
    MWC.Variate,
    Has,
  )
where

import Control.Algebra
import Data.Kind
import Data.Vector.Generic (Vector)
import qualified System.Random.MWC as MWC

-- | GADT representing the functions provided by mwc-random.
data Distrib a where
  Uniform :: MWC.Variate a => Distrib a
  UniformR :: MWC.Variate a => (a, a) -> Distrib a
  Normal :: Double -> Double -> Distrib Double
  Standard :: Distrib Double
  Exponential :: Double -> Distrib Double
  TruncatedExp :: Double -> (Double, Double) -> Distrib Double
  Gamma :: Double -> Double -> Distrib Double
  ChiSquare :: Int -> Distrib Double
  Beta :: Double -> Double -> Distrib Double
  Categorical :: Vector v Double => v Double -> Distrib Int
  LogCategorical :: Vector v Double => v Double -> Distrib Int
  Geometric0 :: Double -> Distrib Int
  Geometric1 :: Double -> Distrib Int
  Bernoulli :: Double -> Distrib Bool
  Dirichlet :: Traversable t => t Double -> Distrib (t Double)
  Permutation :: Vector v Int => Int -> Distrib (v Int)
  Shuffle :: Vector v a => v a -> Distrib (v a)

data Random (m :: Type -> Type) k where
  Random :: Distrib a -> Random m a
  Save :: MWC.Seed -> Random m ()

deriving instance Functor m => Functor (Random m)

-- | Generate a single uniformly distributed random variate.  The
-- range of values produced varies by type:
--
-- * For fixed-width integral types, the type's entire range is
--   used.
--
-- * For floating point numbers, the range (0,1] is used. Zero is
--   explicitly excluded, to allow variates to be used in
--   statistical calculations that require non-zero values
--   (e.g. uses of the 'log' function).
--
-- To generate a 'Float' variate with a range of [0,1), subtract
-- 2**(-33).  To do the same with 'Double' variates, subtract
-- 2**(-53).
uniform :: (MWC.Variate a, Has Random sig m) => m a
uniform = send (Random Uniform pure)
{-# INLINE uniform #-}

-- | Generate single uniformly distributed random variable in a
-- given range.
--
-- * For integral types inclusive range is used.
--
-- * For floating point numbers range (a,b] is used if one ignores
--   rounding errors.
uniformR :: (MWC.Variate a, Has Random sig m) => (a, a) -> m a
uniformR r = send (Random (UniformR r) pure)
{-# INLINE uniformR #-}

-- | Generate a normally distributed random variate with given mean and standard deviation.
normal ::
  Has Random sig m =>
  -- | Mean
  Double ->
  -- | Standard deviation
  Double ->
  m Double
normal m d = send (Random (Normal m d) pure)

-- | Generate a normally distributed random variate with zero mean and unit variance.
standard :: Has Random sig m => m Double
standard = send (Random Standard pure)

-- | Generate an exponentially distributed random variate.
exponential ::
  Has Random sig m =>
  -- | Scale parameter
  Double ->
  m Double
exponential s = send (Random (Exponential s) pure)

-- | Generate truncated exponentially distributed random variate.
truncatedExp ::
  Has Random sig m =>
  -- | Scale parameter
  Double ->
  -- | Range to which distribution is
  --   truncated. Values may be negative.
  (Double, Double) ->
  m (Double)
truncatedExp s r = send (Random (TruncatedExp s r) pure)

-- | Random variate generator for gamma distribution.
gamma ::
  Has Random sig m =>
  -- | Shape parameter
  Double ->
  -- | Scale parameter
  Double ->
  m Double
gamma s d = send (Random (Gamma s d) pure)

-- | Random variate generator for the chi square distribution.
chiSquare ::
  Has Random sig m =>
  -- | Number of degrees of freedom
  Int ->
  m Double
chiSquare d = send (Random (ChiSquare d) pure)

-- | Random variate generator for the geometric distribution,
-- computing the number of failures before success. Distribution's
-- support is [0..].
geometric0 ::
  Has Random sig m =>
  -- | /p/ success probability lies in (0,1]
  Double ->
  m Int
geometric0 p = send (Random (Geometric0 p) pure)

-- | Random variate generator for geometric distribution for number of
-- trials. Distribution's support is [1..] (i.e. just 'geometric0'
-- shifted by 1).
geometric1 ::
  Has Random sig m =>
  -- | /p/ success probability lies in (0,1]
  Double ->
  m Int
geometric1 p = send (Random (Geometric1 p) pure)

-- | Random variate generator for Beta distribution
beta ::
  Has Random sig m =>
  -- | alpha (>0)
  Double ->
  -- | beta  (>0)
  Double ->
  m Double
beta a b = send (Random (Beta a b) pure)
{-# INLINE beta #-}

-- | Random variate generator for Dirichlet distribution
dirichlet ::
  (Has Random sig m, Traversable t) =>
  -- | container of parameters
  t Double ->
  m (t Double)
{-# INLINE dirichlet #-}
dirichlet t = send (Random (Dirichlet t) pure)

-- | Random variate generator for Bernoulli distribution
bernoulli ::
  Has Random sig m =>
  -- | Probability of success (returning True)
  Double ->
  m Bool
{-# INLINE bernoulli #-}
bernoulli p = send (Random (Bernoulli p) pure)

-- | Random variate generator for categorical distribution.
categorical ::
  (Has Random sig m, Vector v Double) =>
  -- | List of weights [>0]
  v Double ->
  m Int
{-# INLINE categorical #-}
categorical v = send (Random (Categorical v) pure)

-- | Random variate generator for categorical distribution where the
--   weights are in the log domain. It's implemented in terms of
--   'categorical'.
logCategorical ::
  (Has Random sig m, Vector v Double) =>
  -- | List of logarithms of weights
  v Double ->
  m Int
logCategorical v = send (Random (LogCategorical v) pure)

-- | Save the state of the random number generator to be used by subsequent
-- carrier invocations.
save :: Has Random sig m => m MWC.Seed
save = send (Save pure)

-- | Random variate generator for uniformly distributed permutations. It returns random permutation of vector [0 .. n-1]. This is the Fisher-Yates shuffle.
uniformPermutation ::
  (Has Random sig m, Vector v Int) =>
  Int ->
  m (v Int)
uniformPermutation n = send (Random (Permutation n) pure)

-- | Random variate generator for a uniformly distributed shuffle (all
--   shuffles are equiprobable) of a vector. It uses Fisher-Yates
--   shuffle algorithm.
--
-- Implementation details prevent a native implementation of the 'MWC.uniformShuffleM'
-- function. Use the native API if this is required.
uniformShuffle ::
  (Has Random sig m, Vector v a) =>
  v a ->
  m (v a)
uniformShuffle n = send (Random (Shuffle n) pure)
