{-# LANGUAGE DeriveFunctor, ExistentialQuantification, RankNTypes, StandaloneDeriving #-}
-- | The @Random@ effect provides access to uniformly distributed random values of
-- user-specified types or from well-known numerical distributions.
--
-- This is the “fancy” syntax that hides most details of randomness
-- behind a nice API.
module Control.Effect.Random
  ( Random (..)
  , uniform
  -- * Re-exports
  , MWC.Variate
  , Has
  ) where

import           Control.Algebra
import qualified System.Random.MWC as MWC

data Random m k
  = forall a . WithGen (MWC.GenIO -> IO a) (a -> m k)

deriving instance Functor m => Functor (Random m)

instance HFunctor Random where
  hmap f (WithGen fn k) = WithGen fn (f . k)

-- | Generate a single, uniformly-distributed random value.
-- The type to be generated must implement the 'Variate' typeclass

uniform :: (MWC.Variate a, Has Random sig m) => m a
uniform = send (WithGen MWC.uniform pure)
{-# INLINE uniform #-}
