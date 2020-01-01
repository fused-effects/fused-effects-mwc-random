{-# LANGUAGE DeriveFunctor, ExistentialQuantification, RankNTypes, StandaloneDeriving #-}

module Control.Effect.Random.Uniform
  ( Random (..)
  , random
  , randomR
  , Has
  ) where

import           Control.Algebra
import qualified System.Random.MWC as MWC

data Random m k
  = forall a . MWC.Variate a => Random (a -> m k)
  | forall a . MWC.Variate a => RandomR (a, a) (a -> m k)

deriving instance Functor m => Functor (Random m)

instance HFunctor Random where
  hmap f (Random k)    = Random (f . k)
  hmap f (RandomR r k) = RandomR r (f . k)

random :: (Has Random sig m, MWC.Variate a) => m a
random = send (Random pure)

randomR :: (Has Random sig m, MWC.Variate a) => (a, a) -> m a
randomR r = send (RandomR r pure)
