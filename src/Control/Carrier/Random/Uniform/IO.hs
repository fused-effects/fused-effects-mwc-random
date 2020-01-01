{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators,
             UndecidableInstances #-}

module Control.Carrier.Random.Uniform.IO where

import           Control.Algebra
import           Control.Carrier.Reader
import           Control.Effect.Random.Uniform
import           Control.Monad.IO.Class
import qualified System.Random.MWC as MWC

newtype RandomC m a = RandomC (ReaderC MWC.GenIO m a)
  deriving (Applicative, Functor, Monad, MonadIO)

instance (Algebra sig m, MonadIO m) => Algebra (Random :+: sig) (RandomC m) where
  alg (L act) = do
    gen <- RandomC ask
    case act of
      Random k    -> liftIO (MWC.uniform gen)    >>= k
      RandomR r k -> liftIO (MWC.uniformR r gen) >>= k
  alg (R other) = RandomC (alg (R (handleCoercible other)))
