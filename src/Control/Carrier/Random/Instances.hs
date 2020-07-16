{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module exists to export instances of @PrimMonad@ for the @fused-effects@
-- ecosystem. These instances are trivially derivable, since all carrier types are
-- monad transformers, but are boring to have to write by hand.
--
-- This module is most useful when porting monad transformer stacks that already use
-- @mwc-random@ to the fused-effects ecosystem, or for when you have very
-- fine-grained need to control the behavior or state of a random number generator.
-- For new code, the 'Control.Effect.Random' effect will provide a more genial interface.
module Control.Carrier.Random.Instances where

import Control.Carrier.Choose.Church (ChooseC)
import Control.Carrier.Cull.Church (CullC)
import Control.Carrier.Cut.Church (CutC)
import Control.Carrier.Empty.Maybe (EmptyC)
import Control.Carrier.Error.Either (ErrorC)
import Control.Carrier.Fail.Either (FailC)
import Control.Carrier.Fresh.Strict (FreshC)
import Control.Carrier.Lift (LiftC)
import Control.Carrier.Reader (ReaderC)
import qualified Control.Carrier.State.Lazy as Lazy
import qualified Control.Carrier.State.Strict as Strict
import Control.Carrier.Throw.Either (ThrowC)
import Control.Carrier.Writer.Strict (WriterC)
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.Trans.Class
import Data.Traversable
import Language.Haskell.TH.Lib

instance PrimMonad m => PrimMonad (LiftC m) where
  type PrimState (LiftC m) = PrimState m
  primitive = lift . primitive
  {-# INLINE primitive #-}

fmap
  join
  ( for
      [''ChooseC, ''CullC, ''CutC, ''EmptyC, ''FailC, ''FreshC]
      ( \c ->
          [d|
            instance PrimMonad m => PrimMonad ($(conT c) m) where
              type PrimState ($(conT c) m) = PrimState m
              primitive = lift . primitive
              {-# INLINE primitive #-}
            |]
      )
  )

fmap
  join
  ( for
      [''ReaderC, ''ErrorC, ''Strict.StateC, ''Lazy.StateC, ''ThrowC, ''WriterC]
      ( \c ->
          [d|
            instance PrimMonad m => PrimMonad ($(conT c) r m) where
              type PrimState ($(conT c) r m) = PrimState m
              primitive = lift . primitive
              {-# INLINE primitive #-}
            |]
      )
  )
