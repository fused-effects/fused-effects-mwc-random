{-# LANGUAGE OverloadedLists, ScopedTypeVariables #-}

module Magic (doit) where

import           Control.Carrier.Lift
import           Control.Carrier.Random.Instances ()
import           Control.Carrier.Reader
import           Control.Monad.IO.Class
import qualified Data.Vector as Vector
import qualified System.Random.MWC as MWC

doit :: IO ()
doit = runM $ runReader 'a' $ do
  x <- MWC.initialize (Vector.fromList [1, 2,3])
  (y :: Double) <- MWC.uniform x
  liftIO $ print y
