{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Carrier.Lift
import Control.Carrier.Random.Lifted
import Control.Monad.IO.Class
import Data.Foldable
import Magic

main :: IO ()
main = do
  doit
  runM . runRandomSystem $
    for_ [(1 :: Int)..4] $ \_ -> uniform @Double >>= liftIO . print
