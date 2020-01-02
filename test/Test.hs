{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Carrier.Lift
import Control.Carrier.Random.IO
import Control.Monad.IO.Class
import Data.Foldable
import Magic

main :: IO ()
main = do
  doit
  runM . runRandom $
    for_ [(1 :: Int)..4] $ \_ -> uniform @Double >>= liftIO . print
