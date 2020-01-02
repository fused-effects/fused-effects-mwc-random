{-# LANGUAGE PackageImports #-}

module Main (main) where

import Control.Monad
import Gauge

import qualified "fused-effects-mwc-random" Control.Carrier.Random.Lifted as MWC
import qualified "fused-effects-random" Control.Effect.Random as System

mwc :: Int -> IO [Double]
mwc n = MWC.runRandomSystem (replicateM n MWC.uniform)

system :: Int -> IO [Double]
system n = System.evalRandomIO (replicateM n System.getRandom)

main :: IO ()
main = defaultMain [ bench "System.Random (generate 1 double)" (nfAppIO system 1)
                   , bench "MWC.Random (generate 1 double)" (nfAppIO mwc 1)
                   , bench "System.Random (generate 10 double)" (nfAppIO system 10)
                   , bench "MWC.Random (generate 10 double)" (nfAppIO mwc 10)
                   , bench "System.Random (generate 100 double)" (nfAppIO system 100)
                   , bench "MWC.Random (generate 100 double)" (nfAppIO mwc 100)
                   ]
