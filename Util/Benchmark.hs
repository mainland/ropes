{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Util.Benchmark
-- Copyright   : (c) Geoffrey Mainland 2011-2013
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Util.Benchmark (
    runBenchmark,
    timeBenchmark,
    timeComp,
    secs,
    readSuffixed
  ) where

import Control.Applicative
import Control.Exception (evaluate)
import Control.Monad (replicateM)
import Data.Char (isAlpha)
import Text.Printf

import Util.GetCPUTime

bENCHTIME :: Double
bENCHTIME = 1.0

mIN_TRIALS :: Int
mIN_TRIALS = 100

runBenchmark :: Show b
             => String
             -> (a -> b)
             -> a
             -> IO ()
runBenchmark name f x = do
    (mean, min, max, sigma) <- timeBenchmark f x
    printf "%s mean %s min %s max %s var %s\n" name (secs mean) (secs min) (secs max) (secs sigma)

timeBenchmark :: Show b
              => (a -> b)
              -> a
              -> IO (Double, Double, Double, Double)
timeBenchmark f x = do
    t         <- timeComp bENCHTIME f x
    let ts    =  [t]
    let mean  =  sum ts / fromIntegral (length ts)
    let min   =  minimum ts
    let max   =  maximum ts
    let sigma =  sqrt (sum (map (\x -> x * x) ts) / fromIntegral (length ts) - mean*mean)
    return (mean, min, max, sigma)

timeComp ::  forall a b . Show b
         =>  Double
         ->  (a -> b)
         ->  a
         ->  IO Double
timeComp benchtime f x = do
    start   <- getCPUTime
    ntrials <- max mIN_TRIALS <$> repeatFromUntilAtLeast start bENCHTIME 0
    start   <- getCPUTime
    replicateM_ ntrials $ evalNf f x
    end     <- getCPUTime
    return $ (end - start) / fromIntegral ntrials
  where
    repeatFromUntilAtLeast :: Double -> Double -> Int -> IO Int
    repeatFromUntilAtLeast !start !delta !n = do
        evalNf f x
        now <- getCPUTime
        if now - start > delta
          then return n
          else repeatFromUntilAtLeast start delta (n+1)

evalNf :: (a -> b) -> a -> IO ()
{-# NOINLINE evalNf #-}
evalNf f x = evaluate (f x) >> return ()

replicateM_ :: Int -> IO a -> IO ()
replicateM_ 0  m = return ()
replicateM_ !n m = do  m
                       replicateM_ (n-1) m

secs :: Double -> String
secs k
    | k < 0      = '-' : secs (-k)
    | k >= 1     = k        `with` "s"
    | k >= 1e-3  = (k*1e3)  `with` "ms"
    | k >= 1e-6  = (k*1e6)  `with` "us"
    | k >= 1e-9  = (k*1e9)  `with` "ns"
    | k >= 1e-12 = (k*1e12) `with` "ps"
    | otherwise  = printf "%g s" k
     where with (t :: Double) (u :: String)
               | t >= 1e9  = printf "%.4g %s" t u
               | t >= 1e6  = printf "%.0f %s" t u
               | t >= 1e5  = printf "%.1f %s" t u
               | t >= 1e4  = printf "%.2f %s" t u
               | t >= 1e3  = printf "%.3f %s" t u
               | t >= 1e2  = printf "%.4f %s" t u
               | t >= 1e1  = printf "%.5f %s" t u
               | otherwise = printf "%.6f %s" t u

readSuffixed :: (Read a, Num a) => String -> a
readSuffixed s
    | isAlpha (last s) = read (init s) * multiplier (last s)
    | otherwise        = read s
  where
    multiplier 'k' = 1024
    multiplier 'K' = 1024
    multiplier 'm' = 1024*1024
    multiplier 'M' = 1024*1024
    multiplier 'g' = 1024*1024*1024
    multiplier 'G' = 1024*1024*1024
    multiplier c   = error $ "Unknown multiplier: " ++ show c
