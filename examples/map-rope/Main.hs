{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.DeepSeq (NFData(..))
import Control.Exception (evaluate)
import Control.Monad.Par
import Debug.Trace (traceEvent)
import System.Environment (getArgs)
import Text.Printf

import qualified Data.Vector.Unboxed as U

import Data.Rope (Rope(..))
import qualified Data.Rope as R
import qualified Data.Rope.Seq as S

import Util.Benchmark

main :: IO ()
main = do
    args <- getArgs
    case args of
          [len, thresh] -> do  R.set_mAX_LEAF_SIZE (readSuffixed thresh)
                               run (readSuffixed len)
          _             -> usage
  where
    usage :: IO ()
    usage = putStrLn "Usage: test <length> <threshold>"

    run :: Int -> IO ()
    run n = do
        let u :: U.Vector Double = U.enumFromTo 1 (fromIntegral n)
        evaluate u
        runOne "mapLTS" n (\u -> runPar $ R.mapLTS (+1) (Leaf (S.fromVector u))) u

    runOne  ::  Show b
            =>  String
            ->  Int
            ->  (a -> b)
            ->  a
            ->  IO ()
    runOne func n f x = do
        (mean, max, min, sigma) <- timeBenchmark f x
        printf "%s,%d,%02f,%02f,%02f,%02f\n" func n mean min max sigma
