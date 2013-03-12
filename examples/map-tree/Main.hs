{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

#define PHASE_STREAM [1]
#define PHASE_INNER  [0]

#define INLINE_STREAM INLINE PHASE_STREAM
#define INLINE_INNER  INLINE PHASE_INNER

import Control.DeepSeq (NFData(..))
import Control.Exception (evaluate)
import Control.Monad (liftM)
import Control.Monad.Par
import Data.Char (isAlpha)
import Debug.Trace (traceEvent)
import System.Environment (getArgs)

import qualified Data.Vector.Fusion.Stream as F
import Data.Vector.Fusion.Stream.Monadic (Step(..))
import qualified Data.Vector.Fusion.Stream.Monadic as MStream
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U

main :: IO ()
main = do
    args <- getArgs
    case args of
          [len, thresh] -> run (readSuffixed len) (readSuffixed thresh)
          _             -> usage
  where
    usage :: IO ()
    usage = putStrLn "Usage: test <length> <threshold>"

run :: Int -> Int -> IO ()
run n thresh = do
    let u :: U.Vector Double  = U.enumFromTo 1 (fromIntegral n)
    evaluate u
    _ <- evaluate $ runPar $ inc u
    return ()
  where
    inc :: U.Vector Double -> Par (U.Vector Double)
    inc u | len < thresh = return $ traceEvent "map" $ mapy (+1) u
          | otherwise    = do  let (v, w) = traceEvent "split" $ U.splitAt (len `div` 2) u
                               iv' <- spawn $ inc v
                               iw' <- spawn $ inc w
                               v' <- get iv'
                               w' <- get iw'
                               return $ traceEvent "append" $ v' U.++ w'
      where
        len :: Int
        len = U.length u

mapy :: (G.Vector v a, G.Vector v b) => (a -> b) -> v a -> v b
{-# INLINE mapy #-}
mapy f = G.unstream . F.inplace (mapyM (return . f)) . G.stream

gcCheck :: Monad m => Int -> m a -> m (Int, a)
{-# INLINE gcCheck #-}
gcCheck i x | i > gC_THRESH = do y <- x
                                 return $ traceEvent "GC Check" (0, Nothing `seq` y)
            | otherwise     = do y <- x
                                 return (i+1, y)

gC_THRESH :: Int
gC_THRESH = 128

-- | Map a monadic function over a 'Stream'
mapyM :: forall m a b . Monad m => (a -> m b) -> MStream.Stream m a -> MStream.Stream m b
{-# INLINE_STREAM mapyM #-}
mapyM f (MStream.Stream step (s :: s) n) = MStream.Stream step' (s, 0) n
  where
    {-# INLINE_INNER step' #-}
    step' :: (s, Int) -> m (Step (s, Int) b)
    step' (s, !i) = do
        r <- step s
        case r of
          Yield x s' -> do  y <- f x
                            return  $ Yield y (s', i+1)
          Skip    s' -> return $ Skip (s', i+1)
          Done       -> return Done

readSuffixed :: (Read a, Num a) => String -> a
{-# NOINLINE readSuffixed #-}
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
