{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}

#define PHASE_STREAM [1]
#define PHASE_INNER  [0]

#define INLINE_STREAM INLINE PHASE_STREAM
#define INLINE_INNER  INLINE PHASE_INNER

#define SPECIALIZE 1

module Data.Rope.Seq (
  Seq,
  fromVector, toVector,
  fromList, toList,
  empty, singleton, last, length, append, split, splitAt,

  Progress(..),

  map, mapUntil,
 ) where

import Prelude hiding (last, length, map, mapM, splitAt)

import Control.DeepSeq
import Control.Monad.Par
import Control.Monad.ST

import qualified Data.Vector as V
import qualified Data.Vector.Fusion.Stream as S
import Data.Vector.Fusion.Stream.Monadic (Step(..), Stream(..))
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U

data Progress a b = More a
                  | Finished b
 deriving (Show)

-- | The 'Seq' data type ensures that its elements are in WHNF
data Seq a where
    Seq        :: V.Vector a -> Seq a
#if SPECIALIZE
    Seq_Double :: U.Vector Double -> Seq Double
#endif /* SPECIALIZE */

instance Show a => Show (Seq a) where
    showsPrec p (Seq v) =
        showParen (p > 10) $
        showString "Seq " . showsPrec 11 v

#if SPECIALIZE
    showsPrec p (Seq_Double v) =
        showParen (p > 10) $
        showString "Seq_Double " . showsPrec 11 v
#endif /* SPECIALIZE */

instance NFData (Seq a) where
    rnf (Seq v)        = v `seq` ()
#if SPECIALIZE
    rnf (Seq_Double v) = v `seq` ()
#endif /* SPECIALIZE */

fromVector :: G.Vector v a => v a -> Seq a
{-# INLINE fromVector #-}
fromVector = Seq . forceConvert

#if SPECIALIZE
{-# RULES

"fromVector [Double]" fromVector = fromVectorDouble

 #-}

fromVectorDouble :: U.Vector Double -> Seq Double
{-# INLINE fromVectorDouble #-}
fromVectorDouble v = Seq_Double v
#endif /* SPECIALIZE */

toVector :: G.Vector v a => Seq a -> v a
{-# INLINE toVector #-}
toVector (Seq v)        = G.convert v
#if SPECIALIZE
toVector (Seq_Double v) = G.convert v
#endif /* SPECIALIZE */

fromList :: [a] -> Seq a
{-# INLINE fromList #-}
fromList xs = Seq (G.fromList xs)

toList :: Seq a -> [a]
{-# INLINE toList #-}
toList (Seq xs)        = G.toList xs
toList (Seq_Double xs) = G.toList xs

empty :: Seq a
{-# INLINE empty #-}
empty = Seq (G.empty)

singleton :: a -> Seq a
singleton x = Seq (G.singleton x)

last :: Seq a -> a
{-# INLINE last #-}
last (Seq v)        = G.last v
#if SPECIALIZE
last (Seq_Double v) = G.last v
#endif /* SPECIALIZE */

length :: Seq a -> Int
{-# INLINE length #-}
length (Seq v)        = G.length v
#if SPECIALIZE
length (Seq_Double v) = G.length v
#endif /* SPECIALIZE */

append :: Seq a -> Seq a -> Seq a
{-# INLINE append #-}
append (Seq v)        (Seq w)        = Seq (v G.++ w)
#if SPECIALIZE
append (Seq_Double v) (Seq_Double w) = Seq_Double (v G.++ w)
append (Seq v)        (Seq_Double w) = Seq (v G.++ G.convert w)
append (Seq_Double v) (Seq w)        = Seq (G.convert v G.++ w)
#endif /* SPECIALIZE */

split :: Seq a -> (Seq a, Seq a)
{-# INLINE split #-}
split (Seq w)        = let (u, v) = G.splitAt (G.length w `div` 2) w in (Seq u, Seq v)
#if SPECIALIZE
split (Seq_Double w) = let (u, v) = G.splitAt (G.length w `div` 2) w in (Seq_Double u, Seq_Double v)
#endif /* SPECIALIZE */

splitAt :: Int -> Seq a -> (Seq a, Seq a)
{-# INLINE splitAt #-}
splitAt n (Seq w)        = let (u, v) = G.splitAt n w in (Seq u, Seq v)
#if SPECIALIZE
splitAt n (Seq_Double w) = let (u, v) = G.splitAt n w in (Seq_Double u, Seq_Double v)
#endif /* SPECIALIZE */

-- | Map a function over a 'Seq'
map :: (a -> b) -> Seq a -> Seq b
{-# INLINE map #-}
map f (Seq v)        = (Seq . G.unstream . S.inplace (mapM (return . f)) . G.stream) v
#if SPECIALIZE
map f (Seq_Double v) = (Seq . G.unstream . S.inplace (mapM (return . f)) . G.stream) v
#endif /* SPECIALIZE */

-- | Map a monadic function over a 'Seq'
mapM :: forall m a b . Monad m => (a -> m b) -> Stream m a -> Stream m b
{-# INLINE_STREAM mapM #-}
mapM f (Stream step (s0 :: s) n) = Stream step' s0 n
  where
    {-# INLINE_INNER step' #-}
    step' :: s -> m (Step s b)
    step' s = do
        r <- step s
        case r of
          Yield x s' -> do  y <- f x
                            y `seq` return $ Yield y s'
          Skip    s' -> return $ Skip s'
          Done       -> return Done

#if SPECIALIZE
{-# RULES

"map [Double]" map = mapDouble

 #-}

mapDouble :: (Double -> Double) -> Seq Double -> Seq Double
mapDouble f (Seq v)        = Seq_Double (U.map f (U.convert v))
mapDouble f (Seq_Double v) = Seq_Double (U.map f v)
#endif /* SPECIALIZE */

mapUntilV  ::  forall v1 v2 a b . (G.Vector v1 a, G.Vector v2 b)
           =>  Par Bool
           ->  (a -> b)
           ->  v1 a
           ->  Progress (v2 b, v1 a) (v2 b)
{-# INLINE mapUntilV #-}
mapUntilV cond f v1 = runST $ do
    mv2 <- GM.new len
    go mv2 0
  where
    go  ::  G.Mutable v2 s b
        ->  Int
        ->  ST s (Progress (v2 b, v1 a) (v2 b))
    go mv2 !i | i >= len = do
      v2 <- G.freeze mv2
      return $ Finished v2

    go mv2 !i = do
      if runPar cond
        then do  v2 <- G.freeze mv2
                 return $ More (G.take i v2, G.drop i v1)
        else do  let x = f (v1 G.! i)
                 x `seq` GM.write mv2 i x
                 go mv2 (i+1)

    len :: Int
    len = G.length v1

mapUntil  ::  forall a b . Par Bool
          ->  (a -> b)
          ->  Seq a
          ->  Progress (Seq b, Seq a) (Seq b)
{-# INLINE mapUntil #-}
mapUntil cond f (Seq u) =
    case mapUntilV cond f u of
      More (v, w)  -> More (Seq v, Seq w)
      Finished v   -> Finished (Seq v)

mapUntil cond f (Seq_Double u) =
    case mapUntilV cond f u of
      More (v, w)  -> More (Seq v, Seq_Double w)
      Finished v   -> Finished (Seq v)

#if SPECIALIZE
{-# RULES

"mapUntil [Double]" mapUntil = mapUntilDouble

 #-}

mapUntilDouble  ::  Par Bool
                ->  (Double -> Double)
                ->  Seq Double
                ->  Progress (Seq Double, Seq Double) (Seq Double)
{-# INLINE mapUntilDouble #-}
mapUntilDouble cond f (Seq u) =
    case mapUntilV cond f u of
      More (v, w)  -> More (Seq_Double v, Seq w)
      Finished v   -> Finished (Seq_Double v)

mapUntilDouble cond f (Seq_Double u) =
    case mapUntilV cond f u of
      More (v, w)  -> More (Seq_Double v, Seq_Double w)
      Finished v   -> Finished (Seq_Double v)
#endif /* SPECIALIZE */

forceConvert :: (G.Vector v a, G.Vector w a) => v a -> w a
{-# INLINE forceConvert #-}
forceConvert = G.unstream . forceStream . G.stream

forceStream :: forall m a . Monad m => Stream m a -> Stream m a
{-# INLINE_STREAM forceStream #-}
forceStream (Stream step (s0 :: s) n) = Stream step' s0 n
  where
    {-# INLINE_INNER step' #-}
    step' :: s -> m (Step s a)
    step' s = do
        r <- step s
        case r of
          Yield x s' -> x `seq` return $ Yield x s'
          Skip    s' -> return $ Skip s'
          Done       -> return Done
