{-# LANGUAGE ScopedTypeVariables #-}

module Data.Rope where

import Control.DeepSeq
import Control.Monad.Par
import Control.Monad     (when)
import Data.IORef        (IORef, newIORef, readIORef, writeIORef)
import Debug.Trace       (traceEvent)
import System.IO.Unsafe  (unsafePerformIO)

import Prelude hiding (length, splitAt)
import qualified Prelude as P

import qualified Data.Vector.Generic as G

import qualified Data.Rope.Seq as Seq
import Data.Rope.Seq (Progress(..), Seq)

mAX_LEAF_SIZE_ref :: IORef Int
{-# NOINLINE mAX_LEAF_SIZE_ref #-}
mAX_LEAF_SIZE_ref = unsafePerformIO (newIORef 512)

set_mAX_LEAF_SIZE :: Int -> IO ()
set_mAX_LEAF_SIZE n = writeIORef mAX_LEAF_SIZE_ref n

mAX_LEAF_SIZE :: Int
mAX_LEAF_SIZE = unsafePerformIO $ readIORef mAX_LEAF_SIZE_ref

data Rope a = Leaf !(Seq a)
            | Cat  {-# UNPACK #-} !Int  -- ^ Length
                   {-# UNPACK #-} !Int  -- ^ Depth
                   !(Rope a)            -- ^ Left branch
                   !(Rope a)            -- ^ Right branch
 deriving (Show)

instance NFData (Rope a) where
    rnf (Leaf xs)     = rnf xs
    rnf (Cat l d v w) = l `seq` d `seq` rnf v `seq` rnf w

fromVector :: G.Vector v a => v a -> Rope a
{-# INLINE fromVector #-}
fromVector = Leaf . Seq.fromVector

toVector :: G.Vector v a => Rope a -> v a
{-# INLINE toVector #-}
toVector (Leaf xs)       = Seq.toVector xs
toVector (Cat _ _ xs ys) = toVector xs G.++ toVector ys

fromList :: [a] -> Rope a
fromList xs = go xs (P.length xs)
  where
    go xs len | len < mAX_LEAF_SIZE = leaf (Seq.fromList xs)
              | otherwise           = ncat  (go (take len' xs) len')
                                            (go (drop len' xs) (len - len'))
      where
        len' = len `div` 2

toList :: Rope a -> [a]
toList (Leaf xs)         = Seq.toList xs
toList (Cat _ _ rp1 rp2) = toList rp1 ++ toList rp2

leaves :: Rope a -> [Seq a]
leaves (Leaf xs)         = [xs]
leaves (Cat _ _ rp1 rp2) = leaves rp1 ++ leaves rp2

toSeq :: Rope a -> Seq a
toSeq rp = foldr1 Seq.append (leaves rp)

length :: Rope a -> Int
{-# INLINE length #-}
length (Leaf xs)       = Seq.length xs
length (Cat len _ _ _) = len

isEmpty :: Rope a -> Bool
{-# INLINE isEmpty #-}
isEmpty rp = length rp == 0

depth :: Rope a -> Int
{-# INLINE depth #-}
depth (Leaf _)      = 0
depth (Cat _ d _ _) = d

empty :: Rope a
{-# INLINE empty #-}
empty = Leaf Seq.empty

singleton :: a -> Rope a
{-# INLINE singleton #-}
singleton x = Leaf (Seq.singleton x)

leaf :: Seq a -> Rope a
{-# INLINE leaf #-}
leaf xs | Seq.length xs > mAX_LEAF_SIZE = error "Leaf too large"
        | otherwise                     = Leaf xs

-- | Check that a rope's length and depth fields are correct
check :: Rope a -> Bool
check (Leaf _) = True
check rp@(Cat len dep rp1 rp2) = len == length' rp &&
                                 dep == depth' rp &&
                                 check rp1 &&
                                 check rp2
  where
    length' :: Rope a -> Int
    length' (Leaf xs)         = Seq.length xs
    length' (Cat _ _ rp1 rp2) = length' rp1 + length' rp2

    depth' :: Rope a -> Int
    depth' (Leaf _)          = 0
    depth' (Cat _ _ rp1 rp2) = max (depth' rp1) (depth' rp2) + 1

-- | Test whether or not a rope is balanced
isBalanced :: Rope a -> Bool
isBalanced (Leaf _) = True
isBalanced rp       = depth rp <= 2 * ceilingLg (length rp)
  where
    ceilingLg :: Int -> Int
    ceilingLg x = ceiling (log (fromIntegral x) / log 2)

-- | Rebalance a rope
balance :: Rope a -> Rope a
balance rp
    | isBalanced rp                   = rp
    | len <= mAX_LEAF_SIZE || len < 2 = leaf (toSeq rp)
    | otherwise                       = balance rp1 `ncat` balance rp2
  where
    (rp1, rp2) = splitAt rp (len `div` 2 - 1)

    len :: Int
    len = length rp

-- | Non-coalescing rope concatenation
ncat :: Rope a -> Rope a -> Rope a
{-# INLINE ncat #-}
ncat rp1 rp2 = Cat len dep rp1 rp2
  where
    len, dep :: Int
    len = length rp1 + length rp2
    dep = max (depth rp1) (depth rp2) + 1

-- | Coalescing rope concatenation
ccat :: Rope a -> Rope a -> Rope a
{-# INLINE ccat #-}
ccat rp1 rp2 =
    case ncat rp1 rp2 of
      rp | length rp <= mAX_LEAF_SIZE -> Leaf (toSeq rp)
         | otherwise                  -> rp

-- | Rope concatenation, ensuring a balanced result
cat :: Rope a -> Rope a -> Rope a
{-# INLINE cat #-}
cat rp1 rp2 = balance (rp1 `ncat` rp2)

-- | Split a rope into two (nearly equally sized) halves
split :: Rope a -> (Rope a, Rope a)
split (Leaf xs)         = let (xs1, xs2) = Seq.split xs
                          in (Leaf xs1, Leaf xs2)
split (Cat _ _ rp1 rp2) = (rp1, rp2)

-- | Split a rope at index 'i'
splitAt :: Rope a -> Int -> (Rope a, Rope a)
splitAt (Leaf xs) i =
    (Leaf xs1, Leaf xs2)
  where
    (xs1, xs2) = Seq.splitAt i xs

splitAt (Cat _ _ l r) i
    | i == len_l - 1 = (l, r)
    | i <  len_l     = let (l1, l2) = splitAt l i
                       in
                         (l1, l2 `ncat` r)
    | otherwise      = let (r1, r2) = splitAt r (i - len_l)
                       in
                         (l `ncat` r1, r2)
  where
    len_l :: Int
    len_l = length l

data MapCtx a b = MCTop
                | MCLeft  (MapCtx a b) (Rope a)
                | MCRight (Rope b)     (MapCtx a b)
 deriving (Show)

data MapCur a b = MapCur (Seq b) (Seq a) (MapCtx a b)
 deriving (Show)

(+++) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(a1,b1) +++ (a2,b2) = (a1+a2, b1+b2)

plug :: Rope a -> MapCtx a a -> Rope a
plug rp MCTop = rp
plug rp (MCLeft  ctx' rrp)  = plug (rp `ncat` rrp) ctx'
plug rp (MCRight lrp  ctx') = plug (lrp `ncat` rp) ctx'

root :: MapCur a a -> Rope a
root (MapCur xs ys ctx) = plug (Leaf (xs `Seq.append` ys)) ctx

ctxLength :: MapCtx a b -> (Int, Int)
ctxLength MCTop = (0, 0)
ctxLength (MCLeft  ctx' rrp)  = ctxLength ctx' +++ (0, length rrp)
ctxLength (MCRight lrp  ctx') = ctxLength ctx' +++ (length lrp, 0)

ctxDepth :: MapCtx a b -> (Int, Int)
ctxDepth  MCTop = (0, 0)
ctxDepth (MCLeft  ctx' rrp)  = ctxDepth ctx' +++ (0, depth rrp)
ctxDepth (MCRight lrp  ctx') = ctxDepth ctx' +++ (depth lrp, 0)

curLength :: MapCur a b -> (Int, Int)
curLength (MapCur pseq useq ctx) =
    ctxLength ctx +++ (Seq.length pseq, Seq.length useq)

curDepth :: MapCur a b -> (Int, Int)
curDepth (MapCur pseq useq ctx) =
    ctxDepth ctx +++ (1, 1)

lengthRight :: MapCur a b -> Int
lengthRight cur = snd (curLength cur)

depthRight :: MapCur a b -> Int
depthRight cur = snd (curDepth cur)

leftmost :: Rope a -> MapCtx a b -> (Seq a, MapCtx a b)
leftmost (Leaf xs)         ctx = (xs, ctx)
leftmost (Cat _ _ lrp rrp) ctx = leftmost lrp (MCLeft ctx rrp)

next :: Rope b -> MapCtx a b -> Progress (Seq a, MapCtx a b) (Rope b)
next rp MCTop              = Finished rp
next rp (MCLeft ctx' rrp)  = let (xs, ctx'') = leftmost rrp (MCRight rp ctx')
                             in
                               More (xs, ctx'')
next rp (MCRight lrp ctx') = next (lrp `ncat` rp) ctx'

splitCur :: MapCur a b -> (Rope a, Rope a, MapCurReb b)
splitCur cur =
  let n = snd (curLength cur) `div` 2
      UnzipMapCtx ls rs ds = curUnzip cur
      (rps1, mrp, k, rps2) = divideRopes rs n
      UnzipMapCtx mls mrs mds = curUnzip (splitAtAsCur mrp k)
      n1 = P.length rps1
      n2 = P.length mrs
      (rp1, l1) = encodeRopes (rps1 ++ mls)
      (rp2, l2) = encodeRopes (mrs ++ rps2)
  in
    (rp1, rp2, MapCurReb ls ds mds n1 n2 l1 l2)

join :: Rope a -> Rope a -> MapCurReb b -> MapCur a b
join rp1 rp2 (MapCurReb ls ds mds n1 n2 l1 l2) =
    let  xs1 = decodeRope rp1 l1
         rps1 = take n1 xs1
         mls = drop n1 xs1
         xs2 = decodeRope rp2 l2
         mrs = take n2 xs2
         rps2 = drop n2 xs2
         mrp = root (curZip (UnzipMapCtx mls mrs mds))
         rs = rps1 ++ [mrp] ++ rps2
    in
      curZip (UnzipMapCtx ls rs ds)

mapLTSUntil :: forall a b . Par Bool
            -> (a -> b)
            -> Rope a
            -> Par (Progress (MapCur a b) (Rope b))
mapLTSUntil cond f rp =
    m xs ctx
  where
    mSeq :: Seq b -> MapCtx a b -> Rope b
    mSeq xs ctx = case next (Leaf xs) ctx of
                    Finished rp' -> rp'
                    More (xs', ctx') -> mSeq (Seq.map f xs') ctx'

    m :: Seq a -> MapCtx a b -> Par (Progress (MapCur a b) (Rope b))
    m xs ctx =
        case Seq.mapUntil cond f xs of
          Finished pseq' -> case next (Leaf pseq') ctx of
                              Finished rp'-> return $ Finished rp'
                              More (seq', ctx') -> m seq' ctx'
          More (pseq', useq') -> if snd (curLength (MapCur pseq' useq' ctx)) >= 2
                                 then return $ More (MapCur pseq' useq' ctx)
                                 else return $ Finished (mSeq (pseq' `Seq.append` Seq.map f useq') ctx)

    xs :: Seq a
    ctx :: MapCtx a b
    (xs, ctx) = leftmost rp MCTop

data Dir = L | R
 deriving (Show)

data UnzipMapCtx a b = UnzipMapCtx [Rope b] [Rope a] [Dir]
 deriving (Show)

type UnzipMapCur a b = UnzipMapCtx a b

ctxUnzip :: MapCtx a b -> UnzipMapCtx a b
ctxUnzip MCTop         = UnzipMapCtx [] [] []
ctxUnzip (MCLeft c r)  = let UnzipMapCtx ls rs ds = ctxUnzip c
                         in
                           UnzipMapCtx ls (r:rs) (L:ds)
ctxUnzip (MCRight l c) = let UnzipMapCtx ls rs ds = ctxUnzip c
                         in
                           UnzipMapCtx (l:ls) rs (R:ds)

curUnzip :: MapCur a b -> UnzipMapCur a b
curUnzip (MapCur pseq useq ctx) =
    UnzipMapCtx (Leaf pseq:ls) (Leaf useq:rs) ds
  where
    (UnzipMapCtx ls rs ds) = ctxUnzip ctx

ctxZip :: UnzipMapCtx a b -> MapCtx a b
ctxZip (UnzipMapCtx [] [] []) = MCTop
ctxZip (UnzipMapCtx ls (r:rs) (L:ds)) = MCLeft (ctxZip (UnzipMapCtx ls rs ds)) r
ctxZip (UnzipMapCtx (l:ls) rs (R:ds)) = MCRight l (ctxZip (UnzipMapCtx ls rs ds))

curZip :: UnzipMapCur a b -> MapCur a b
curZip (UnzipMapCtx (Leaf pseq:ls) (Leaf useq:rs) ds) =
    MapCur pseq useq (ctxZip (UnzipMapCtx ls rs ds))

divideRopes :: [Rope a] -> Int -> ([Rope a], Rope a, Int, [Rope a])
divideRopes (rp:rps) n | n <= length rp =
    ([], rp, n, rps)
divideRopes (rp:rps) n =
    let (rps1, rp', n', rps2) = divideRopes rps (n - length rp)
    in
      (rp:rps1, rp', n', rps2)

splitAtAsCur :: Rope a -> Int -> MapCur a a
splitAtAsCur rp n =
    s rp MCTop n
  where
    s (Leaf seq) ctx n = let (lseq, rseq) = Seq.splitAt n seq
                         in
                           (MapCur lseq rseq ctx)
    s (Cat _ _ lrp rrp) ctx n
        | n < length rrp = s lrp (MCLeft ctx rrp) n
        | otherwise      = s rrp (MCRight lrp ctx) (n - length lrp)

encodeRopes :: forall a . [Rope a] -> (Rope a, Int)
encodeRopes rps =
    (go rps, P.length rps)
  where
    go :: [Rope a] -> Rope a
    go []       = error "encodeRopes: can't happen"
    go [rp]     = rp
    go (rp:rps) = rp `ncat` go rps

decodeRope :: Rope a -> Int -> [Rope a]
decodeRope rp            1 = [rp]
decodeRope (Cat _ _ l r) n = l:decodeRope r (n-1)
decodeRope (Leaf {})     _ = error "decodeRope: can't happen"

data MapCurReb b = MapCurReb [Rope b] [Dir] [Dir]
    {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int
 deriving (Show)

mapLTS :: forall a b . (a -> b) -> Rope a -> Par (Rope b)
{-# INLINE mapLTS #-}
mapLTS f rp0 =
    go rp0
  where
    go :: Rope a -> Par (Rope b)
    go rp@(Leaf u)
        | len < mAX_LEAF_SIZE = do  prog <- mapLTSUntil isHungry f rp
                                    case prog of
                                      Finished rp' -> return $ traceEvent ("finished: "  ++ show (length rp')) $ rp'
                                      More cur' -> do  let (rp1, rp2, reb) = splitCur cur'
                                                       prp1' <- traceEvent ("spawn left: "  ++ show (length rp1)) $
                                                                spawn (mapLTS f rp1)
                                                       prp2' <- traceEvent ("spawn right: " ++ show (length rp2)) $
                                                                spawn (mapLTS f rp2)
                                                       rp1'  <- get prp1'
                                                       rp2'  <- get prp2'
                                                       return $ root (join rp1' rp2' reb)

        | otherwise           = do  let (v, w) = traceEvent "split" $ Seq.splitAt (len `div` 2) u
                                    iv'  <- spawn $ go (Leaf v)
                                    iw'  <- spawn $ go (Leaf w)
                                    v'   <- get iv'
                                    w'   <- get iw'
                                    return $ traceEvent "ncat" $ v' `cat` w'
      where
        len :: Int
        len = Seq.length u

    go (Cat l d v w) = do  iv' <- spawn $ go v
                           iw' <- spawn $ go w
                           v' <- get iv'
                           w' <- get iw'
                           return $ traceEvent "Cat" $ Cat l d v' w'

mapP :: forall a b . (a -> b) -> Rope a -> Par (Rope b)
{-# INLINE mapP #-}
mapP f rp =
    go rp
  where
    go :: Rope a -> Par (Rope b)
    go (Leaf u)
        | len < mAX_LEAF_SIZE = return $ traceEvent "map" $ Leaf (Seq.map f u)
        | otherwise           = do  let (v, w) = traceEvent "split" $ Seq.splitAt (len `div` 2) u
                                    iv'  <- spawn $ go (Leaf v)
                                    iw'  <- spawn $ go (Leaf w)
                                    v'   <- get iv'
                                    w'   <- get iw'
                                    return $ traceEvent "ncat" $ v' `ncat` w'
      where
        len :: Int
        len = Seq.length u

    go (Cat l d v w) = do  iv' <- spawn $ go v
                           iw' <- spawn $ go w
                           v' <- get iv'
                           w' <- get iw'
                           return $ traceEvent "Cat" $ Cat l d v' w'
