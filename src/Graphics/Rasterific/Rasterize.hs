{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Rasterific.Rasterize
    ( CoverageSpan( .. )
    , rasterize
    , toOpaqueCoverage
    , clip
    ) where

import GHC.IO
import Unsafe.Coerce
import Debug.Trace
import Control.DeepSeq
import Control.Parallel.Strategies
import Control.Parallel
--import Control.Monad.
import Control.Monad.Primitive
import Control.Monad.ST
import Control.Monad.ST.Unsafe
import Data.Fixed( mod' )
import Data.Monoid( Endo( Endo, appEndo ) )
import Graphics.Rasterific.Types
import Graphics.Rasterific.QuadraticBezier
import Graphics.Rasterific.CubicBezier
import Graphics.Rasterific.Line
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VS
import qualified Data.Vector.Algorithms.Optimal as O
import qualified Data.Vector.Generic.Mutable as MV

data CoverageSpan = CoverageSpan
    { _coverageX      :: {-# UNPACK #-} !Float
    , _coverageY      :: {-# UNPACK #-} !Float
    , _coverageVal    :: {-# UNPACK #-} !Float
    , _coverageLength :: {-# UNPACK #-} !Float
    }
    deriving Show

toOpaqueCoverage :: CoverageSpan -> CoverageSpan
{-# INLINE toOpaqueCoverage #-}
toOpaqueCoverage coverage = coverage { _coverageVal = 1 }

combineEdgeSamples :: (Float -> Float) -> V.Vector EdgeSample
                   -> [CoverageSpan]
{-# INLINE combineEdgeSamples #-}
combineEdgeSamples prepareCoverage vec = go 0 0 0 0 0
  where
    !maxi = V.length vec
    go !ix !x !y !a !_h | ix >= maxi = [CoverageSpan x y (prepareCoverage a) 1]
    go !ix !x !y !a !h = sub (vec `V.unsafeIndex` ix) where
      sub (EdgeSample x' y' a' h')
        | y == y' && x == x' = go (ix + 1) x' y' (a + a') (h + h')
        | y == y' = p1 : p2 : go (ix + 1) x' y' (h + a') (h + h')
        | otherwise =
           CoverageSpan x y (prepareCoverage a) 1 : go (ix + 1) x' y' a' h'
             where p1 = CoverageSpan x y (prepareCoverage a) 1
                   p2 = CoverageSpan (x + 1) y (prepareCoverage h) (x' - x - 1)

-- | Clip the geometry to a rectangle.
clip :: Point     -- ^ Minimum point (corner upper left)
     -> Point     -- ^ Maximum point (corner bottom right)
     -> Primitive -- ^ Primitive to be clipped
     -> Container Primitive
clip mini maxi (LinePrim l) = clipLine mini maxi l
clip mini maxi (BezierPrim b) = clipBezier mini maxi b
clip mini maxi (CubicBezierPrim c) = clipCubicBezier mini maxi c

decompose :: Primitive -> Producer EdgeSample
decompose (LinePrim l) = decomposeLine l
decompose (BezierPrim b) = decomposeBeziers b
decompose (CubicBezierPrim c) =
    {-decomposeCubicBezierForwardDifference c-}
    decomposeCubicBeziers c

xyCompare :: EdgeSample -> EdgeSample -> Ordering
{-# INLINE xyCompare #-}
xyCompare !(EdgeSample { _sampleY = ay, _sampleX = ax })
          !(EdgeSample { _sampleY = by, _sampleX = bx }) =
  case compare ay by of
    EQ -> compare ax bx
    c -> c

--Borrowed from
--https://hackage.haskell.org/pac-- kage/vector-algorithms-0.7.0.1/docs/src/Data-Vector-Algorithms-Intro.html#partitionBy
partitionBy :: forall m v e. (PrimMonad m, MV.MVector v e)
            => VS.Comparison e -> v (PrimState m) e -> e -> Int -> Int -> m Int
{-# INLINE partitionBy #-}
partitionBy cmp a = partUp
 where
 partUp :: e -> Int -> Int -> m Int
 partUp p l u
   | l < u = do e <- MV.unsafeRead a l
                case cmp e p of
                  LT -> partUp p (l+1) u
                  _  -> partDown p l (u-1)
   | otherwise = return l

 partDown :: e -> Int -> Int -> m Int
 partDown p l u
   | l < u = do e <- MV.unsafeRead a u
                case cmp p e of
                  LT -> partDown p l (u-1)
                  _  -> MV.unsafeSwap a l u >> partUp p (l+1) u
   | otherwise = return l

parST :: ST s a -> ST s a
parST m = x `par` return x
  where
    x = runST (unsafeIOToST noDuplicate >> unsafeCoerce m)

-- parSort :: (PrimMonad m, MV.MVector v e) => VS.Comparison e -> v (PrimState m) e -> ST (m ())
parSort :: (MV.MVector v e, Ord e) => v s e -> ST s ()
parSort a 
  | n < 2 = return ()
  | n < 2^12 = VS.sort a
  | otherwise = do
      p <- MV.unsafeRead a (n `div` 2)
      m <- MV.unstablePartition (<p) a -- partitionBy cmp a p 0 n
      --MV.unsafeSwap a 0 (m-1)
      let a1 = MV.unsafeSlice 0 m a
      let a2 = MV.unsafeSlice (max m 1) (n-(max m 1)) a
      v <- parST $ parSort a1
      parSort a2
      v `seq` return ()
  where
    n = MV.length a
              
instance Eq EdgeSample where
  x == y = case xyCompare x y of
    EQ -> True
    _ -> False

instance Ord EdgeSample where
  compare = xyCompare

parQuickSort :: (Ord a) => [a] -> [a]
parQuickSort [] = []
parQuickSort (x:xs) = runEval $ do
  preS <- rpar $ parQuickSort pre
  postS <- rpar $ parQuickSort post
  rseq preS
  rseq postS
  return (preS ++ mid ++ postS)
  where pre = [e | e <- xs, e < x]
        mid = [e | e <- xs, e == x]
        post = [e | e <- xs, e > x]

sortEdgeSamples :: [EdgeSample] -> V.Vector EdgeSample
sortEdgeSamples samples = runST $ do
  -- Resist the urge to make this a storable vector,
  -- it is actually a pessimisation.
  mutableVector <- V.unsafeThaw $ V.fromList samples
  parSort mutableVector
  V.unsafeFreeze mutableVector
  --V.fromList $ parQuickSort samples
  

rasterize :: FillMethod -> Container Primitive -> [CoverageSpan]
rasterize method = 
  case method of
    FillWinding -> combineEdgeSamples combineWinding 
                        . sortEdgeSamples
                        . (($ []) . appEndo)
                        . foldMap (Endo . decompose)
    FillEvenOdd -> combineEdgeSamples combineEvenOdd
                        . sortEdgeSamples
                        . (($ []) . appEndo)
                        . foldMap (Endo . decompose)
  where combineWinding = min 1 . abs
        combineEvenOdd cov = abs $ abs (cov - 1) `mod'` 2 - 1

