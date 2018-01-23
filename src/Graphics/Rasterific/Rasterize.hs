{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
module Graphics.Rasterific.Rasterize
    ( CoverageSpan( .. )
    , rasterize
    , toOpaqueCoverage
    , clip
    ) where

{-import Debug.Trace-}
import Control.DeepSeq
import Control.Monad.Par.Scheds.Sparks( runPar )
import Control.Monad.Par( parMap )
import Control.Monad.ST( runST )
import Data.Fixed( mod' )
import Data.List(groupBy)
import Data.Map.Strict ( Map, fromAscListWith )
import Data.Monoid( Endo( Endo, appEndo ) )
import Graphics.Rasterific.Types
import Graphics.Rasterific.QuadraticBezier
import Graphics.Rasterific.CubicBezier
import Graphics.Rasterific.Line
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VS

data CoverageSpan = CoverageSpan
    { _coverageX      :: {-# UNPACK #-} !Float
    , _coverageY      :: {-# UNPACK #-} !Float
    , _coverageVal    :: {-# UNPACK #-} !Float
    , _coverageLength :: {-# UNPACK #-} !Float
    }
    deriving Show

instance NFData CoverageSpan where rnf !_ = ()

toOpaqueCoverage :: CoverageSpan -> CoverageSpan
{-# INLINE toOpaqueCoverage #-}
toOpaqueCoverage coverage = coverage { _coverageVal = 1 }

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
  case compare ay by of EQ -> compare ax bx; c -> c

-- sort x'es in reverse order
xyCompareR :: EdgeSample -> EdgeSample -> Ordering
{-# INLINE xyCompareR #-}
xyCompareR !(EdgeSample { _sampleY = ay, _sampleX = ax })
           !(EdgeSample { _sampleY = by, _sampleX = bx }) =
  case compare ay by of EQ -> compare bx ax; c -> c

-- NB: choose here
sortAndCombineEdgeSamples :: (Float -> Float) -> [EdgeSample] -> [CoverageSpan]
sortAndCombineEdgeSamples = sortAndCombineEdgeSamples2

rasterize :: FillMethod -> Container Primitive -> [CoverageSpan]
rasterize method primitives =
    let combiner = case method of
                     FillWinding -> combineWinding
                     FillEvenOdd -> combineEvenOdd
        a = foldMap (Endo . decompose) primitives
        b = (($ []) . appEndo) a
    in sortAndCombineEdgeSamples combiner b
  where combineWinding = min 1 . abs
        combineEvenOdd cov = abs $ abs (cov - 1) `mod'` 2 - 1

--------------------------------------------------------------------------------
-- parallelisation of rows
--------------------------------------------------------------------------------

-- based on finding row indices

sortAndCombineEdgeSamples1 :: (Float -> Float) -> [EdgeSample] -> [CoverageSpan]
sortAndCombineEdgeSamples1 f =
  combineEdgeSamples1 f . sortEdgeSamples1 xyCompare

combineEdgeSamples1 :: (Float -> Float) -> V.Vector EdgeSample -> [CoverageSpan]
{-# INLINE combineEdgeSamples1 #-}
combineEdgeSamples1 prepareCoverage samples =
  concat $ runPar $ parMap (go 0 0 0 0) yind
  where
    !maxi = V.length samples
    !hd = samples `V.unsafeIndex` 0
    -- (start, end) index pairs of each y-segment
    (_,_,yind) =
      V.ifoldl'
      (\(y,o,acc) i e ->
        let y' = _sampleY e in
        if i/=maxi-1 && y'==y then (y,o,acc)
        else if y'/=y then (y',i,(o,i):acc)
        else (y',i,(o,maxi):acc) --last element
      )
      (_sampleY hd,0,[])
      samples
    go !x !y !a !_h (!s, !e) | s == e = [CoverageSpan x y (prepareCoverage a) 1]
    go !x !y !a !h (!ix, !e) = sub (samples `V.unsafeIndex` ix) where
      sub (EdgeSample x' y' a' h')
        | x == x' = go x' y' (a + a') (h + h') (ix+1,e)
        | otherwise = p1 : p2 : go x' y' (h + a') (h + h') (ix+1,e)
             where p1 = CoverageSpan x y (prepareCoverage a) 1
                   p2 = CoverageSpan (x + 1) y (prepareCoverage h) (x' - x - 1)

sortEdgeSamples1 :: (EdgeSample -> EdgeSample -> Ordering)
                    -> [EdgeSample] -> V.Vector EdgeSample
sortEdgeSamples1 cmp samples = runST $ do
    -- Resist the urge to make this a storable vector,
    -- it is actually a pessimisation.
    mutableVector <- V.unsafeThaw $ V.fromList samples
    VS.sortBy cmp mutableVector
    V.unsafeFreeze mutableVector

--------------------------------------------------------------------------------

-- based on grouping rows

sortAndCombineEdgeSamples2 :: (Float -> Float) -> [EdgeSample] -> [CoverageSpan]
sortAndCombineEdgeSamples2 f = combineEdgeSamples2 f . sortEdgeSamplesList2

combineEdgeSamples2 :: forall (t :: * -> *) . Traversable t
                   => (Float -> Float) -> t [EdgeSample] -> [CoverageSpan]
{-# INLINE combineEdgeSamples2 #-}
combineEdgeSamples2 prepareCoverage =
  concat . runPar . parMap gogo
  where
    gogo (EdgeSample a b c d:as) = go a b c d as
    -- all edge samples in a sublist have the same y coordinate
    go !x !y !a !_h [] = [CoverageSpan x y (prepareCoverage a) 1]
    go !x !y !a !h (EdgeSample x' y' a' h' : vs)
      | x == x' = go x' y' (a+a') (h+h') vs
      | otherwise = p1 : p2 : go x' y' (h+a') (h+h') vs
      where
        p1 = CoverageSpan x y (prepareCoverage a) 1
        p2 = CoverageSpan (x+1) y (prepareCoverage h) (x'-x-1)

-- map-based grouping
sortEdgeSamplesMap2 :: [EdgeSample] -> Map Float [EdgeSample]
sortEdgeSamplesMap2 samples =
  fromAscListWith (++) [(_sampleY a, [a]) | a <- sorted]
  where sorted = V.toList $ sortEdgeSamples1 xyCompareR samples

-- list-based grouping
sortEdgeSamplesList2 :: [EdgeSample] -> [[EdgeSample]]
sortEdgeSamplesList2 samples =
  groupBy (\a b -> _sampleY a == _sampleY b) sorted
  where sorted = V.toList $ sortEdgeSamples1 xyCompare samples

--------------------------------------------------------------------------------
