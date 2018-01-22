import Internal( EdgeSample(..), parQuickSort, parSort, xyCompare )
import Text.Printf
import Control.Exception
import System.CPUTime
import Control.DeepSeq
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VS
import Control.Monad.ST

sortEdgeSamples :: [EdgeSample] -> V.Vector EdgeSample
sortEdgeSamples samples = runST $ do
  -- Resist the urge to make this a storable vector,
  -- it is actually a pessimisation.
  mutableVector <- V.unsafeThaw $ V.fromList samples
  parSort mutableVector
  -- VS.sortBy xyCompare mutableVector
  V.unsafeFreeze mutableVector
  --V.fromList $ parQuickSort samples

getXy :: EdgeSample -> (Float, Float)
getXy x = (_sampleX x, _sampleY x)

main :: IO ()
main = do
  let xs = [x | x <- [1..]] :: [Int]
      ys = [y*2 | y <- [6..]] :: [Int]
      list = take 100 [x `mod` 11 | x <- xs]
      edgesamples = [EdgeSample { _sampleX = fromIntegral  (x `mod` 10),
                                  _sampleY = fromIntegral (y `mod` 30),
                                  _sampleAlpha = (1.5::Float),
                                  _sampleH = (2.1 :: Float)
                                }
                    | x <- xs,
                      y <- ys]
      pa = take 100000 edgesamples
  start <- getCPUTime
  let sortedPar = sortEdgeSamples pa
  end <- sortedPar `seq` getCPUTime
  let time = (fromIntegral (end - start)) / (10^12)
  printf "Time: %0.9f sec\n" (time :: Double)
