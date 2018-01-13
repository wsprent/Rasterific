import Control.Monad( replicateM )
import qualified Data.Vector as V
import Codec.Picture
    ( Image
    , PixelRGBA8( .. )
    , writePng
    )
import Graphics.Rasterific
    ( Drawing
    , V2( .. )
    , Line( .. )
    , fill
    , withTexture
    , renderDrawing
    )

import Graphics.Rasterific.Texture( uniformTexture )
import System.Random( randomRIO )
import Criterion( whnf, bench )
import Criterion.Main( defaultMain )

randomColor :: IO PixelRGBA8
randomColor = PixelRGBA8
              <$> randomRIO (0, 255)
              <*> randomRIO (0, 255)
              <*> randomRIO (0, 255)
              <*> randomRIO (0, 255)

randomLine :: Float -> Float -> IO (Line, PixelRGBA8)
randomLine w h = do
  p1 <- V2 <$> randomRIO (0.0 , w) <*> randomRIO (0.0 , h)
  p2 <- V2 <$> randomRIO (0.0 , w) <*> randomRIO (0.0 , h)
  col <- randomColor
  return (Line p1 p2, col)
  
renderLine :: (Line, PixelRGBA8) -> Drawing PixelRGBA8 ()
renderLine (line, col)  =
 (withTexture (uniformTexture col) . 
     fill) line

renderLines :: Float -> Float -> V.Vector (Line, PixelRGBA8) -> Image PixelRGBA8
renderLines width height l =
  renderDrawing (floor width) (floor height) background $
    foldMap renderLine l
    where
      background = PixelRGBA8 0 0 0 255
      -- background = PixelRGBA8 255 255 255 255

benchLines :: Float -> Float -> V.Vector (Line, PixelRGBA8) -> Image PixelRGBA8
benchLines width height l =
  renderDrawing (floor width) (floor height) background $
    foldMap renderLine l
    where
      background = PixelRGBA8 0 0 0 255

main :: IO ()
main = do
  let width = 800
      height = 600
  l <- V.fromList <$> replicateM 100000 (randomLine width height)
  writePng "lines.png" $
    renderLines width height l
  -- benching
  defaultMain [bench "flake draw" $ whnf (benchLines width height) l]

