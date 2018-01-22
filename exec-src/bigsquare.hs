import Codec.Picture (Image, PixelRGBA8( .. ), writePng)
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Criterion( whnf, bench )
import Criterion.Main( defaultMain )

renderSquare :: Float -> Image PixelRGBA8
renderSquare n =
  let ni = round n
      white = PixelRGBA8 255 255 255 255
      recColor = PixelRGBA8 0xFF 0x53 0x73 255
  in renderDrawing ni ni white $
       withTexture (uniformTexture recColor) .
       fill $ rectangle (V2 0 0) n n

main :: IO ()
main = defaultMain [bench "big square draw" $ whnf renderSquare 10000]
 -- writePng "bigsquare.png" renderSquare
