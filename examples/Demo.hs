{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}
import Mellow.FFmpeg      -- The main 'Mellow' module
import Vision.Image as I hiding (map) -- The Friday library for image manipulation
import Vision.Image.Contour

main :: IO ()
main =
  mellow blackRGBA                         -- Black image to start
         (\d _ -> return (imgWords d))     -- Noop conversion for straight video
         return                            -- Noop renders
         (defaultEventHandler return)
              -- The default event handler allows us to quit using 'esc'
              -- as well as save screen captures with 's'.

imgWords :: RGBA -> RGBA
imgWords i0 =
  let g :: Grey
      g  = convert i0
      ot = otsu (BinaryThreshold 255 0) g :: Grey
      th = tophat 1 ot
      sz = tallContours 10 th
      sc   = scw (Z :. 7 :. 2) (Z :. 3 :. 2) beta (BinaryThreshold 255 0) g :: Grey
      scEr = erode 1 sc :: Grey
  in torgba scEr
 where
  beta = 0.5 :: Double
  torgba :: Grey -> RGBA
  torgba = convert

  tophat :: Int -> Grey -> Grey
  tophat v x = x .- erode v x

  (.-) :: Grey -> Grey -> Grey
  (.-) = imgZipWith (-)

  imgZipWith :: (FromFunction i, ImagePixel i ~ p, MaskedImage i, Image i, FromFunctionPixel i ~ p) => (p -> p -> p) -> i -> i -> i
  imgZipWith f x y = fromFunction (shape x) (\p -> f (x ! p) (y ! p))

  tallContours :: Int -> Grey -> Grey
  tallContours h i =
     let cs  = contours i
         p cid = let ys = map height (contourPerimeter cid cs)
                 in abs (maximum ys - minimum ys) >= h
         matches :: [ContourId]
         matches = filter p (allContourIds cs)
         height (Z :. h :. _) = h
     in drawContours cs FillWithHoles matches (shape i)

  -- Friday image manipulation map operation
  -- let i1 = otsu i0
  --     i2 = whiteTopHat i1
  --     cs  = contours i2
  --     cs' = filterLargeContours cs
  --     i3  = drawContours (allContours cs')
  --     i4  = dilate i3

blackRGBA :: RGBA
blackRGBA = I.fromFunction (Z :. 480 :. 640) (const (RGBAPixel 0 0 0 0))
