import Mellow.FFmpeg        -- The main 'Mellow' module
import Vision.Image as I    -- The Friday library for image manipulation
import Vision.Detector.Edge (canny)

main :: IO ()
main =
  mellow blackRGBA                      -- Black image to start
         (\d _ -> return (imgWords d))  -- Noop conversion for straight video
         return                         -- Noop renders
         (defaultEventHandler return)
              -- The default event handler allows us to quit using 'esc'
              -- as well as save screen captures with 's'.

imgWords :: RGBA -> RGBA
imgWords i0 =
  let g,bl,ot,ed :: Grey
      g  = convert i0
      bl = blur 2 g
      ed = canny 2 0 0 bl -- No, I don't know good canny constants.
  in convert ed

blackRGBA :: RGBA
blackRGBA = I.fromFunction (Z :. 480 :. 640) (const (RGBAPixel 0 0 0 0))
