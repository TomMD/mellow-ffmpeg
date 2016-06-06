import Mellow.FFmpeg        -- The main 'Mellow' module
import Vision.Image as I    -- The Friday library for image manipulation
import Vision.Detector.Edge (canny)

main :: IO ()
main =
  mellow blackRGBA                      -- Black image to start
         (\d _ -> return (getEdges d))  -- Custom image manipulator
         return                         -- Noop renders
         (defaultEventHandler return)
              -- The default event handler allows us to quit using 'esc'
              -- as well as save screen captures with 's'.

getEdges :: RGBA -> RGBA
getEdges i0 =
  let g,bl,ed :: Grey
      g  = convert i0        -- Obtain the Grey image from the RGBA
      bl = blur 3 g          -- Blur to get rid of noise
      ed = canny 2 64 512 bl -- Canny edge detection with some reasonable constants
  in convert ed              -- Convert to RGBA for rendering

blackRGBA :: RGBA
blackRGBA = I.fromFunction (Z :. 480 :. 640) (const (RGBAPixel 0 0 0 0))
