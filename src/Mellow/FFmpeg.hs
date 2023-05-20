{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE MultiWayIf         #-}
module Mellow.FFmpeg
  ( -- * Types
    MellowCfg(..)
  , Display(..)
  , defaultCfg
    -- * Main Interface
  , mellow, mellowWith
  , defaultEventHandler
    -- * Friday re-exports
  , Z(..), shape, (:.)(..), DIM2
  ) where

import Codec.FFmpeg as FF
import Codec.Picture (DynamicImage(..))
import Codec.Picture.Saving (imageToJpg)
import Control.Concurrent
import Control.Monad
import qualified Data.ByteString.Lazy as Lazy
import Data.Maybe (fromMaybe)
import Data.Time
import Foreign.Storable ()
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Juicy (fromImageRGBA8)
import System.Exit
import Vision.Image as I
import Vision.Image.JuicyPixels (toJuicyRGBA, toFridayRGBA)
import Vision.Primitive

data MellowCfg s =
      MellowCfg { world           :: s
                , resolution      :: Display
                , ffmpegInput     :: Maybe FF.InputSource
                , backgroundColor :: Color
                , framerate       :: Int
                , updateOp        :: RGBA -> s -> IO s
                , renderOp        :: s -> IO RGBA
                , eventOp         :: Event -> s -> IO s
                }

defaultCfg :: s                        -- ^ Initial state
           -> (RGBA -> s -> IO s)      -- ^ Integrate new depth frame
           -> (s -> IO RGBA)           -- ^ Render state into an RGBA image
           -> (Event -> s -> IO s)     -- ^ Event handler
           -> MellowCfg s
defaultCfg s = MellowCfg s (InWindow "Mellow" (640,480) (200,200)) Nothing black 20

mellowWith :: MellowCfg s -> IO ()
mellowWith (MellowCfg {..}) = do
  ref        <- newEmptyMVar :: IO (MVar RGBA)
  worldRef   <- newMVar world
  let rdImg   = takeMVar ref
      wtImg x = void (tryPutMVar ref (toFridayRGBA x))

  -- Start a thread reading frames.
  initFFmpeg
  let defIpt = Camera "0:0" defaultCameraConfig { FF.resolution = Just (640,480) }
      ffipt  = fromMaybe defIpt ffmpegInput
  (get,_) <- imageReader ffipt
  _ <- forkIO $ forever (get >>= maybe (return ()) wtImg)

  -- Start a thread that updates the state with each frame.
  _ <- forkIO $ forever $
                do i <- rdImg
                   modifyMVar_ worldRef (updateOp i)

  playIO resolution backgroundColor framerate ()
         (const $ readMVar worldRef >>= (fmap toPicture . renderOp))
         (\e () -> modifyMVar_ worldRef $ eventOp e)
         (const return)

-- | @mellow state0 updateOp renderOp keyPress@ will continually call
-- updateOp with each new frame from the camera, call @renderOp@ to render
-- the frame using Friday, and @keyPress@ to handle key presses.
mellow :: s -> (RGBA -> s -> IO s) -> (s -> IO RGBA) -> (Event -> s -> IO s) -> IO ()
mellow world updateOp renderOp keyPress =
  mellowWith (defaultCfg world updateOp renderOp keyPress)

toPicture :: RGBA -> Picture
toPicture = fromImageRGBA8 . toJuicyRGBA

-- |
-- Example handle event:
--   * 'esc' quit
--   * 's' save the frame
defaultEventHandler :: (s -> IO RGBA) -> Event -> s -> IO s
defaultEventHandler _ (EventKey (SpecialKey KeyEsc) _ _ _) _ = exitSuccess
defaultEventHandler rend (EventKey (Char 's') Down _ _) st =
  do jpg <- toJuicyRGBA <$> rend st
     let bs = imageToJpg 98 (ImageRGBA8 jpg)
     t <- getCurrentTime
     Lazy.writeFile (show t ++ ".jpg") bs
     return st
defaultEventHandler _ _ st = return st
