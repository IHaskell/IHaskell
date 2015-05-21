{-# LANGUAGE NoImplicitPrelude, TypeSynonymInstances, FlexibleInstances #-}

module IHaskell.Display.Juicypixels (module IHaskell.Display, module Codec.Picture) where

import           Codec.Picture
import           ClassyPrelude
import           IHaskell.Display
import           System.Directory
import           System.IO.Unsafe

-- instances
instance IHaskellDisplay DynamicImage where
  display = displayImageAsJpg

instance IHaskellDisplay (Image Pixel8) where
  display = displayImageAsJpg . ImageY8

instance IHaskellDisplay (Image Pixel16) where
  display = displayImageAsJpg . ImageY16

instance IHaskellDisplay (Image PixelF) where
  display = displayImageAsJpg . ImageYF

instance IHaskellDisplay (Image PixelYA8) where
  display = displayImageAsJpg . ImageYA8

instance IHaskellDisplay (Image PixelYA16) where
  display = displayImageAsJpg . ImageYA16

instance IHaskellDisplay (Image PixelRGB8) where
  display = displayImageAsJpg . ImageRGB8

instance IHaskellDisplay (Image PixelRGB16) where
  display = displayImageAsJpg . ImageRGB16

instance IHaskellDisplay (Image PixelRGBF) where
  display = displayImageAsJpg . ImageRGBF

instance IHaskellDisplay (Image PixelRGBA8) where
  display = displayImageAsJpg . ImageRGBA8

instance IHaskellDisplay (Image PixelRGBA16) where
  display = displayImageAsJpg . ImageRGBA16

instance IHaskellDisplay (Image PixelYCbCr8) where
  display = displayImageAsJpg . ImageYCbCr8

instance IHaskellDisplay (Image PixelCMYK8) where
  display = displayImageAsJpg . ImageCMYK8

instance IHaskellDisplay (Image PixelCMYK16) where
  display = displayImageAsJpg . ImageCMYK16

-- main rendering function
displayImageAsJpg :: DynamicImage -> IO Display
displayImageAsJpg renderable = do
  switchToTmpDir

  let filename = ".ihaskell.juicypixels.jpg"
  -- Write the image
  saveJpgImage 95 filename renderable
  -- Convert to base64.
  imgData <- readFile filename
  return $ Display [jpg (imWidth renderable) (imHeight renderable) $ base64 imgData]

-- The type DynamicImage does not have a function to extract width and height
imWidth :: DynamicImage -> Int
imWidth img = w
  where
    (w, h) = imWidthHeight img

imHeight :: DynamicImage -> Int
imHeight img = h
  where
    (w, h) = imWidthHeight img

-- Helper functions to pattern match on the DynamicImage Constructors
imWidthHeight :: DynamicImage -> (Int, Int)
imWidthHeight (ImageY8 im) = imWH im
imWidthHeight (ImageY16 im) = imWH im
imWidthHeight (ImageYF im) = imWH im
imWidthHeight (ImageYA8 im) = imWH im
imWidthHeight (ImageYA16 im) = imWH im
imWidthHeight (ImageRGB8 im) = imWH im
imWidthHeight (ImageRGB16 im) = imWH im
imWidthHeight (ImageRGBF im) = imWH im
imWidthHeight (ImageRGBA8 im) = imWH im
imWidthHeight (ImageRGBA16 im) = imWH im
imWidthHeight (ImageYCbCr8 im) = imWH im
imWidthHeight (ImageCMYK8 im) = imWH im
imWidthHeight (ImageCMYK16 im) = imWH im

imWH :: (Image a) -> (Int, Int)
imWH im = (imageWidth im, imageHeight im)
