{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module IHaskell.Display.Juicypixels (module IHaskell.Display, module Codec.Picture) where

import qualified Codec.Picture as P
import           Codec.Picture (Image(..))
import           Codec.Picture.Png (PngSavable, encodePng)
import           IHaskell.Display (IHaskellDisplay, Display(..), display, png, base64)
import           Data.ByteString.Lazy (ByteString, toStrict)

instance IHaskellDisplay (Image P.Pixel8) where
  display = return . format

instance IHaskellDisplay (Image P.Pixel16) where
  display = return . format

instance IHaskellDisplay (Image P.PixelYA8) where
  display = return . format

instance IHaskellDisplay (Image P.PixelYA16) where
  display = return . format

instance IHaskellDisplay (Image P.PixelRGB8) where
  display = return . format

instance IHaskellDisplay (Image P.PixelRGB16) where
  display = return . format

instance IHaskellDisplay (Image P.PixelRGBA8) where
  display = return . format

instance IHaskellDisplay (Image P.PixelRGBA16) where
  display = return . format

format :: PngSavable a => Image a -> Display
format im@(Image w h _) = Display [png w h . base64 . toStrict . encodePng $ im]
