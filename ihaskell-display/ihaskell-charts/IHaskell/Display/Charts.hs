{-# LANGUAGE NoImplicitPrelude #-}
module IHaskell.Display.Charts where

import ClassyPrelude

import System.Directory
import Data.Default.Class
import Graphics.Rendering.Chart.Renderable
import Graphics.Rendering.Chart.Backend.Cairo
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as Char
import System.IO.Unsafe

import IHaskell.Display

width :: Width
width = 600

height :: Height
height = 400

instance IHaskellDisplay (Renderable a) where
  display renderable = do
    imgData <- chartData renderable PNG

    -- We can add `svg svgDisplay` to the output of `display`,
    -- but SVGs are not resizable in the IPython notebook.
    svgDisplay <- chartData renderable SVG

    return [png width height imgData, svg svgDisplay]

chartData :: Renderable a -> FileFormat -> IO String
chartData renderable format = do
  -- Switch to a temporary directory so that any files we create aren't
  -- visible. On Unix, this is usually /tmp.
  try (getTemporaryDirectory >>= setCurrentDirectory) :: IO (Either SomeException ())

  -- Write the PNG image.
  let filename = ".ihaskell-chart.png"
      opts = def{_fo_format = format, _fo_size = (width, height)}
  renderableToFile opts renderable filename

  -- Convert to base64.
  imgData <- readFile $ fpFromString filename
  return $ Char.unpack $ case format of
    PNG -> Base64.encode imgData
    _ -> imgData
