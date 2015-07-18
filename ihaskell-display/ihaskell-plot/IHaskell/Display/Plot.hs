module IHaskell.Display.Plot where

import qualified Data.ByteString.Char8 as Char
import           Graphics.Rendering.Plot
import           Control.Monad (void)
import           Control.Applicative ((<$>))

import           IHaskell.Display

instance IHaskellDisplay (Figure a) where
  display fig = do
    let figure = void fig
    pngDisp <- figureData figure PNG
    svgDisp <- figureData figure SVG
    return $ Display [pngDisp, svgDisp]

figureData :: Figure () -> OutputType -> IO DisplayData
figureData figure format = do
  switchToTmpDir

  -- Width and height
  let size = 300
      w = size
      h = size

  -- Write the image.
  let fname = ".ihaskell-plot." ++ extension format
  writeFigure format fname (w, h) figure

  -- Read back, and convert to base64.
  imgData <- Char.readFile fname
  let value =
        case format of
          PNG -> png w h $ base64 imgData
          SVG -> svg $ Char.unpack imgData
          _   -> error "Unsupported format for display"

  return value

  where
    extension SVG = "svg"
    extension PNG = "png"
    extension _ = ""
