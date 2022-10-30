module IHaskell.Display.Plot where

import qualified Data.ByteString.Char8 as Char
import           Graphics.Rendering.Plot
import           Control.Monad (void)
import           Control.Applicative ((<$>))
import           System.IO.Temp
import           System.FilePath ((</>))

import           IHaskell.Display

instance IHaskellDisplay (Figure a) where
  display fig = do
    let figure = void fig
    pngDisp <- figureData figure PNG
    svgDisp <- figureData figure SVG
    return $ Display [pngDisp, svgDisp]

figureData :: Figure () -> OutputType -> IO DisplayData
figureData figure format = do
  withSystemTempDirectory "ihaskell-plot" $ \tmpdir  -> do

    -- Width and height
    let size = 300
        w = size
        h = size

    let path = case format of
          PNG -> tmpdir </> "ihaskell-plot.png"
          SVG -> tmpdir </> "ihaslell-plot.svg"
          _ -> error "Unreachable case"

    -- Write the image.
    writeFigure format path (w, h) figure

    -- Read back, and convert to base64.
    imgData <- Char.readFile path
    let value =
          case format of
            PNG -> png w h $ base64 imgData
            SVG -> svg $ Char.unpack imgData
            _   -> error "Unsupported format for display"

    return value
