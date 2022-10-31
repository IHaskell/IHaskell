module IHaskell.Display.Plot where

import qualified Data.ByteString.Char8 as Char
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T.Encoding
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

    case format of
      PNG -> do
        -- Read back, and convert to base64.
        imgData <- Char.readFile path
        pure $ png w h $ base64 imgData
      SVG -> do
        imgData <- BS.readFile path
        pure $ svg $ T.Encoding.decodeUtf8 imgData
      _   -> error "Unreachable case"
