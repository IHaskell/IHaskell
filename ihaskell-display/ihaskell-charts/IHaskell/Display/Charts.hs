module IHaskell.Display.Charts () where

import           System.Directory
import           Data.Default.Class
import           Graphics.Rendering.Chart.Renderable
import           Graphics.Rendering.Chart.Backend.Cairo
import qualified Data.ByteString.Char8 as Char
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T.Encoding
import           System.IO.Temp
import           System.IO.Unsafe
import           System.FilePath ((</>))

import           IHaskell.Display

width :: Width
width = 450

height :: Height
height = 300

instance IHaskellDisplay (Renderable a) where
  display renderable = do
    pngDisp <- chartData renderable PNG

    -- We can add `svg svgDisplay` to the output of `display`, but SVGs are not resizable in the IPython
    -- notebook.
    svgDisp <- chartData renderable SVG

    return $ Display [pngDisp, svgDisp]

chartData :: Renderable a -> FileFormat -> IO DisplayData
chartData renderable format = do
  -- We should not have to round-trip this ByteString to a temp file.
  -- https://github.com/IHaskell/IHaskell/issues/1248
  withSystemTempDirectory "ihaskell-charts" $ \tmpdir -> do

  -- Write the PNG image.
    let
      filename = tmpdir </> "ihaskell-chart.png"
      opts = def { _fo_format = format, _fo_size = (width, height) }
    renderableToFile opts filename renderable

    case format of
      PNG -> do
        -- Convert to base64.
        imgData <- Char.readFile filename
        pure $ png width height $ base64 imgData
      SVG -> do
        imgData <- BS.readFile filename
        pure $ svg $ T.Encoding.decodeUtf8 imgData
      _ -> error "Unreachable case, not PNG or SVG"
