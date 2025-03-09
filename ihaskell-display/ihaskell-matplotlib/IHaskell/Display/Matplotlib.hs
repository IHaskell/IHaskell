module IHaskell.Display.Matplotlib where

import qualified Data.ByteString.Char8 as Char
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.Text.Encoding as T.Encoding
import           System.IO.Temp
import           System.FilePath ((</>))
import           Graphics.Matplotlib
import           IHaskell.Display

instance IHaskellDisplay Matplotlib where
  display = graphDataDisplayBoth

-- Width and height
w, h :: Int 
w = 300
h = 300

graphDataPNG :: Matplotlib -> IO DisplayData
graphDataPNG m = do
  withSystemTempDirectory "ihaskell-matplotlib" $ \tmpdir -> do
    let path = tmpdir </> "ihaskell-matplotlib.png"

    -- Write the image.
    res <- file path m
    case res of
      Left  _ -> error "Matplotlib could not generate an immage"
      Right _ -> do
        -- Read back, and convert to base64.
        imgData <- Char.readFile path
        return $ png w h $ base64 imgData

graphDataSVG :: Matplotlib -> IO DisplayData
graphDataSVG m = do
  res <- toSvg m
  case res of
    Left  s -> error s
    Right f -> return $ svg $ T.Encoding.decodeUtf8 $ BSU.fromString f

graphDataDisplayBoth :: Matplotlib -> IO Display
graphDataDisplayBoth fig = do
    pngDisp <- graphDataPNG fig
    svgDisp <- graphDataSVG fig
    return $ Display [pngDisp, svgDisp]
