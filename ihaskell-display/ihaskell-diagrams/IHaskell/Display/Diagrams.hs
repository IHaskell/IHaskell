{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module IHaskell.Display.Diagrams (diagram, animation, imgHeight) where

import qualified Data.ByteString.Char8 as Char
import           System.Directory
import           System.IO.Unsafe
import           Data.IORef
import           Diagrams.Backend.Cairo
import           Diagrams.Prelude
import           IHaskell.Display
import           IHaskell.Display.Diagrams.Animation

instance IHaskellDisplay (QDiagram Cairo V2 Double Any) where
  display renderable = do
    png <- diagramData renderable PNG
    svg <- diagramData renderable SVG
    return $ Display [png, svg]

{-# NOINLINE imgHeight #-}
imgHeight :: IORef Double
imgHeight = unsafePerformIO (newIORef 300)

diagramData :: Diagram Cairo -> OutputType -> IO DisplayData
diagramData renderable format = do
  switchToTmpDir

  imgHeight' <- readIORef imgHeight

  -- Compute width and height.
  let w = width renderable
      h = height renderable
      aspect = w / h
      imgWidth = aspect * imgHeight'

  -- Write the image.
  let filename = ".ihaskell-diagram." ++ extension format
  renderCairo filename (mkSizeSpec2D (Just imgWidth) (Just imgHeight')) renderable

  -- Convert to base64.
  imgData <- Char.readFile filename
  let value =
        case format of
          PNG -> png (floor imgWidth) (floor imgHeight') $ base64 imgData
          SVG -> svg (Char.unpack imgData)

  return value

  where
    extension SVG = "svg"
    extension PNG = "png"

-- Rendering hint.
diagram :: Diagram Cairo -> Diagram Cairo
diagram = id
