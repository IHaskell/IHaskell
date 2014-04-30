{-# LANGUAGE NoImplicitPrelude, TypeSynonymInstances, FlexibleInstances  #-}
module IHaskell.Display.Diagrams (diagram) where

import ClassyPrelude

import System.Directory
import qualified Data.ByteString.Char8 as Char
import System.IO.Unsafe

import Diagrams.Prelude
import Diagrams.Backend.Cairo

import IHaskell.Display

instance IHaskellDisplay (Diagram Cairo R2) where
  display renderable = do
    png <- diagramData renderable PNG
    svg <- diagramData renderable SVG
    return $ Display [png, svg]

diagramData :: Diagram Cairo R2 -> OutputType -> IO DisplayData
diagramData renderable format = do
  switchToTmpDir

  -- Compute width and height.
  let w = width renderable
      h = height renderable
      aspect = w / h
      imgHeight = 300
      imgWidth = aspect * imgHeight

  -- Write the image.
  let filename = ".ihaskell-diagram." ++ extension format
  renderCairo filename (Height imgHeight) renderable

  -- Convert to base64.
  imgData <- readFile $ fpFromString filename
  let value = case format of
        PNG -> png (floor imgWidth) (floor imgHeight) $ base64 imgData
        SVG -> svg $ Char.unpack imgData

  return value
  where
    extension SVG = "svg"
    extension PNG = "png"

-- Rendering hint.
diagram :: Diagram Cairo R2 -> Diagram Cairo R2
diagram = id
