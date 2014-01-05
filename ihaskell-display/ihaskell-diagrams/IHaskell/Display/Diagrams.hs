{-# LANGUAGE NoImplicitPrelude, TypeSynonymInstances, FlexibleInstances  #-}
module IHaskell.Display.Diagrams where

import ClassyPrelude

import System.Directory
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as Char
import System.IO.Unsafe

import Diagrams.Prelude
import Diagrams.Backend.Cairo

import IHaskell.Display

instance IHaskellDisplay (Diagram Cairo R2) where
  display renderable = do
    (width, height, imgData) <- diagramData renderable PNG
    (_, _, svgData) <- diagramData renderable SVG
    return [png (floor width) (floor height) imgData, svg svgData]

diagramData :: Diagram Cairo R2 -> OutputType -> IO (Double, Double, String)
diagramData renderable format = do
  -- Switch to a temporary directory so that any files we create aren't
  -- visible. On Unix, this is usually /tmp.
  try (getTemporaryDirectory >>= setCurrentDirectory) :: IO (Either SomeException ())

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
  let value = Char.unpack $ case format of
        PNG -> Base64.encode imgData
        _ ->   imgData

  return (imgWidth, imgHeight, value)
  where
    extension SVG = "svg"
    extension PNG = "png"

-- Rendering hint.
diagram :: Diagram Cairo R2 -> Diagram Cairo R2
diagram = id
