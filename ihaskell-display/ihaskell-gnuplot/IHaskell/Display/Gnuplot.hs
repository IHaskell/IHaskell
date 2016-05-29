{-# LANGUAGE FlexibleInstances #-}

{- There are 3 types of plots to consider in haskell-gnuplot: Plot, Frame and Multiplot.
   Plot types are the actual plots, whereas Frame types are plots with additional options
   e.g. custom axes tics, graph title etc.. Multiplots are collections of 2D and/or 3D plots.
   We have to create instances of IHaskellDisplay for all of these types.

   Note: To stop gnuplot from printing the filepath ontop of the canvas, you have to set
         the gnuplot option "key" to "noautotitle".
         Code: Graphics.Gnuplot.Frame.cons (Graphics.Gnuplot.Frame.OptionSet.add
                                                    (Graphics.Gnuplot.Frame.Option.key "")
                                                    ["noautotitle"] $ ...)
-}
module IHaskell.Display.Gnuplot where

import qualified Graphics.Gnuplot.Plot as P
import qualified Graphics.Gnuplot.Frame as F
import qualified Graphics.Gnuplot.MultiPlot as M
import qualified Graphics.Gnuplot.Terminal.PNG as Pn
import qualified Graphics.Gnuplot.Terminal.SVG as Sv
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Tw
import qualified Graphics.Gnuplot.Graph.ThreeDimensional as Th
import qualified Data.ByteString.Char8 as Char
import           Graphics.Gnuplot.Advanced (plot)
import           Graphics.Gnuplot.Value.Atom (C)
import           IHaskell.Display

-- Plot-types
instance (C x, C y) => IHaskellDisplay (P.T (Tw.T x y)) where
  display fig = do
    pngDisp <- graphDataPNG2P fig
    svgDisp <- graphDataSVG2P fig
    return $ Display [pngDisp, svgDisp]

instance (C x, C y, C z) => IHaskellDisplay (P.T (Th.T x y z)) where
  display fig = do
    pngDisp <- graphDataPNG3P fig
    svgDisp <- graphDataSVG3P fig
    return $ Display [pngDisp, svgDisp]

-- Frame-types
instance (C x, C y) => IHaskellDisplay (F.T (Tw.T x y)) where
  display fig = do
    pngDisp <- graphDataPNG2F fig
    svgDisp <- graphDataSVG2F fig
    return $ Display [pngDisp, svgDisp]

instance (C x, C y, C z) => IHaskellDisplay (F.T (Th.T x y z)) where
  display fig = do
    pngDisp <- graphDataPNG3F fig
    svgDisp <- graphDataSVG3F fig
    return $ Display [pngDisp, svgDisp]

-- Type: Multiplot
instance IHaskellDisplay M.T where
  display fig = do
    pngDisp <- graphDataPNGM fig
    svgDisp <- graphDataSVGM fig
    return $ Display [pngDisp, svgDisp]

-- Filename
name = ".ihaskell-gnuplot."

-- Width and height
w = 300

h = 300

graphDataPNG2P :: (C x, C y) => P.T (Tw.T x y) -> IO DisplayData
graphDataPNG2P graph = do
  switchToTmpDir

  -- Write the image.
  let fname = Pn.cons $ name ++ "png"
  plot fname graph

  -- Read back, and convert to base64.
  imgData <- Char.readFile $ name ++ "png"
  return $ png w h $ base64 imgData

graphDataSVG2P :: (C x, C y) => P.T (Tw.T x y) -> IO DisplayData
graphDataSVG2P graph = do
  switchToTmpDir

  -- Write the image.
  let fname = Sv.cons $ name ++ "svg"
  plot fname graph

  -- Read back, and convert to base64.
  imgData <- Char.readFile $ name ++ "svg"
  return $ svg $ Char.unpack imgData

graphDataPNG2F :: (C x, C y) => F.T (Tw.T x y) -> IO DisplayData
graphDataPNG2F graph = do
  switchToTmpDir

  -- Write the image.
  let fname = Pn.cons $ name ++ "png"
  plot fname graph

  -- Read back, and convert to base64.
  imgData <- Char.readFile $ name ++ "png"
  return $ png w h $ base64 imgData

graphDataSVG2F :: (C x, C y) => F.T (Tw.T x y) -> IO DisplayData
graphDataSVG2F graph = do
  switchToTmpDir

  -- Write the image.
  let fname = Sv.cons $ name ++ "svg"
  plot fname graph

  -- Read back, and convert to base64.
  imgData <- Char.readFile $ name ++ "svg"
  return $ svg $ Char.unpack imgData

graphDataPNG3P :: (C x, C y, C z) => P.T (Th.T x y z) -> IO DisplayData
graphDataPNG3P graph = do
  switchToTmpDir

  -- Write the image.
  let fname = Pn.cons $ name ++ "png"
  plot fname graph

  -- Read back, and convert to base64.
  imgData <- Char.readFile $ name ++ "png"
  return $ png w h $ base64 imgData

graphDataSVG3P :: (C x, C y, C z) => P.T (Th.T x y z) -> IO DisplayData
graphDataSVG3P graph = do
  switchToTmpDir

  -- Write the image.
  let fname = Sv.cons $ name ++ "svg"
  plot fname graph

  -- Read back, and convert to base64.
  imgData <- Char.readFile $ name ++ "svg"
  return $ svg $ Char.unpack imgData

graphDataPNG3F :: (C x, C y, C z) => F.T (Th.T x y z) -> IO DisplayData
graphDataPNG3F graph = do
  switchToTmpDir

  -- Write the image.
  let fname = Pn.cons $ name ++ "png"
  plot fname graph

  -- Read back, and convert to base64.
  imgData <- Char.readFile $ name ++ "png"
  return $ png w h $ base64 imgData

graphDataSVG3F :: (C x, C y, C z) => F.T (Th.T x y z) -> IO DisplayData
graphDataSVG3F graph = do
  switchToTmpDir

  -- Write the image.
  let fname = Sv.cons $ name ++ "svg"
  plot fname graph

  -- Read back, and convert to base64.
  imgData <- Char.readFile $ name ++ "svg"
  return $ svg $ Char.unpack imgData

graphDataPNGM :: M.T -> IO DisplayData
graphDataPNGM graph = do
  switchToTmpDir

  -- Write the image.
  let fname = Pn.cons $ name ++ "png"
  plot fname graph

  -- Read back, and convert to base64.
  imgData <- Char.readFile $ name ++ "png"
  return $ png w h $ base64 imgData

graphDataSVGM :: M.T -> IO DisplayData
graphDataSVGM graph = do
  switchToTmpDir

  -- Write the image.
  let fname = Sv.cons $ name ++ "svg"
  plot fname graph

  -- Read back, and convert to base64.
  imgData <- Char.readFile $ name ++ "svg"
  return $ svg $ Char.unpack imgData
