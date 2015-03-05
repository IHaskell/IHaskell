{-# LANGUAGE NoImplicitPrelude, TypeSynonymInstances, FlexibleInstances  #-}
module IHaskell.Display.Diagrams.Animation (animation) where

import ClassyPrelude hiding (filename)

import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.CmdLine (GifOpts (..))
import Diagrams.Backend.CmdLine (DiagramOpts (..), mainRender)

import IHaskell.Display

instance IHaskellDisplay (QAnimation Cairo R2 Any) where
  display renderable = do
    gif <- animationData renderable
    return $ Display [html $ "<img src=\"data:image/gif;base64,"
                               ++ gif ++ "\" />"]

animationData :: Animation Cairo R2 -> IO String
animationData renderable = do
  switchToTmpDir

  -- Generate the frames
  let fps = 30
      animAdjusted = animEnvelope' fps renderable
      frames = simulate fps animAdjusted
      timediff = 100 `div` ceiling fps :: Int
      frameSet = map (\x -> (x # bg white, timediff)) frames

  -- Compute width and height.
  let shape = activeStart animAdjusted
      w = width shape
      h = height shape
      aspect = w / h
      imgHeight = 300
      imgWidth = aspect * imgHeight

  -- Write the image.
  let filename = ".ihaskell-diagram.gif"
      diagOpts = DiagramOpts {
                             _width = Just . ceiling $ imgWidth
                           , _height = Just . ceiling $ imgHeight
                           , _output = filename
                           }
      gifOpts = GifOpts {
                  _dither = True
                , _noLooping = False
                , _loopRepeat = Nothing
                }
  mainRender (diagOpts, gifOpts) frameSet

  -- Convert to ascii represented base64 encoding
  imgData <- readFile $ fpFromString filename
  return . unpack . base64 $ imgData

-- Rendering hint.
animation :: Animation Cairo R2 -> Animation Cairo R2
animation = id
