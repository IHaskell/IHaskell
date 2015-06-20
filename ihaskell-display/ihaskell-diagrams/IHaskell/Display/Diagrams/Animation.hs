{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module IHaskell.Display.Diagrams.Animation (animation) where

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as CBS

import           Diagrams.Prelude
import           Diagrams.Backend.Cairo
import           Diagrams.Backend.Cairo.CmdLine (GifOpts(..))
import           Diagrams.Backend.CmdLine (DiagramOpts(..), mainRender)

import           IHaskell.Display

instance IHaskellDisplay (QAnimation Cairo V2 Double Any) where
  display renderable = do
    gif <- animationData renderable
    return $ Display [html $ "<img src=\"data:image/gif;base64,"
                             ++ gif ++ "\" />"]

animationData :: Animation Cairo V2 Double -> IO String
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
      diagOpts = DiagramOpts
        { _width = Just . ceiling $ imgWidth
        , _height = Just . ceiling $ imgHeight
        , _output = filename
        }
      gifOpts = GifOpts { _dither = True, _noLooping = False, _loopRepeat = Nothing }
  mainRender (diagOpts, gifOpts) frameSet

  -- Convert to ascii represented base64 encoding
  imgData <- CBS.readFile filename
  return . T.unpack . base64 $ imgData

-- Rendering hint.
animation :: Animation Cairo V2 Double -> Animation Cairo V2 Double
animation = id
