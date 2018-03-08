{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE TypeFamilies                            #-}

module IHaskell.Display.Diagrams.Animation (animation) where

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as CBS

import           Diagrams.Prelude
import           Diagrams.Backend.Cairo
import           Diagrams.Backend.Cairo.CmdLine (GifOpts(..))
import           Diagrams.Backend.CmdLine (DiagramOpts(..), mainRender)

import           IHaskell.Display
import           IHaskell.Display.Diagrams.ImgSize

instance IHaskellDisplay (ManuallySized (QAnimation Cairo V2 Double Any)) where
  display renderable = do
    gif <- animationData renderable
    return $ Display [html $ "<img src=\"data:image/gif;base64,"
                             ++ gif ++ "\" />"]

animationData :: ManuallySized (Animation Cairo V2 Double) -> IO String
animationData (ManuallySized renderable imgWidth imgHeight) = do
  switchToTmpDir

  -- Generate the frames
  let fps = 30
      animAdjusted = animEnvelope' fps renderable
      frames = simulate fps animAdjusted
      timediff = 100 `div` ceiling fps :: Int
      frameSet = map (\x -> (x # bg white, timediff)) frames

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

instance (b ~ Cairo, v ~ V2, s ~ Double, m ~ Any)
              => ManuallySizeable (QAnimation b v s m) where
  withSizeSpec spec renderable = ManuallySized renderable imgWidth imgHeight
    where
       fps = 30
       shape = activeStart $ animEnvelope' fps renderable
       aspect = width shape / height shape
       V2 imgWidth imgHeight = case getSpec spec of
         V2 (Just w) (Just h) -> V2 w h
         V2 (Just w) Nothing  -> V2 w (w/aspect)
         V2 Nothing (Just h)  -> V2 (aspect*h) h
         V2 Nothing Nothing   -> (defaultDiagonal / sqrt (1 + aspect^2))
                                    *^ V2 aspect 1
                                 -- w^2 + h^2 = defaultDiagonal^2 / (1+aspect^2)
                                 --                * (aspect^2 + 1)
                                 --           = defaultDiagonal^2
                                 -- w/h = aspect/1 = aspect
       defaultDiagonal = 500


instance IHaskellDisplay (QAnimation Cairo V2 Double Any) where
  display = display . withSizeSpec (mkSizeSpec2D Nothing Nothing)
