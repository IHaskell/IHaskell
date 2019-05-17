{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE TypeFamilies                            #-}
{-# LANGUAGE DeriveFunctor, DeriveGeneric #-}

module IHaskell.Display.Diagrams.Animation
         ( animation
         , ManuallySampled, withAnimFps
         ) where

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as CBS

import           GHC.Generics (Generic)
import           Data.Maybe (fromMaybe)
import           Diagrams.Prelude
import           Diagrams.Backend.Cairo
import           Diagrams.Backend.Cairo.CmdLine (GifOpts(..))
import           Diagrams.Backend.CmdLine (DiagramOpts(..), mainRender)

import           IHaskell.Display
import           IHaskell.Display.Diagrams.ImgSize


data ManuallySampled a = ManuallySampled
    { contentToSample :: a
    , signalManualSampleRate :: Maybe Rational
    } deriving (Show, Functor, Generic)

class ManuallySamplable a where
  withSamplingSpec :: Maybe Rational -> a -> ManuallySampled a


defaultFps = 30

withAnimFps :: ManuallySamplable a => Rational -> a -> ManuallySampled a
withAnimFps fps = withSamplingSpec (Just fps)


instance IHaskellDisplay (ManuallySized (ManuallySampled (QAnimation Cairo V2 Double Any))) where
  display renderable = do
    gif <- animationData renderable
    return $ Display [html $ "<img src=\"data:image/gif;base64,"
                             ++ gif ++ "\" />"]


animationData :: ManuallySized (ManuallySampled (Animation Cairo V2 Double)) -> IO String
animationData (ManuallySized (ManuallySampled renderable fps) imgWidth imgHeight) = do
  switchToTmpDir

  -- Generate the frames
  let actualFps = fromMaybe defaultFps fps
      animAdjusted = animEnvelope' actualFps renderable
      frames = simulate actualFps animAdjusted
      timediff = 100 `div` ceiling actualFps :: Int
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


getImgSize renderable sizeSpec fps = out
  where
    actualFps = fromMaybe defaultFps fps
    shape = activeStart $ animEnvelope' actualFps renderable
    aspect = width shape / height shape
    out = case getSpec sizeSpec of
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


instance (b ~ Cairo, v ~ V2, s ~ Double, m ~ Any)
              => ManuallySamplable (QAnimation b v s m) where
  withSamplingSpec fps renderable = ManuallySampled renderable fps

instance (b ~ Cairo, v ~ V2, s ~ Double, m ~ Any)
              => ManuallySamplable (ManuallySized (QAnimation b v s m)) where
  withSamplingSpec fps sizedRenderable = ManuallySampled sizedRenderable fps

instance (b ~ Cairo, v ~ V2, s ~ Double, m ~ Any)
              => ManuallySizeable (QAnimation b v s m) where
  withSizeSpec spec renderable = ManuallySized renderable imgWidth imgHeight
    where
      fps = Nothing
      V2 imgWidth imgHeight = getImgSize renderable spec fps

instance (b ~ Cairo, v ~ V2, s ~ Double, m ~ Any)
              => ManuallySizeable (ManuallySampled (QAnimation b v s m)) where
  withSizeSpec spec (ManuallySampled renderable fps) = out
    where
      out = ManuallySized (ManuallySampled renderable fps) w h
      V2 w h = getImgSize renderable spec fps


instance IHaskellDisplay (QAnimation Cairo V2 Double Any) where
  display = display . withSizeSpec (mkSizeSpec2D Nothing Nothing) . withSamplingSpec fps
    where
      fps = Nothing

instance IHaskellDisplay (ManuallySized (QAnimation Cairo V2 Double Any)) where
  display (ManuallySized renderable w h) = out
    where
      fps = Nothing
      sizeSpec = mkSizeSpec2D (Just w) (Just h)
      out = display . withSizeSpec sizeSpec $ withSamplingSpec fps renderable

instance IHaskellDisplay (ManuallySampled (QAnimation Cairo V2 Double Any)) where
  display = display . withSizeSpec (mkSizeSpec2D Nothing Nothing)

instance IHaskellDisplay (ManuallySampled (ManuallySized (QAnimation Cairo V2 Double Any))) where
  display (ManuallySampled (ManuallySized renderable w h) fps) = out
    where
      sizeSpec = mkSizeSpec2D (Just w) (Just h)
      out = display . withSizeSpec sizeSpec $ withSamplingSpec fps renderable