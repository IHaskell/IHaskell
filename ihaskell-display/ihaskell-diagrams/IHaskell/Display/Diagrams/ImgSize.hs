{-# LANGUAGE DeriveFunctor, DeriveGeneric #-}

module IHaskell.Display.Diagrams.ImgSize where

import           GHC.Generics (Generic)
import           Diagrams.Prelude (SizeSpec, mkSizeSpec2D, V2)

data ManuallySized a = ManuallySized
    { contentToDisplay :: a
    , imgManualWidth :: Double
    , imgManualHeight :: Double
    } deriving (Show, Functor, Generic)

class ManuallySizeable a where
  withSizeSpec :: SizeSpec V2 Double -> a -> ManuallySized a

withImgWidth :: ManuallySizeable a => Int -> a -> ManuallySized a
withImgWidth imgWidth = withSizeSpec $ mkSizeSpec2D (Just $ fromIntegral imgWidth)
                                                    Nothing

withImgHeight :: ManuallySizeable a => Int -> a -> ManuallySized a
withImgHeight imgHeight = withSizeSpec $ mkSizeSpec2D Nothing
                                                      (Just $ fromIntegral imgHeight)

