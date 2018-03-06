{-# LANGUAGE DeriveFunctor, DeriveGeneric #-}

module IHaskell.Display.Diagrams.ImgSize where

import           GHC.Generics (Generic)

data ManuallySized a = ManuallySized
    { contentToDisplay :: a
    , imgManualWidth :: Double
    , imgManualHeight :: Double
    } deriving (Show, Functor, Generic)
