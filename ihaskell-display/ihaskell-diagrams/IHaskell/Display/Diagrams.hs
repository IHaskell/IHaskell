{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor, DeriveGeneric #-}

module IHaskell.Display.Diagrams
         ( diagram, animation
         , ManuallySized, withSizeSpec, withImgWidth, withImgHeight
         ) where

import qualified Data.ByteString.Char8 as Char
import           System.Directory
import           System.IO.Unsafe
import           Data.IORef
import           Diagrams.Backend.Cairo
import           Diagrams.Prelude
import           IHaskell.Display
import           IHaskell.Display.Diagrams.Animation

import           GHC.Generics (Generic)

data ManuallySized a = ManuallySized
    { contentToDisplay :: a
    , imgManualWidth :: Double
    , imgManualHeight :: Double
    } deriving (Show, Functor, Generic)

instance IHaskellDisplay (ManuallySized (QDiagram Cairo V2 Double Any)) where
  display renderable = do
    png <- diagramData renderable PNG
    svg <- diagramData renderable SVG
    return $ Display [png, svg]

diagramData :: ManuallySized (Diagram Cairo) -> OutputType -> IO DisplayData
diagramData (ManuallySized renderable imgWidth imgHeight) format = do
  switchToTmpDir

  -- Write the image.
  let filename = ".ihaskell-diagram." ++ extension format
  renderCairo filename (mkSizeSpec2D (Just imgWidth)
                                     (Just imgHeight)) renderable

  -- Convert to base64.
  imgData <- Char.readFile filename
  let value =
        case format of
          PNG -> png (floor imgWidth) (floor imgHeight) $ base64 imgData
          SVG -> svg (Char.unpack imgData)

  return value

  where
    extension SVG = "svg"
    extension PNG = "png"

-- Rendering hint.
diagram :: Diagram Cairo -> Diagram Cairo
diagram = id

withSizeSpec :: SizeSpec V2 Double -> Diagram Cairo -> ManuallySized (Diagram Cairo)
withSizeSpec spec renderable = ManuallySized renderable imgWidth imgHeight
 where aspect = width renderable / height renderable
       (imgWidth, imgHeight) = case getSpec spec of
         V2 (Just w) (Just h) -> (w, h)
         V2 (Just w) Nothing  -> (w, w/aspect)
         V2 Nothing (Just h)  -> (aspect*h, h)
         V2 Nothing Nothing   -> (aspect*defaultHeight, defaultHeight)
       defaultHeight = 300

withImgWidth :: Int -> Diagram Cairo -> ManuallySized (Diagram Cairo)
withImgWidth imgWidth = withSizeSpec $ mkSizeSpec2D (Just $ fromIntegral imgWidth)
                                                    Nothing

withImgHeight :: Int -> Diagram Cairo -> ManuallySized (Diagram Cairo)
withImgHeight imgHeight = withSizeSpec $ mkSizeSpec2D Nothing
                                                      (Just $ fromIntegral imgHeight)

instance IHaskellDisplay (QDiagram Cairo V2 Double Any) where
  display = display . withSizeSpec (mkSizeSpec2D Nothing Nothing)
