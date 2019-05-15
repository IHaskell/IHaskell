{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE TypeFamilies                            #-}

module IHaskell.Display.Diagrams
         ( diagram, animation
         , ManuallySized, withSizeSpec, withImgWidth, withImgHeight
         , ManuallySampled, withAnimFps
         ) where

import qualified Data.ByteString.Char8 as Char
import           System.Directory
import           Diagrams.Backend.Cairo
import           Diagrams.Prelude
import           IHaskell.Display
import           IHaskell.Display.Diagrams.Animation
import           IHaskell.Display.Diagrams.ImgSize

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

instance (b ~ Cairo, v ~ V2, s ~ Double, m ~ Any)
              => ManuallySizeable (QDiagram a v s m) where
  withSizeSpec spec renderable = ManuallySized renderable imgWidth imgHeight
    where
       aspect = width renderable / height renderable
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

instance IHaskellDisplay (QDiagram Cairo V2 Double Any) where
  display = display . withSizeSpec (mkSizeSpec2D Nothing Nothing)
