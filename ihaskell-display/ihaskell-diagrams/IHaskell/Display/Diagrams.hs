{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE TypeFamilies                            #-}

module IHaskell.Display.Diagrams
         ( diagram, animation
         , ManuallySized, withSizeSpec, withImgWidth, withImgHeight
         , ManuallySampled, withAnimFps
         ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char
import qualified Data.Text.Encoding as T.Encoding
import           System.Directory
import           System.IO.Temp
import           System.FilePath ((</>))
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
  -- We should not have to round-trip this ByteString to a temp file.
  -- https://github.com/IHaskell/IHaskell/issues/1248
  withSystemTempDirectory "ihaskell-diagram" $ \tmpdir -> do
    let path = case format of
          SVG -> tmpdir </> "ihaskell-diagram.svg"
          PNG -> tmpdir </> "ihaskell-diagram.png"
          _ -> error "Unreachable case"

    -- Write the image.
    renderCairo path (mkSizeSpec2D (Just imgWidth)
                                   (Just imgHeight)) renderable

    case format of
      PNG -> do
        -- Convert to base64.
        imgData <- Char.readFile path
        pure $ png (floor imgWidth) (floor imgHeight) $ base64 imgData
      SVG -> do
        imgData <- BS.readFile path
        pure $ svg (T.Encoding.decodeUtf8 imgData)
      _ -> error "Unreachable case"

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
