{-# LANGUAGE OverloadedStrings #-}

module IHaskell.Display.Widgets.Common (ButtonStyle(..), ImageFormat(..)) where

import           Data.Aeson (ToJSON(..))
import qualified Data.Text as T

-- | Pre-defined button-styles
data ButtonStyle = Primary
                 | Success
                 | Info
                 | Warning
                 | Danger
                 | None
  deriving (Eq, Show)

instance ToJSON ButtonStyle where
  toJSON Primary = "primary"
  toJSON Success = "success"
  toJSON Info = "info"
  toJSON Warning = "warning"
  toJSON Danger = "danger"
  toJSON None = ""

-- | Image formats for ImageWidget
data ImageFormat = PNG
                 | SVG
                 | JPG
  deriving Eq

instance Show ImageFormat where
  show PNG = "png"
  show SVG = "svg"
  show JPG = "jpg"

instance ToJSON ImageFormat where
  toJSON = toJSON . T.pack . show
