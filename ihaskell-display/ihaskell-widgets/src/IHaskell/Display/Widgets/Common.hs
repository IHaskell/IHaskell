{-# LANGUAGE OverloadedStrings #-}

module IHaskell.Display.Widgets.Common (
    -- * Predefined button styles
    ButtonStyle(..),
    ) where

import Data.Aeson (ToJSON (..))

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
