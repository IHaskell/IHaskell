{-# LANGUAGE OverloadedStrings #-}

module IHaskell.Display.Widgets.Common (
    -- * Convenience types
    ButtonStyle(..),
    ImageFormat(..),
    PosInt(..),
    -- * Convenience functions (for internal use)
    update,
    modify,
    str,
    ) where

import           Data.Aeson hiding (Success)
import           Data.Aeson.Types (Pair)
import qualified Data.Text as T
import           Data.IORef

import           IHaskell.Display
import           IHaskell.Eval.Widgets

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

-- | A wrapper around Int. 'toJSON' gives the no. for positive numbers, and empty string otherwise
newtype PosInt = PosInt { unwrap :: Int }

instance ToJSON PosInt where
  toJSON (PosInt n)
    | n > 0 = toJSON $ str $ show n
    | otherwise = toJSON $ str $ ""

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

-- | Send an update msg for a widget, with custom json. Make it easy to update fragments of the
-- state, by accepting a Pair instead of a Value.
update :: IHaskellWidget a => a -> [Pair] -> IO ()
update widget = widgetSendUpdate widget . toJSON . object

-- | Modify attributes of a widget, stored inside it as IORefs
modify :: IHaskellWidget a => a -> (a -> IORef b) -> b -> IO ()
modify widget attr newval = writeIORef (attr widget) newval

-- | Useful with toJSON
str :: String -> String
str = id
