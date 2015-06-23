{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module IHaskell.Display.Widgets.Image (
    -- * The Image Widget
    ImageWidget,
    -- * Create a new image widget
    mkImageWidget,
    -- * Set image properties
    setImageFormat,
    setImageB64Value,
    setImageWidth,
    setImageHeight,
    -- * Get image properties
    getImageFormat,
    getImageB64Value,
    getImageWidth,
    getImageHeight,
    ) where

-- To keep `cabal repl` happy when running from the ihaskell repo
import           Prelude

import           Control.Monad (when)
import           Data.Aeson (ToJSON, Value(..), object, toJSON, (.=))
import           Data.Aeson.Types (Pair)
import           Data.HashMap.Strict as Map
import           Data.IORef
import           Data.Text (Text)
import qualified Data.Text as T
import           System.IO.Unsafe (unsafePerformIO)

import           IHaskell.Display
import           IHaskell.Eval.Widgets
import qualified IHaskell.IPython.Message.UUID as U
import           IHaskell.Types (WidgetMethod(..))

import           IHaskell.Display.Widgets.Common

-- | A 'Image' represents a Image from IPython.html.widgets.
data ImageWidget =
       ImageWidget
         { uuid :: U.UUID
         , format :: IORef ImageFormat
         , height :: IORef ImageInt
         , width :: IORef ImageInt
         , b64value :: IORef Base64
         }

newtype ImageInt = ImageInt { unwrap :: Int }

instance ToJSON ImageInt where
  toJSON (ImageInt n) | n > 0 = toJSON $ str $ show n
                      | otherwise = toJSON $ str $ ""

-- | Create a new image widget
mkImageWidget :: IO ImageWidget
mkImageWidget = do
  -- Default properties, with a random uuid
  commUUID <- U.random
  fmt <- newIORef PNG
  hgt <- newIORef (ImageInt 0)
  wdt <- newIORef (ImageInt 0)
  val <- newIORef ""

  let initData = object ["model_name" .= str "WidgetModel", "widget_class" .= str "IPython.Image"]
      b = ImageWidget
        { uuid = commUUID
        , format = fmt
        , height = hgt
        , width = wdt
        , b64value = val
        }

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen b initData (toJSON b)

  -- Return the image widget
  return b

-- | Send an update msg for a image, with custom json. Make it easy to update fragments of the
-- state, by accepting a Pair instead of a Value.
update :: ImageWidget -> [Pair] -> IO ()
update b v = widgetSendUpdate b . toJSON . object $ v

-- | Modify attributes of a image, stored inside it as IORefs
modify :: ImageWidget -> (ImageWidget -> IORef a) -> a -> IO ()
modify b attr val = writeIORef (attr b) val

-- | Set the image style
setImageFormat :: ImageWidget -> ImageFormat -> IO ()
setImageFormat b fmt = do
  modify b format fmt
  update b ["format" .= fmt]

-- | Set the image value (encoded in base64)
setImageB64Value :: ImageWidget -> Base64 -> IO ()
setImageB64Value b val = do
  modify b b64value val
  update b ["_b64value" .= val]

-- | Set the image width
setImageWidth :: ImageWidget -> Int -> IO ()
setImageWidth b wdt = do
  let w = ImageInt wdt
  modify b width w
  update b ["width" .= w]

-- | Set the image height
setImageHeight :: ImageWidget -> Int -> IO ()
setImageHeight b hgt = do
  let h = ImageInt hgt
  modify b height h
  update b ["height" .= h]

-- | Get the image format
getImageFormat :: ImageWidget -> IO ImageFormat
getImageFormat = readIORef . format

-- | Get the image value (encoded in base64)
getImageB64Value :: ImageWidget -> IO Base64
getImageB64Value = readIORef . b64value

-- | Get the image width
getImageWidth :: ImageWidget -> IO Int
getImageWidth = fmap unwrap . readIORef . width

-- | Get the image height
getImageHeight :: ImageWidget -> IO Int
getImageHeight = fmap unwrap . readIORef . height

instance ToJSON ImageWidget where
  toJSON b = object
               [ "_view_module" .= str ""
               , "background_color" .= str ""
               , "border_width" .= str ""
               , "border_color" .= str ""
               , "width" .= get width b
               , "_dom_classes" .= object []
               , "margin" .= str ""
               , "font_style" .= str ""
               , "font_weight" .= str ""
               , "height" .= get height b
               , "font_size" .= str ""
               , "border_style" .= str ""
               , "padding" .= str ""
               , "border_radius" .= str ""
               , "version" .= (0 :: Int)
               , "font_family" .= str ""
               , "color" .= str ""
               , "_view_name" .= str "ImageView"
               , "visible" .= True
               , "_css" .= object []
               , "msg_throttle" .= (3 :: Int)
               , "format" .= get format b
               , "_b64value" .= get b64value b
               ]
    where
      get x y = unsafePerformIO . readIORef . x $ y

instance IHaskellDisplay ImageWidget where
  display b = do
    widgetSendView b
    return $ Display []

instance IHaskellWidget ImageWidget where
  getCommUUID = uuid

str :: String -> String
str = id
