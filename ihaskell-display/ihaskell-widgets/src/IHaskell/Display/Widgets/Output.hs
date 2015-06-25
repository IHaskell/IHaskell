{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module IHaskell.Display.Widgets.Output (
    -- * The Output  Widget
    OutputWidget,
    -- * Constructor
    mkOutputWidget,
    -- * Get/Set/Modify width
    getOutputWidth,
    setOutputWidth,
    modifyOutputWidth,
    modifyOutputWidth_,
    -- * Output to widget
    setOutput,
    ) where

-- To keep `cabal repl` happy when running from the ihaskell repo
import           Prelude

import           Control.Monad (when, void)
import           Data.Aeson (ToJSON, Value(..), object, toJSON, (.=))
import           Data.Aeson.Types (Pair, Array)
import qualified Data.HashMap.Strict as Map
import           Data.IORef
import           Data.Text (Text)
import qualified Data.Text as T
import           System.IO.Unsafe (unsafePerformIO)

import           IHaskell.Display
import           IHaskell.Eval.Widgets
import qualified IHaskell.IPython.Message.UUID as U
import           IHaskell.Types (WidgetMethod(..))

import           IHaskell.Display.Widgets.Common

data OutputWidget = OutputWidget { uuid :: U.UUID
                                 , width :: IORef PosInt
                                 }

mkOutputWidget :: IO OutputWidget
mkOutputWidget = do
  -- Default properties, with a random uuid
  commUUID <- U.random
  wdt <- newIORef $ PosInt 400
  dis <- newIORef False

  let b = OutputWidget { uuid = commUUID, width = wdt }

  let initData = object [ "model_name" .= str "WidgetModel" ]

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen b initData (toJSON b)

  -- Return the widget
  return b

-- | Get the output widget width
getOutputWidth :: OutputWidget -> IO Int
getOutputWidth = fmap unwrap . readIORef . width

-- | Set the output widget width
setOutputWidth :: OutputWidget -> Int -> IO ()
setOutputWidth widget widthInt = do
  let w = PosInt widthInt
  modify widget width w
  update widget ["width" .= w]

-- | Modify the output widget width (with IO)
modifyOutputWidth :: OutputWidget -> (Int -> IO Int) -> IO ()
modifyOutputWidth widget modifier = getOutputWidth widget >>= modifier >>= setOutputWidth widget

-- | Modify the output widget width (with pure modifier)
modifyOutputWidth_ :: OutputWidget -> (Int -> Int) -> IO ()
modifyOutputWidth_ widget modifier = getOutputWidth widget >>= setOutputWidth widget . modifier

setOutput :: IHaskellDisplay a => OutputWidget -> a -> IO ()
setOutput widget out = do
  disp <- display out
  widgetPublishDisplay widget disp

instance ToJSON OutputWidget where
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
               , "height" .= str ""
               , "font_size" .= str ""
               , "border_style" .= str ""
               , "padding" .= str ""
               , "border_radius" .= str ""
               , "version" .= (0 :: Int)
               , "font_family" .= str ""
               , "color" .= str ""
               , "_view_name" .= str "OutputView"
               , "visible" .= True
               , "_css" .= object []
               , "msg_throttle" .= (3 :: Int)
               ]
    where
      get x = unsafePerformIO . readIORef . x

instance IHaskellDisplay OutputWidget where
  display b = do
    widgetSendView b
    return $ Display []

instance IHaskellWidget OutputWidget where
  getCommUUID = uuid
