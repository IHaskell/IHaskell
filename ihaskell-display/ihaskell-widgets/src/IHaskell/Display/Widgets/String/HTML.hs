{-# LANGUAGE OverloadedStrings #-}

module IHaskell.Display.Widgets.String.HTML (
    -- * The HTML Widget
    HTMLWidget,
    -- * Constructor
    mkHTMLWidget,
    -- * Set properties
    setHTMLValue,
    setHTMLDescription,
    setHTMLPlaceholder,
    -- * Get properties
    getHTMLValue,
    getHTMLDescription,
    getHTMLPlaceholder,
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

import           IHaskell.Display.Widgets.Common

data HTMLWidget =
       HTMLWidget
         { uuid :: U.UUID
         , value :: IORef Text
         , description :: IORef Text
         , placeholder :: IORef Text
         }

-- | Create a new HTML widget
mkHTMLWidget :: IO HTMLWidget
mkHTMLWidget = do
  -- Default properties, with a random uuid
  commUUID <- U.random
  val <- newIORef ""
  des <- newIORef ""
  plc <- newIORef ""

  let b = HTMLWidget { uuid = commUUID, value = val, description = des, placeholder = plc }

  let initData = object ["model_name" .= str "WidgetModel", "widget_class" .= str "IPython.HTML"]

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen b initData (toJSON b)

  -- Return the string widget
  return b

-- | Set the HTML string value.
setHTMLValue :: HTMLWidget -> Text -> IO ()
setHTMLValue b txt = do
  modify b value txt
  update b ["value" .= txt]

-- | Set the HTML description
setHTMLDescription :: HTMLWidget -> Text -> IO ()
setHTMLDescription b txt = do
  modify b description txt
  update b ["description" .= txt]

-- | Set the HTML placeholder, i.e. text displayed in empty widget
setHTMLPlaceholder :: HTMLWidget -> Text -> IO ()
setHTMLPlaceholder b txt = do
  modify b placeholder txt
  update b ["placeholder" .= txt]

-- | Get the HTML string value.
getHTMLValue :: HTMLWidget -> IO Text
getHTMLValue = readIORef . value

-- | Get the HTML description value.
getHTMLDescription :: HTMLWidget -> IO Text
getHTMLDescription = readIORef . description

-- | Get the HTML placeholder value.
getHTMLPlaceholder :: HTMLWidget -> IO Text
getHTMLPlaceholder = readIORef . placeholder

instance ToJSON HTMLWidget where
  toJSON b = object
               [ "_view_name" .= str "HTMLView"
               , "visible" .= True
               , "_css" .= object []
               , "msg_throttle" .= (3 :: Int)
               , "value" .= get value b
               ]
    where
      get x y = unsafePerformIO . readIORef . x $ y

instance IHaskellDisplay HTMLWidget where
  display b = do
    widgetSendView b
    return $ Display []

instance IHaskellWidget HTMLWidget where
  getCommUUID = uuid
