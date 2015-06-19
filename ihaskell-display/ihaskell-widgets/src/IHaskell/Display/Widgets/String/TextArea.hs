{-# LANGUAGE OverloadedStrings #-}

module IHaskell.Display.Widgets.String.TextArea (
    -- * The TextArea Widget
    TextAreaWidget,
    -- * Constructor
    mkTextAreaWidget,
    -- * Set properties
    setTextAreaValue,
    setTextAreaDescription,
    setTextAreaPlaceholder,
    -- * Get properties
    getTextAreaValue,
    getTextAreaDescription,
    getTextAreaPlaceholder,
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

import           IHaskell.Display.Widgets.Common (ButtonStyle (..))

data TextAreaWidget =
       TextAreaWidget
         { uuid :: U.UUID
         , value :: IORef Text
         , description :: IORef Text
         , placeholder :: IORef Text
         }

-- | Create a new TextArea widget
mkTextAreaWidget :: IO TextAreaWidget
mkTextAreaWidget = do
  -- Default properties, with a random uuid
  commUUID <- U.random
  val <- newIORef ""
  des <- newIORef ""
  plc <- newIORef ""

  let b = TextAreaWidget
            { uuid = commUUID
            , value = val
            , description = des
            , placeholder = plc
            }

  let initData = object [ "model_name" .= str "WidgetModel"
                        , "widget_class" .= str "IPython.Textarea"
                        ]

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen b initData (toJSON b)

  -- Return the string widget
  return b

-- | Send an update msg for a widget, with custom json. Make it easy to update fragments of the
-- state, by accepting a Pair instead of a Value.
update :: TextAreaWidget -> [Pair] -> IO ()
update b v = widgetSendUpdate b . toJSON . object $ v

-- | Modify attributes stored inside the widget as IORefs
modify :: TextAreaWidget -> (TextAreaWidget -> IORef a) -> a -> IO ()
modify b attr val = writeIORef (attr b) val

-- | Set the TextArea string value.
setTextAreaValue :: TextAreaWidget -> Text -> IO ()
setTextAreaValue b txt = do
  modify b value txt
  update b ["value" .= txt]

-- | Set the TextArea widget "description"
setTextAreaDescription :: TextAreaWidget -> Text -> IO ()
setTextAreaDescription b txt = do
  modify b description txt
  update b ["description" .= txt]

-- | Set the TextArea widget "placeholder", i.e. text displayed in empty widget
setTextAreaPlaceholder :: TextAreaWidget -> Text -> IO ()
setTextAreaPlaceholder b txt = do
  modify b placeholder txt
  update b ["placeholder" .= txt]

-- | Get the TextArea string value.
getTextAreaValue :: TextAreaWidget -> IO Text
getTextAreaValue = readIORef . value

-- | Get the TextArea widget "description" value.
getTextAreaDescription :: TextAreaWidget -> IO Text
getTextAreaDescription = readIORef . description

-- | Get the TextArea widget placeholder value.
getTextAreaPlaceholder :: TextAreaWidget -> IO Text
getTextAreaPlaceholder = readIORef . placeholder

instance ToJSON TextAreaWidget where
  toJSON b = object
               [ "_view_name" .= str "TextareaView"
               , "visible" .= True
               , "_css" .= object []
               , "msg_throttle" .= (3 :: Int)
               , "value" .= get value b
               , "description" .= get description b
               , "placeholder" .= get placeholder b
               ]
    where
      get x y = unsafePerformIO . readIORef . x $ y

instance IHaskellDisplay TextAreaWidget where
  display b = do
    widgetSendView b
    return $ Display []

instance IHaskellWidget TextAreaWidget where
  getCommUUID = uuid

str :: String -> String
str = id
