{-# LANGUAGE OverloadedStrings #-}

module IHaskell.Display.Widgets.String.Text (
    -- * The Text Widget
    TextWidget,
    -- * Constructor
    mkTextWidget,
    -- * Set properties
    setTextValue,
    -- * Get properties
    getTextValue,
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

data TextWidget =
       TextWidget
         { uuid :: U.UUID
         , value :: IORef String
         }

-- | Create a new Text widget
mkTextWidget :: IO TextWidget
mkTextWidget = do
  -- Default properties, with a random uuid
  commUUID <- U.random
  val <- newIORef ""

  let b = TextWidget
            { uuid = commUUID
            , value = val
            }

  let initData = object [ "model_name" .= str "WidgetModel"
                        , "widget_class" .= str "IPython.Text"
                        ]

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen b initData (toJSON b)

  -- Return the string widget
  return b

-- | Send an update msg for a widget, with custom json. Make it easy to update fragments of the
-- state, by accepting a Pair instead of a Value.
update :: TextWidget -> [Pair] -> IO ()
update b v = widgetSendUpdate b . toJSON . object $ v

-- | Modify attributes stored inside the widget as IORefs
modify :: TextWidget -> (TextWidget -> IORef a) -> a -> IO ()
modify b attr val = writeIORef (attr b) val

-- | Set the Text string value.
setTextValue :: TextWidget -> String -> IO ()
setTextValue b txt = do
  modify b value txt
  update b ["value" .= txt]

-- | Get the Text string value.
getTextValue :: TextWidget -> IO String
getTextValue = readIORef . value

instance ToJSON TextWidget where
  toJSON b = object
               [ "_view_name" .= str "TextView"
               , "visible" .= True
               , "_css" .= object []
               , "msg_throttle" .= (3 :: Int)
               , "value" .= get value b
               ]
    where
      get x y = unsafePerformIO . readIORef . x $ y

instance IHaskellDisplay TextWidget where
  display b = do
    widgetSendView b
    return $ Display []

instance IHaskellWidget TextWidget where
  getCommUUID = uuid

str :: String -> String
str = id
