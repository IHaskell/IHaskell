{-# LANGUAGE OverloadedStrings #-}

module IHaskell.Display.Widgets.String.Latex (
    -- * The Latex Widget
    LatexWidget,
    -- * Constructor
    mkLatexWidget,
    -- * Set properties
    setLatexValue,
    setLatexWidth,
    -- * Get properties
    getLatexValue,
    getLatexWidth,
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

data LatexWidget =
       LatexWidget
         { uuid :: U.UUID
         , value :: IORef String
         , width :: IORef Int
         }

-- | Create a new Latex widget
mkLatexWidget :: IO LatexWidget
mkLatexWidget = do
  -- Default properties, with a random uuid
  commUUID <- U.random
  val <- newIORef ""
  width <- newIORef 400

  let b = LatexWidget
        { uuid = commUUID
        , value = val
        , width = width
        }

  let initData = object [ "model_name" .= str "WidgetModel"
                        , "widget_class" .= str "IPython.Latex"
                        ]

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen b initData (toJSON b)

  -- Return the string widget
  return b

-- | Send an update msg for a widget, with custom json. Make it easy to update fragments of the
-- state, by accepting a Pair instead of a Value.
update :: LatexWidget -> [Pair] -> IO ()
update b v = widgetSendUpdate b . toJSON . object $ v

-- | Modify attributes stored inside the widget as IORefs
modify :: LatexWidget -> (LatexWidget -> IORef a) -> a -> IO ()
modify b attr val = writeIORef (attr b) val

-- | Set the Latex string value.
setLatexValue :: LatexWidget -> String -> IO ()
setLatexValue b txt = do
  modify b value txt
  update b ["value" .= txt]

-- | Set the Latex widget width.
setLatexWidth :: LatexWidget -> Int -> IO ()
setLatexWidth b wid = do
  modify b width wid
  update b ["width" .= wid]

-- | Get the Latex string value.
getLatexValue :: LatexWidget -> IO String
getLatexValue = readIORef . value

-- | Get the Latex widget width.
getLatexWidth :: LatexWidget -> IO Int
getLatexWidth = readIORef . width

instance ToJSON LatexWidget where
  toJSON b = object
               [ "_view_name" .= str "LatexView"
               , "visible" .= True
               , "_css" .= object []
               , "msg_throttle" .= (3 :: Int)
               , "value" .= get value b
               ]
    where
      get x y = unsafePerformIO . readIORef . x $ y

instance IHaskellDisplay LatexWidget where
  display b = do
    widgetSendView b
    return $ Display []

instance IHaskellWidget LatexWidget where
  getCommUUID = uuid

str :: String -> String
str = id
