{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module IHaskell.Display.Widgets.String (
    -- * The String Widgets
    HTMLWidget,
    LatexWidget,
    TextWidget,
    TextAreaWidget,
    -- * Create a new button
    mkButton,
    -- * Set button properties
    setStrWidgetButtonStyle,
    setStrWidgetText,
    -- * Get button properties
    getStrWidgetButtonStyle,
    getStrWidgetText,
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

data ViewName = HTMLView | LatexView | TextView | TextareaView

data StringWidget =
       StringWidget
         { uuid :: U.UUID
         , strWidgetType :: StrWidgetType
         , value :: IORef String
         , description :: IORef Text
         , disabled :: IORef Bool
         , placeholder :: IORef String
         , buttonStyle :: IORef ButtonStyle
         }

-- | Create a new string widget
mkStringWidget :: StrWidgetType -> IO StringWidget
mkStringWidget widgetType = do
  -- Default properties, with a random uuid
  commUUID <- U.random
  wType <- newIORef widgetType
  val <- newIORef ""
  desc <- newIORef ""
  dis <- newIORef False
  placeholder <- newIORef "Enter your text here..."
  bst <- newIORef None

  let b = StringWidget
        { uuid = commUUID
        , strWidgetType = wType
        , value = val
        , description = desc
        , disabled = dis
        , buttonStyle = bst
        }

  let initData = object [ "model_name" .= str "WidgetModel"
                        , "widget_class" .= getViewName widgetType
                        ]

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen b initData (toJSON b)

  -- Return the string widget
  return b

-- | Send an update msg for a widget, with custom json. Make it easy to update fragments of the
-- state, by accepting a Pair instead of a Value.
update :: StringWidget -> [Pair] -> IO ()
update b v = widgetSendUpdate b . toJSON . object $ v

-- | Modify attributes of a widget, stored inside it as IORefs
modify :: StringWidget -> (StringWidget -> IORef a) -> a -> IO ()
modify b attr val = writeIORef (attr b) val

-- | Set the button style
setStrWidgetButtonStyle :: StringWidget -> ButtonStyle -> IO ()
setStrWidgetButtonStyle b bst = do
  modify b buttonStyle bst
  update b ["button_style" .= bst]

-- | Set the widget text
setStrWidgetText :: StringWidget -> Text -> IO ()
setStrWidgetText b txt = do
  modify b description txt
  update b ["description" .= txt]

-- | Get the button style
getStrWidgetButtonStyle :: Button -> IO ButtonStyle
getStrWidgetButtonStyle = readIORef . buttonStyle

-- | Get the widget text
getStrWidgetText :: Button -> IO Text
getStrWidgetText = readIORef . description

instance ToJSON StringWidget where
  toJSON StringWidget {wType = strWidgetType} =
    object
      [ "_view_name" .= toJSON . getViewName $ wType
      , "visible" .= True
      , "_css" .= object []
      , "msg_throttle" .= (3 :: Int)
      , "disabled" .= get disabled b
      , "description" .= get description b
      , "tooltip" .= get tooltip b
      , "button_style" .= get buttonStyle b
      ]
    where
      get x y = unsafePerformIO . readIORef . x $ y

instance IHaskellDisplay StringWidget where
  display b = do
    widgetSendView b
    return $ Display []

instance IHaskellWidget Button where
  getCommUUID = uuid
  comm widget (Object dict1) _ = do
    let key1 = "content" :: Text
        key2 = "event" :: Text
        Just (Object dict2) = Map.lookup key1 dict1
        Just (String event) = Map.lookup key2 dict2
    when (event == "click") $ triggerClick widget

str :: String -> String
str = id
