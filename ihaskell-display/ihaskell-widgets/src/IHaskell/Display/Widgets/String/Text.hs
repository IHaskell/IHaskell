{-# LANGUAGE OverloadedStrings #-}

module IHaskell.Display.Widgets.String.Text (
    -- * The Text Widget
    TextWidget,
    -- * Constructor
    mkTextWidget,
    -- * Set properties
    setTextValue,
    setTextDescription,
    setTextPlaceholder,
    -- * Get properties
    getTextValue,
    getTextDescription,
    getTextPlaceholder,
    -- * Submit handling
    setSubmitHandler,
    getSubmitHandler,
    triggerSubmit,
    ) where

-- To keep `cabal repl` happy when running from the ihaskell repo
import           Prelude

import           Control.Monad (when, void)
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

import           IHaskell.Display.Widgets.Common (ButtonStyle(..))

data TextWidget =
       TextWidget
         { uuid :: U.UUID
         , value :: IORef Text
         , description :: IORef Text
         , placeholder :: IORef Text
         , submitHandler :: IORef (TextWidget -> IO ())
         }

-- | Create a new Text widget
mkTextWidget :: IO TextWidget
mkTextWidget = do
  -- Default properties, with a random uuid
  commUUID <- U.random
  val <- newIORef ""
  des <- newIORef ""
  plc <- newIORef ""
  sh <- newIORef $ const $ return ()

  let b = TextWidget
        { uuid = commUUID
        , value = val
        , description = des
        , placeholder = plc
        , submitHandler = sh
        }

  let initData = object ["model_name" .= str "WidgetModel", "widget_class" .= str "IPython.Text"]

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
setTextValue :: TextWidget -> Text -> IO ()
setTextValue b txt = do
  modify b value txt
  update b ["value" .= txt]

-- | Set the text widget "description"
setTextDescription :: TextWidget -> Text -> IO ()
setTextDescription b txt = do
  modify b description txt
  update b ["description" .= txt]

-- | Set the text widget "placeholder", i.e. text displayed in empty text widget
setTextPlaceholder :: TextWidget -> Text -> IO ()
setTextPlaceholder b txt = do
  modify b placeholder txt
  update b ["placeholder" .= txt]

-- | Get the Text string value.
getTextValue :: TextWidget -> IO Text
getTextValue = readIORef . value

-- | Get the Text widget "description" value.
getTextDescription :: TextWidget -> IO Text
getTextDescription = readIORef . description

-- | Get the Text widget placeholder value.
getTextPlaceholder :: TextWidget -> IO Text
getTextPlaceholder = readIORef . placeholder

-- | Set a function to be activated on click
setSubmitHandler :: TextWidget -> (TextWidget -> IO ()) -> IO ()
setSubmitHandler = writeIORef . submitHandler

-- | Get the click handler for a button
getSubmitHandler :: TextWidget -> IO (TextWidget -> IO ())
getSubmitHandler = readIORef . submitHandler

-- | Artificially trigger a TextWidget submit
triggerSubmit :: TextWidget -> IO ()
triggerSubmit tw = do
  handler <- getSubmitHandler tw
  handler tw

instance ToJSON TextWidget where
  toJSON b = object
               [ "_view_name" .= str "TextView"
               , "visible" .= True
               , "_css" .= object []
               , "msg_throttle" .= (3 :: Int)
               , "value" .= get value b
               , "description" .= get description b
               , "placeholder" .= get placeholder b
               ]
    where
      get x y = unsafePerformIO . readIORef . x $ y

instance IHaskellDisplay TextWidget where
  display b = do
    widgetSendView b
    return $ Display []

instance IHaskellWidget TextWidget where
  getCommUUID = uuid
  -- Two possibilities: 1. content -> event -> "submit" 2. sync_data -> value -> <new_value>
  comm tw (Object dict1) _ =
    case Map.lookup "sync_data" dict1 of
      Just (Object dict2) ->
        case Map.lookup "value" dict2 of
          Just (String val) -> setTextValue tw val
          Nothing           -> return ()
      Nothing ->
        case Map.lookup "content" dict1 of
          Just (Object dict2) ->
            case Map.lookup "event" dict2 of
              Just (String event) -> when (event == "submit") $ triggerSubmit tw
              Nothing             -> return ()
          Nothing -> return ()

str :: String -> String
str = id
