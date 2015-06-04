{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module IHaskell.Display.Widgets.Button where

import           Prelude

import           Control.Monad                 (when)
import           Data.Aeson                    (ToJSON, Value (..), object,
                                                toJSON, (.=))
import           Data.Aeson.Types              (Pair)
import           Data.HashMap.Strict           as Map
import           Data.IORef
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           IHaskell.Display
import qualified IHaskell.IPython.Message.UUID as U
import           IHaskell.Eval.Widgets
import           IHaskell.Types (WidgetMethod (..))
import           System.IO.Unsafe              (unsafePerformIO)

-- | ADT for a button
data Button = Button { uuid         :: U.UUID
                     , description  :: IORef Text
                     , tooltip      :: IORef Text
                     , disabled     :: IORef Bool
                     , buttonStyle  :: IORef ButtonStyle
                     , clickHandler :: IORef (Button -> IO ())
                     }

-- | Pre-defined button-styles
data ButtonStyle = Primary | Success | Info | Warning | Danger | None
  deriving (Eq, Show)

-- | Create a new button
mkButton :: IO Button
mkButton = do
  -- Default properties, with a random uuid
  uuid   <- U.random
  sender <- newIORef Nothing
  desc   <- newIORef "label"        -- Non-empty to get a display
  ttip   <- newIORef ""
  dis    <- newIORef False
  sty    <- newIORef None
  fun    <- newIORef (\_ -> return ())

  let b = Button uuid desc ttip dis sty fun

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen b (toJSON ButtonInitData) (toJSON b)

  -- Return the button widget
  return b

update :: Button -> [Pair] -> IO ()
update b v = widgetSendUpdate b . toJSON . object $ v

modify :: Button -> (Button -> IORef a) -> a -> IO ()
modify b attr val = writeIORef (attr b) val

-- | Set the button style
setButtonStyle :: ButtonStyle -> Button -> IO ()
setButtonStyle bst b = do
  modify b buttonStyle bst
  update b [ "button_style" .= bst ]

-- | Set the button label
setButtonLabel :: Text -> Button -> IO ()
setButtonLabel txt b = do
  modify b description txt
  update b [ "description" .= txt ]

-- | Set the button tooltip
setButtonTooltip :: Text -> Button -> IO ()
setButtonTooltip txt b = do
  modify b tooltip txt
  update b [ "tooltip" .= txt ]

-- | Disable the button
disableButton :: Button -> IO ()
disableButton b = do
  modify b disabled True
  update b [ "disabled" .= True ]

-- | Enable the button
enableButton :: Button -> IO ()
enableButton b = do
  modify b disabled False
  update b [ "disabled" .= False ]

-- | Toggle the button
toggleButtonStatus :: Button -> IO ()
toggleButtonStatus b = do
  oldVal <- isDisabled b
  let newVal = not oldVal
  modify b disabled newVal 
  update b [ "disabled" .= newVal ]

-- | Get the button style
getButtonStyle :: Button -> IO ButtonStyle
getButtonStyle = readIORef . buttonStyle

-- | Get the button text
getButtonText :: Button -> IO Text
getButtonText = readIORef . description

-- | Get the button tooltip
getButtonTooltip :: Button -> IO Text
getButtonTooltip = readIORef . tooltip

-- | Check whether the button is disabled
isDisabled :: Button -> IO Bool
isDisabled = readIORef . disabled

-- | Set a function to be activated on click
onClicked :: Button -> (Button -> IO ()) -> IO ()
onClicked = writeIORef . clickHandler

-- | Artificially trigger a button click
triggerClick :: Button -> IO ()
triggerClick button = do
  handler <- readIORef $ clickHandler button
  handler button

instance ToJSON ButtonStyle where
  toJSON Primary = "primary"
  toJSON Success = "success"
  toJSON Info    = "info"
  toJSON Warning = "warning"
  toJSON Danger  = "danger"
  toJSON None    = ""

data ViewName = ButtonWidget

instance ToJSON ViewName where
  toJSON ButtonWidget = "ButtonView"

data InitData = ButtonInitData

instance ToJSON InitData where
  toJSON ButtonInitData = object [ "model_name"   .= str "WidgetModel"
                                 , "widget_class" .= str "IPython.Button"
                                 ]

instance ToJSON Button where
  toJSON b = object [ "_view_name" .= toJSON ButtonWidget
                    , "visible" .= True
                    , "_css" .= object []
                    , "msg_throttle" .= (3 :: Int)
                    , "disabled" .= get disabled b
                    , "description" .= get description b
                    , "tooltip" .= get tooltip b
                    , "button_style" .= get buttonStyle b
                    ]
    where get x y = unsafePerformIO . readIORef . x $ y

instance IHaskellDisplay Button where
  display b = do
    widgetSendView b
    return $ Display []

instance IHaskellWidget Button where
  getCommUUID = uuid
  comm widget (Object dict1) _ = do
    let key1 = "content" :: Text
        key2 = "event"   :: Text
        Just (Object dict2) = Map.lookup key1 dict1
        Just (String event) = Map.lookup key2 dict2
    when (event == "click") $ triggerClick widget

str :: String -> String
str = id
