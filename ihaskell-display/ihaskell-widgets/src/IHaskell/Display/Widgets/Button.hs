{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module IHaskell.Display.Widgets.Button (
    -- * The Button Widget
    Button,
    -- * Create a new button
    mkButton,
    -- * Set button properties
    setButtonStyle,
    setButtonLabel,
    setButtonTooltip,
    setButtonStatus,
    toggleButtonStatus,
    -- * Get button properties
    getButtonStyle,
    getButtonLabel,
    getButtonTooltip,
    getButtonStatus,
    -- * Click handlers
    setClickHandler,
    getClickHandler,
    triggerClick,
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

-- | A 'Button' represents a Button from IPython.html.widgets.
data Button =
       Button
         { uuid :: U.UUID       -- ^ The UUID for the comm
         , description :: IORef Text -- ^ The label displayed on the button
         , tooltip :: IORef Text     -- ^ The tooltip shown on mouseover
         , disabled :: IORef Bool    -- ^ Whether the button is disabled
         , buttonStyle :: IORef ButtonStyle -- ^ The button_style
         , clickHandler :: IORef (Button -> IO ()) -- ^ Function executed when button is clicked
         }

-- | Create a new button
mkButton :: IO Button
mkButton = do
  -- Default properties, with a random uuid
  commUUID <- U.random
  desc <- newIORef "label"      -- Non-empty to get a display
  ttip <- newIORef ""
  dis <- newIORef False
  sty <- newIORef None
  fun <- newIORef $ const $ return ()

  let b = Button
        { uuid = commUUID
        , description = desc
        , tooltip = ttip
        , disabled = dis
        , buttonStyle = sty
        , clickHandler = fun
        }

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen b (toJSON ButtonInitData) (toJSON b)

  -- Return the button widget
  return b

-- | Set the button style
setButtonStyle :: Button -> ButtonStyle -> IO ()
setButtonStyle b bst = do
  modify b buttonStyle bst
  update b ["button_style" .= bst]

-- | Set the button label
setButtonLabel :: Button -> Text -> IO ()
setButtonLabel b txt = do
  modify b description txt
  update b ["description" .= txt]

-- | Set the button tooltip
setButtonTooltip :: Button -> Text -> IO ()
setButtonTooltip b txt = do
  modify b tooltip txt
  update b ["tooltip" .= txt]

-- | Set buttton status. True: Enabled, False: Disabled
setButtonStatus :: Button -> Bool -> IO ()
setButtonStatus b stat = do
  let newStatus = not stat
  modify b disabled newStatus
  update b ["disabled" .= newStatus]

-- | Toggle the button
toggleButtonStatus :: Button -> IO ()
toggleButtonStatus b = do
  oldVal <- getButtonStatus b
  let newVal = not oldVal
  modify b disabled newVal
  update b ["disabled" .= newVal]

-- | Get the button style
getButtonStyle :: Button -> IO ButtonStyle
getButtonStyle = readIORef . buttonStyle

-- | Get the button label
getButtonLabel :: Button -> IO Text
getButtonLabel = readIORef . description

-- | Get the button tooltip
getButtonTooltip :: Button -> IO Text
getButtonTooltip = readIORef . tooltip

-- | Check whether the button is enabled / disabled
getButtonStatus :: Button -> IO Bool
getButtonStatus = fmap not . readIORef . disabled

-- | Set a function to be activated on click
setClickHandler :: Button -> (Button -> IO ()) -> IO ()
setClickHandler = writeIORef . clickHandler

-- | Get the click handler for a button
getClickHandler :: Button -> IO (Button -> IO ())
getClickHandler = readIORef . clickHandler

-- | Artificially trigger a button click
triggerClick :: Button -> IO ()
triggerClick button = do
  handler <- getClickHandler button
  handler button

data ViewName = ButtonWidget

instance ToJSON ViewName where
  toJSON ButtonWidget = "ButtonView"

data InitData = ButtonInitData

instance ToJSON InitData where
  toJSON ButtonInitData = object
                            [ "model_name" .= str "WidgetModel"
                            , "widget_class" .= str "IPython.Button"
                            ]

instance ToJSON Button where
  toJSON b = object
               [ "_view_name" .= toJSON ButtonWidget
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

instance IHaskellDisplay Button where
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
