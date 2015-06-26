{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module IHaskell.Display.Widgets.Dropdown (
    -- * The dropdown widget
    DropdownWidget,
    -- * Constructor
    mkDropdownWidget,
    -- * Set properties
    setDropdownText,
    setDropdownStatus,
    setDropdownOptions,
    setDropdownSelected,
    -- * Get properties
    getDropdownText,
    getDropdownStatus,
    getDropdownOptions,
    getDropdownSelected,
    -- * Handle changes
    setSelectionHandler,
    getSelectionHandler,
    triggerSelection,
    ) where

-- To keep `cabal repl` happy when running from the ihaskell repo
import           Prelude

import           Control.Monad (when)
import           Data.Aeson (ToJSON, Value(..), object, toJSON, (.=))
import           Data.Aeson.Types (Pair)
import           Data.HashMap.Strict as Map
import           Data.IORef
import           Data.Text (Text)
import           System.IO.Unsafe (unsafePerformIO)

import           IHaskell.Display
import           IHaskell.Eval.Widgets
import qualified IHaskell.IPython.Message.UUID as U

import           IHaskell.Display.Widgets.Common

-- | A 'Dropdown' represents a Dropdown widget from IPython.html.widgets.
data DropdownWidget =
       DropdownWidget
         { uuid :: U.UUID       -- ^ The UUID for the comm
         , description :: IORef Text -- ^ The label displayed beside the dropdown
         , disabled :: IORef Bool    -- ^ Whether the dropdown is disabled
         , selectedLabel :: IORef Text -- ^ The label which is currently selected
         , labelOptions :: IORef [Text] -- ^ The possible label options
         , selectionHandler :: IORef (DropdownWidget -> IO ())
         }

-- | Create a new dropdown
mkDropdownWidget :: IO DropdownWidget
mkDropdownWidget = do
  -- Default properties, with a random uuid
  commUUID <- U.random
  desc <- newIORef ""
  dis <- newIORef False
  sel <- newIORef ""
  opts <- newIORef []
  handler <- newIORef $ const $ return ()

  let b = DropdownWidget
        { uuid = commUUID
        , description = desc
        , disabled = dis
        , selectedLabel = sel
        , labelOptions = opts
        , selectionHandler = handler
        }

  let initData = object
                   [ "model_name" .= str "WidgetModel"
                   , "widget_class" .= str "IPython.Dropdown"
                   ]

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen b initData $ toJSON b

  -- Return the dropdown widget
  return b

setDropdownText :: DropdownWidget -> Text -> IO ()
setDropdownText widget text = do
  modify widget description text
  update widget ["description" .= text]

setDropdownStatus :: DropdownWidget -> Bool -> IO ()
setDropdownStatus widget stat = do
  let newStat = not stat
  modify widget disabled newStat
  update widget ["disabled" .= newStat]

setDropdownOptions :: DropdownWidget -> [Text] -> IO ()
setDropdownOptions widget opts = do
  modify widget labelOptions opts
  update widget ["_options_labels" .= opts]

setDropdownSelected :: DropdownWidget -> Text -> IO ()
setDropdownSelected widget opt = do
  possibleOpts <- getDropdownOptions widget
  when (opt `elem` possibleOpts) $ do
    modify widget selectedLabel opt
    update widget ["selected_label" .= opt]
  triggerSelection widget

toggleDropdownStatus :: DropdownWidget -> IO ()
toggleDropdownStatus widget = modifyIORef (disabled widget) not

getDropdownText :: DropdownWidget -> IO Text
getDropdownText = readIORef . description

getDropdownStatus :: DropdownWidget -> IO Bool
getDropdownStatus = fmap not . readIORef . disabled

getDropdownOptions :: DropdownWidget -> IO [Text]
getDropdownOptions = readIORef . labelOptions

getDropdownSelected :: DropdownWidget -> IO Text
getDropdownSelected = readIORef . selectedLabel

-- | Set a function to be activated on selection
setSelectionHandler :: DropdownWidget -> (DropdownWidget -> IO ()) -> IO ()
setSelectionHandler = writeIORef . selectionHandler

-- | Get the selection handler for a dropdown
getSelectionHandler :: DropdownWidget -> IO (DropdownWidget -> IO ())
getSelectionHandler = readIORef . selectionHandler

-- | Artificially trigger a selection
triggerSelection :: DropdownWidget -> IO ()
triggerSelection widget = do
  handler <- getSelectionHandler widget
  handler widget

instance ToJSON DropdownWidget where
  toJSON b = object
               [ "_view_name" .= str "DropdownView"
               , "visible" .= True
               , "_css" .= object []
               , "msg_throttle" .= (3 :: Int)
               , "disabled" .= get disabled b
               , "description" .= get description b
               , "_options_labels" .= get labelOptions b
               , "selected_label" .= get selectedLabel b
               , "button_style" .= str ""
               ]
    where
      get x y = unsafePerformIO . readIORef . x $ y

instance IHaskellDisplay DropdownWidget where
  display b = do
    widgetSendView b
    return $ Display []

instance IHaskellWidget DropdownWidget where
  getCommUUID = uuid
  comm widget (Object dict1) _ = do
    let key1 = "sync_data" :: Text
        key2 = "selected_label" :: Text
        Just (Object dict2) = Map.lookup key1 dict1
        Just (String label) = Map.lookup key2 dict2
    modify widget selectedLabel label
    triggerSelection widget
