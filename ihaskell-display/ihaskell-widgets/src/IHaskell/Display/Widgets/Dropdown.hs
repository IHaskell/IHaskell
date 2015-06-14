{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module IHaskell.Display.Widgets.Dropdown where

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

-- | A 'Dropdown' represents a Dropdown widget from IPython.html.widgets.
data Dropdown =
       Dropdown
         { uuid :: U.UUID       -- ^ The UUID for the comm
         , description :: IORef Text -- ^ The label displayed beside the dropdown
         , disabled :: IORef Bool    -- ^ Whether the dropdown is disabled
         , selectedLabel :: IORef Text -- ^ The label which is currently selected
         , labelOptions :: IORef [Text] -- ^ The possible label options
         }

-- | Create a new dropdown
mkDropdown :: IO Dropdown
mkDropdown = do
  -- Default properties, with a random uuid
  commUUID <- U.random
  desc <- newIORef ""
  dis <- newIORef False
  sel <- newIORef ""
  opts <- newIORef []

  let b = Dropdown
        { uuid = commUUID
        , description = desc
        , disabled = dis
        , selectedLabel = sel
        , labelOptions = opts
        }

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen b (toJSON DropdownInitData) (toJSON b)

  -- Return the dropdown widget
  return b

-- | Send an update msg with custom json. Make it easy to update fragments of the
-- state, by accepting Pairs instead of a Value.
update :: Dropdown -> [Pair] -> IO ()
update b v = widgetSendUpdate b . toJSON . object $ v

-- | Modify attributes of a dropdown, stored inside it as IORefs
modify :: Dropdown -> (Dropdown -> IORef a) -> a -> IO ()
modify d attr val = writeIORef (attr d) val

setDropdownText :: Dropdown -> Text -> IO ()
setDropdownText widget text = do
  modify widget description text
  update widget ["description" .= text]

setDropdownStatus :: Dropdown -> Bool -> IO ()
setDropdownStatus widget stat = do
  let newStat = not stat
  modify widget disabled newStat
  update widget ["disabled" .= newStat]

setDropdownOptions :: Dropdown -> [Text] -> IO ()
setDropdownOptions widget opts = do
  modify widget labelOptions opts
  update widget ["_options_labels" .= opts]

setDropdownSelected :: Dropdown -> Text -> IO ()
setDropdownSelected widget opt = do
  possibleOpts <- getDropdownOptions widget
  when (opt `elem` possibleOpts) $ do
    modify widget selectedLabel opt
    update widget ["selected_label" .= opt]

toggleDropdownStatus :: Dropdown -> IO ()
toggleDropdownStatus widget = modifyIORef (disabled widget) not

getDropdownText :: Dropdown -> IO Text
getDropdownText = readIORef . description

getDropdownStatus :: Dropdown -> IO Bool
getDropdownStatus = fmap not . readIORef . disabled

getDropdownOptions :: Dropdown -> IO [Text]
getDropdownOptions = readIORef . labelOptions

getDropdownSelected :: Dropdown -> IO Text
getDropdownSelected = readIORef . selectedLabel

data ViewName = DropdownWidget

instance ToJSON ViewName where
  toJSON DropdownWidget = "DropdownView"

data InitData = DropdownInitData

instance ToJSON InitData where
  toJSON DropdownInitData = object
                            [ "model_name" .= str "WidgetModel"
                            , "widget_class" .= str "IPython.Dropdown"
                            ]

instance ToJSON Dropdown where
  toJSON b = object
               [ "_view_name" .= toJSON DropdownWidget
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

instance IHaskellDisplay Dropdown where
  display b = do
    widgetSendView b
    return $ Display []

instance IHaskellWidget Dropdown where
  getCommUUID = uuid
  comm widget (Object dict1) _ = do
    let key1 = "sync_data" :: Text
        key2 = "selected_label" :: Text
        Just (Object dict2) = Map.lookup key1 dict1
        Just (String label) = Map.lookup key2 dict2
    modify widget selectedLabel label

str :: String -> String
str = id
