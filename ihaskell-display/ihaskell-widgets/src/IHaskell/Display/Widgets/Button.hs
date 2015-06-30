{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module IHaskell.Display.Widgets.Button (
    -- * The Button Widget
    Button,
    -- * Create a new button
    mkButton,
    -- * Click manipulation
    triggerClick,
    ) where

-- To keep `cabal repl` happy when running from the ihaskell repo
import           Prelude

import           Control.Monad (when, join)
import           Data.Aeson (ToJSON, Value(..), object, toJSON, (.=))
import           Data.Aeson.Types (Pair)
import           Data.Map as M
import           Data.HashMap.Strict as HashMap
import           Data.IORef
import           Data.Text (Text)
import qualified Data.Text as T
import           System.IO.Unsafe (unsafePerformIO)

import           IHaskell.Display hiding (Widget)
import           IHaskell.Eval.Widgets
import qualified IHaskell.IPython.Message.UUID as U
import           IHaskell.Types (WidgetMethod(..))

import           IHaskell.Display.Widgets.Types

-- | A 'Button' represents a Button from IPython.html.widgets.
type Button = Widget ButtonType

-- | Create a new button
mkButton :: IO Button
mkButton = do
  -- Default properties, with a random uuid
  uuid <- U.random

  let dom = domWidgetWith "ButtonView"
      but = [ SDescription ~= ""
            , STooltip ~= ""
            , SDisabled ~= False
            , SIcon ~= ""
            , SButtonStyle ~= DefaultButton
            , SClickHandler ~= return ()
            ]
      attributes = M.fromList $ dom ++ but

  attrIO <- newIORef attributes

  let button = Widget uuid attrIO :: Widget ButtonType
      initData = object ["model_name" .= str "WidgetModel", "widget_class" .= str "IPython.Button"]

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen button initData $ toJSON button

  -- Return the button widget
  return button

-- | Artificially trigger a button click
triggerClick :: Button -> IO ()
triggerClick button = join $ getField button SClickHandler

-- instance ToJSON Button where
--   toJSON b = object
--                [ "_view_name" .= str "ButtonView"
--                , "visible" .= True
--                , "_css" .= object []
--                , "msg_throttle" .= (3 :: Int)
--                , "disabled" .= get disabled b
--                , "description" .= get description b
--                , "tooltip" .= get tooltip b
--                , "button_style" .= get buttonStyle b
--                ]
--     where
--       get x y = unsafePerformIO . readIORef . x $ y

instance IHaskellDisplay Button where
  display b = do
    widgetSendView b
    return $ Display []

instance IHaskellWidget Button where
  getCommUUID = uuid
  comm widget (Object dict1) _ = do
    let key1 = "content" :: Text
        key2 = "event" :: Text
        Just (Object dict2) = HashMap.lookup key1 dict1
        Just (String event) = HashMap.lookup key2 dict2
    when (event == "click") $ triggerClick widget
