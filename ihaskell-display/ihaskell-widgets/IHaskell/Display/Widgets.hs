{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module IHaskell.Display.Widgets () where

import           ClassyPrelude

import           Data.Aeson

import           IHaskell.Widgets
import           IHaskell.Display

data WidgetName = ButtonWidget

data WidgetMessage = DisplayWidget
                   | InitialState WidgetName

instance ToJSON WidgetName where
  toJSON ButtonWidget = "ButtonView"

instance ToJSON WidgetMessage where
  toJSON DisplayWidget = object ["method" .= str "display"]
  toJSON (InitialState name) = object
                                 [ "method" .= str "update"
                                 , "state" .= object
                                                [ "_view_name" .= name
                                                , "visible" .= True
                                                , "_css" .= object []
                                                , "msg_throttle" .= (3 :: Int)
                                                , "disabled" .= False
                                                , "description" .= str "Button"
                                                ]
                                 ]

str :: String -> String
str = id

instance IHaskellDisplay Slider where
  display _ = return $ Display []

-- | Text to parse.
data ParseText = ParseText String

instance FromJSON ParseText where
  parseJSON (Object v) = ParseText <$> v .: "text"
  parseJSON _ = fail "Expecting object"

instance IHaskellWidget Slider where
  -- Name for this widget.
  targetName _ = "WidgetModel"
  -- Start by sending messages to set up the widget.
  open widget send = do
    putStrLn "Sending widgets!"
    send $ toJSON $ InitialState ButtonWidget
    send $ toJSON DisplayWidget
