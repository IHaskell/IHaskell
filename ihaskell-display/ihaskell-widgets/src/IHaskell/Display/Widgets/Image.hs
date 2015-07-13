{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

module IHaskell.Display.Widgets.Image (
-- * The Image Widget
ImageWidget,
             -- * Constructor
             mkImageWidget) where

-- To keep `cabal repl` happy when running from the ihaskell repo
import           Prelude

import           Control.Monad (when, join)
import           Data.Aeson
import           Data.HashMap.Strict as HM
import           Data.IORef (newIORef)
import           Data.Monoid (mempty)
import           Data.Text (Text)
import           Data.Vinyl (Rec(..), (<+>))

import           IHaskell.Display
import           IHaskell.Eval.Widgets
import           IHaskell.IPython.Message.UUID as U

import           IHaskell.Display.Widgets.Types
import           IHaskell.Display.Widgets.Common

-- | An 'ImageWidget' represents a Image widget from IPython.html.widgets.
type ImageWidget = IPythonWidget ImageType

-- | Create a new image widget
mkImageWidget :: IO ImageWidget
mkImageWidget = do
  -- Default properties, with a random uuid
  uuid <- U.random

  let dom = defaultDOMWidget "ImageView"
      img = (SImageFormat =:: PNG)
            :& (SB64Value =:: mempty)
            :& RNil
      widgetState = WidgetState (dom <+> img)

  stateIO <- newIORef widgetState

  let widget = IPythonWidget uuid stateIO

  let initData = object ["model_name" .= str "WidgetModel", "widget_class" .= str "IPython.Image"]

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen widget initData $ toJSON widgetState

  -- Return the image widget
  return widget

instance IHaskellDisplay ImageWidget where
  display b = do
    widgetSendView b
    return $ Display []

instance IHaskellWidget ImageWidget where
  getCommUUID = uuid
