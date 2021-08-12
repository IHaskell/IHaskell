{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module IHaskell.Display.Widgets.Media.Image
  ( -- * The Image Widget
    ImageWidget
    -- * Constructor
  , mkImage
  ) where

-- To keep `cabal repl` happy when running from the ihaskell repo
import           Prelude

import           Data.Aeson
import           Data.IORef (newIORef)
import           Data.Vinyl (Rec(..), (<+>))

import           IHaskell.Display
import           IHaskell.Eval.Widgets
import           IHaskell.IPython.Message.UUID as U

import           IHaskell.Display.Widgets.Types
import           IHaskell.Display.Widgets.Common
import           IHaskell.Display.Widgets.Layout.LayoutWidget

-- | An 'ImageWidget' represents a Image widget from IPython.html.widgets.
type ImageWidget = IPythonWidget 'ImageType

-- | Create a new image widget
mkImage :: IO ImageWidget
mkImage = do
  -- Default properties, with a random uuid
  wid <- U.random
  layout <- mkLayout

  let mediaAttrs = defaultMediaWidget "ImageView" "ImageModel" layout
      imageAttrs = (ImageFormat =:: PNG)
                   :& (Width =:+ 0)
                   :& (Height =:+ 0)
                   :& RNil
      widgetState = WidgetState (mediaAttrs <+> imageAttrs)

  stateIO <- newIORef widgetState

  let widget = IPythonWidget wid stateIO

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen widget $ toJSON widgetState

  -- Return the image widget
  return widget

instance IHaskellWidget ImageWidget where
  getCommUUID = uuid
  getBufferPaths _ = [["value"]]
