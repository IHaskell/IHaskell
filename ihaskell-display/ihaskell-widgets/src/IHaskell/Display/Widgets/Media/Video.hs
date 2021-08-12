{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module IHaskell.Display.Widgets.Media.Video
  ( -- * The Video Widget
    VideoWidget
    -- * Constructor
  , mkVideo
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

-- | An 'VideoWidget' represents a video widget from IPython.html.widgets.
type VideoWidget = IPythonWidget 'VideoType

-- | Create a new video widget
mkVideo :: IO VideoWidget
mkVideo = do
  -- Default properties, with a random uuid
  wid <- U.random
  layout <- mkLayout

  let mediaAttrs = defaultMediaWidget "VideoView" "VideoModel" layout
      videoAttrs = (VideoFormat =:: MP4)
              :& (Width =:+ 0)
              :& (Height =:+ 0)
              :& (AutoPlay =:: True)
              :& (Loop =:: True)
              :& (Controls =:: True)
              :& RNil
      widgetState = WidgetState (mediaAttrs <+> videoAttrs)

  stateIO <- newIORef widgetState

  let widget = IPythonWidget wid stateIO

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen widget $ toJSON widgetState

  -- Return the video widget
  return widget

instance IHaskellWidget VideoWidget where
  getCommUUID = uuid
  getBufferPaths _ = [["value"]]
