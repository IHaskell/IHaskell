{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module IHaskell.Display.Widgets.Media.Audio
  ( -- * The Audio Widget
    AudioWidget
    -- * Constructor
  , mkAudio
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

-- | An 'AudioWidget' represents a Audio widget from IPython.html.widgets.
type AudioWidget = IPythonWidget 'AudioType

-- | Create a new audio widget
mkAudio :: IO AudioWidget
mkAudio = do
  -- Default properties, with a random uuid
  wid <- U.random
  layout <- mkLayout

  let mediaAttrs = defaultMediaWidget "AudioView" "AudioModel" layout
      audioAttrs = (AudioFormat =:: MP3)
              :& (AutoPlay =:: True)
              :& (Loop =:: True)
              :& (Controls =:: True)
              :& RNil
      widgetState = WidgetState (mediaAttrs <+> audioAttrs)

  stateIO <- newIORef widgetState

  let widget = IPythonWidget wid stateIO

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen widget $ toJSON widgetState

  -- Return the audio widget
  return widget

instance IHaskellWidget AudioWidget where
  getCommUUID = uuid
  getBufferPaths _ = [["value"]]
