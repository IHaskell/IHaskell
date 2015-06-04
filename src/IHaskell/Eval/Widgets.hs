module IHaskell.Eval.Widgets
       ( widgetSendOpen
       , widgetSendUpdate
       , widgetSendView
       , widgetSendClose
       , relayWidgetMessages
       ) where

import IHaskellPrelude

import Data.Aeson (Value)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan
import System.IO.Unsafe (unsafePerformIO)

import IHaskell.Display
import IHaskell.Types (Message (..), WidgetMsg (..))
import IHaskell.IPython.Message.UUID
import IHaskell.Eval.Util (unfoldM)

-- All comm_open messages go here
widgetMessages :: TChan WidgetMsg
{-# NOINLINE widgetMessages #-}
widgetMessages  = unsafePerformIO newTChanIO

-- | Return all pending comm_close messages
relayWidgetMessages :: IO [WidgetMsg]
relayWidgetMessages = relayMessages widgetMessages

-- | Extract all messages from a TChan and wrap them in a list
relayMessages :: TChan a -> IO [a]
relayMessages = unfoldM . atomically . tryReadTChan

-- | Write a widget message to the chan
queue :: WidgetMsg -> IO ()
queue = atomically . writeTChan widgetMessages

-- | Send a message
widgetSend :: IHaskellWidget a
              => (Widget -> Value -> WidgetMsg)
              -> a -> Value -> IO ()
widgetSend msgType widget value = queue $ msgType (Widget widget) value

widgetSendOpen :: IHaskellWidget a => a -> Value -> Value -> IO ()
widgetSendOpen widget initVal stateVal =
  queue $ Open (Widget widget) initVal stateVal

widgetSendUpdate :: IHaskellWidget a => a -> Value -> IO ()
widgetSendUpdate = widgetSend Update

widgetSendView :: IHaskellWidget a => a -> IO ()
widgetSendView = queue . View . Widget

widgetSendClose :: IHaskellWidget a => a -> Value -> IO ()
widgetSendClose = widgetSend Close
