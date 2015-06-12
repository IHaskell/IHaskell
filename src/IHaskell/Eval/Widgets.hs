module IHaskell.Eval.Widgets (
    widgetSendOpen,
    widgetSendUpdate,
    widgetSendView,
    widgetSendClose,
    relayWidgetMessages,
    widgetHandler,
    ) where

import           IHaskellPrelude

import           Control.Concurrent.Chan (writeChan)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TChan
import           Control.Monad (foldM)
import           Data.Aeson
import qualified Data.Map as Map
import           System.IO.Unsafe (unsafePerformIO)

import           IHaskell.Display
import           IHaskell.Eval.Util (unfoldM)
import           IHaskell.IPython.Message.UUID
import           IHaskell.Types

-- All comm_open messages go here
widgetMessages :: TChan WidgetMsg
{-# NOINLINE widgetMessages #-}
widgetMessages = unsafePerformIO newTChanIO

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

-- | Handle a single widget message. Takes necessary actions according to the message type, such as
-- opening comms, storing and updating widget representation in the kernel state etc.
handleMessage :: (Message -> IO ())
              -> MessageHeader
              -> KernelState
              -> WidgetMsg
              -> IO KernelState
handleMessage send replyHeader state msg = do
  let oldComms = openComms state

  case msg of
    Open widget initVal stateVal -> do
      let target = targetName widget
          uuid = getCommUUID widget
          present = isJust $ Map.lookup uuid oldComms

          newComms = Map.insert uuid widget oldComms
          newState = state { openComms = newComms }

          communicate val = do
            head <- dupHeader replyHeader CommDataMessage
            send $ CommData head uuid val

      -- If the widget is present, don't open it again.
      if present
        then return state
        else do
          -- Send the comm open
          header <- dupHeader replyHeader CommOpenMessage
          send $ CommOpen header target uuid initVal

          -- Initial state update
          communicate . toJSON $ UpdateState stateVal

          -- Send anything else the widget requires.
          open widget communicate

          -- Store the widget in the kernelState
          return newState

    Close widget value -> do
      let target = targetName widget
          uuid = getCommUUID widget
          present = isJust $ Map.lookup uuid oldComms

          newComms = Map.delete uuid $ openComms state
          newState = state { openComms = newComms }

      -- If the widget is not present in the state, we don't close it.
      if present
        then do
          header <- dupHeader replyHeader CommCloseMessage
          send $ CommClose header uuid value
          return newState
        else return state

    View widget -> do
      let uuid = getCommUUID widget
          present = isJust $ Map.lookup uuid oldComms

      -- If the widget is present, we send a display message on it's comm.
      when present $ do
        header <- dupHeader replyHeader CommDataMessage
        send . CommData header uuid $ toJSON DisplayWidget
      return state

    Update widget value -> do
      -- Assume that a state update means that it is time the stored widget also gets updated. Thus
      -- replace the stored widget with the copy passed in the CommMsg.
      let uuid = getCommUUID widget
          present = isJust $ Map.lookup uuid oldComms

          -- The update inside the kernel state happens here.
          newComms = Map.insert uuid widget oldComms
          newState = state { openComms = newComms }

      -- If the widget is present, we send an update message on its comm. We also replace the widget
      -- stored in the kernel state with the one provided here.
      if present
        then do
          header <- dupHeader replyHeader CommDataMessage
          send . CommData header uuid . toJSON $ UpdateState value
          return newState
        else return state

-- Handle messages one-by-one, while updating state simultaneously
widgetHandler :: (Message -> IO ())
              -> MessageHeader
              -> KernelState
              -> [WidgetMsg]
              -> IO KernelState
widgetHandler sender header = foldM (handleMessage sender header)
