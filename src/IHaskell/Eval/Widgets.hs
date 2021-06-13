{-# language NoImplicitPrelude, DoAndIfThenElse, OverloadedStrings, ExtendedDefaultRules #-}
module IHaskell.Eval.Widgets (
    widgetSendOpen,
    widgetSendView,
    widgetSendUpdate,
    widgetSendCustom,
    widgetSendClose,
    widgetSendValue,
    widgetPublishDisplay,
    widgetClearOutput,
    relayWidgetMessages,
    widgetHandler,
    ) where

import           IHaskellPrelude

import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TChan
import           Control.Monad (foldM)
import           Data.Aeson
import           Data.Aeson.Types (emptyArray)
import qualified Data.Map as Map
import           System.IO.Unsafe (unsafePerformIO)

import           IHaskell.Display
import           IHaskell.Eval.Util (unfoldM)
import           IHaskell.IPython.Types (showMessageType)
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
widgetSend mtype widget value = queue $ mtype (Widget widget) value

-- | Send a message to open a comm
widgetSendOpen :: IHaskellWidget a => a -> Value -> IO ()
widgetSendOpen = widgetSend Open

-- | Send a state update message
widgetSendUpdate :: IHaskellWidget a => a -> Value -> IO ()
widgetSendUpdate = widgetSend Update

-- | Send a [method .= display] comm_msg
widgetSendView :: IHaskellWidget a => a -> IO ()
widgetSendView = queue . View . Widget

-- | Send a comm_close
widgetSendClose :: IHaskellWidget a => a -> Value -> IO ()
widgetSendClose = widgetSend Close

-- | Send a [method .= custom, content .= value] comm_msg
widgetSendCustom :: IHaskellWidget a => a -> Value -> IO ()
widgetSendCustom = widgetSend Custom

-- | Send a custom Value
widgetSendValue :: IHaskellWidget a => a -> Value -> IO ()
widgetSendValue widget = queue . JSONValue (Widget widget)

-- | Send a `display_data` message as a [method .= custom] message
widgetPublishDisplay :: (IHaskellWidget a, IHaskellDisplay b) => a -> b -> IO ()
widgetPublishDisplay widget disp = display disp >>= queue . DispMsg (Widget widget)

-- | Send a `clear_output` message as a [method .= custom] message
widgetClearOutput :: IHaskellWidget a => a -> Bool -> IO ()
widgetClearOutput widget w = queue $ ClrOutput (Widget widget) w

-- | Handle a single widget message. Takes necessary actions according to the message type, such as
-- opening comms, storing and updating widget representation in the kernel state etc.
handleMessage :: (Message -> IO ())
              -> MessageHeader
              -> KernelState
              -> WidgetMsg
              -> IO KernelState
handleMessage send replyHeader state msg = do
  case msg of
    Open widget value -> do
      let target_name = targetName widget
          target_module = targetModule widget
          uuid = getCommUUID widget
          present = isJust $ Map.lookup uuid oldComms

          newComms = Map.insert uuid widget oldComms
          newState = state { openComms = newComms }

          content = object [ "state" .= value, "buffer_paths" .= emptyArray ]

          communicate val = do
            head <- dupHeader replyHeader CommDataMessage
            send $ CommData head uuid val

      -- If the widget is present, don't open it again.
      if present
        then return state
        else do
          -- Send the comm open, with the initial state
          hdr <- dupHeader replyHeader CommOpenMessage
          let hdrV = setVersion hdr "2.0.0" -- Widget Messaging Protocol Version
          send $ CommOpen hdrV target_name target_module uuid content

          -- Send anything else the widget requires.
          open widget communicate

          -- Store the widget in the kernelState
          return newState

    Close widget value -> do
      let uuid = getCommUUID widget
          present = isJust $ Map.lookup uuid oldComms

          newComms = Map.delete uuid $ openComms state
          newState = state { openComms = newComms }

      -- If the widget is not present in the state, we don't close it.
      if present
        then do
          hdr <- dupHeader replyHeader CommCloseMessage
          send $ CommClose hdr uuid value
          return newState
        else return state

    View widget -> sendMessage widget (toJSON DisplayWidget)

    Update widget value -> sendMessage widget (toJSON $ UpdateState value)

    Custom widget value -> sendMessage widget (toJSON $ CustomContent value)

    JSONValue widget value -> sendMessage widget value

    DispMsg widget disp -> do
      dispHeader <- dupHeader replyHeader DisplayDataMessage
      let dmsg = WidgetDisplay dispHeader $ unwrap disp
      sendMessage widget (toJSON $ CustomContent $ toJSON dmsg)

    ClrOutput widget w -> do
      hdr <- dupHeader replyHeader ClearOutputMessage
      let cmsg = WidgetClear hdr w
      sendMessage widget (toJSON $ CustomContent $ toJSON cmsg)

  where
    oldComms = openComms state
    sendMessage widget value = do
      let uuid = getCommUUID widget
          present = isJust $ Map.lookup uuid oldComms

      -- If the widget is present, we send an update message on its comm.
      when present $ do
        hdr <- dupHeader replyHeader CommDataMessage
        send $ CommData hdr uuid value
      return state

    unwrap :: Display -> [DisplayData]
    unwrap (ManyDisplay ds) = concatMap unwrap ds
    unwrap (Display ddatas) = ddatas

-- Override toJSON for PublishDisplayData for sending Display messages through [method .= custom]
data WidgetDisplay = WidgetDisplay MessageHeader [DisplayData]

instance ToJSON WidgetDisplay where
  toJSON (WidgetDisplay replyHeader ddata) =
    let pbval = toJSON $ PublishDisplayData replyHeader ddata Nothing
    in toJSON $ IPythonMessage replyHeader pbval DisplayDataMessage

-- Override toJSON for ClearOutput
data WidgetClear = WidgetClear MessageHeader Bool

instance ToJSON WidgetClear where
  toJSON (WidgetClear replyHeader w) =
    let clrVal = toJSON $ ClearOutput replyHeader w
    in toJSON $ IPythonMessage replyHeader clrVal ClearOutputMessage

data IPythonMessage = IPythonMessage MessageHeader Value MessageType

instance ToJSON IPythonMessage where
  toJSON (IPythonMessage replyHeader val mtype) =
    object
      [ "header" .= replyHeader
      , "parent_header" .= str ""
      , "metadata" .= object []
      , "content" .= val
      , "msg_type" .= (toJSON . showMessageType $ mtype)
      ]

str :: String -> String
str = id

-- Handle messages one-by-one, while updating state simultaneously
widgetHandler :: (Message -> IO ())
              -> MessageHeader
              -> KernelState
              -> [WidgetMsg]
              -> IO KernelState
widgetHandler sender hdr = foldM (handleMessage sender hdr)
