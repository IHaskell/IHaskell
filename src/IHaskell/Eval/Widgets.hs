{-# language NoImplicitPrelude, DoAndIfThenElse, OverloadedStrings, ExtendedDefaultRules, CPP #-}
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
import           Data.Aeson
import           Data.ByteString.Base64 as B64 (decodeLenient)
import qualified Data.Map as Map
import           Data.Text.Encoding (encodeUtf8)

import           Data.Foldable (foldl)
import           System.IO.Unsafe (unsafePerformIO)

import           IHaskell.Display
import           IHaskell.Eval.Util (unfoldM)
import           IHaskell.IPython.Types (showMessageType)
import           IHaskell.Types

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap as KM (lookup,insert,delete)
import qualified Data.Aeson.Key    as Key
#else
import qualified Data.HashMap.Strict as HM (lookup,insert,delete)
#endif

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

-- | Send a `clear_output` message
widgetClearOutput :: Bool -> IO ()
widgetClearOutput w = queue $ ClrOutput w

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

          (newvalue,buffers,bp) = processBPs value $ getBufferPaths widget
          applyBuffers x = x {mhBuffers = buffers}
          content = object [ "state" .= newvalue, "buffer_paths" .= bp ]

          communicate val = do
            head <- applyBuffers <$> dupHeader replyHeader CommDataMessage
            send $ CommData head uuid val

      -- If the widget is present, don't open it again.
      if present
        then return state
        else do
          -- Send the comm open, with the initial state
          hdr <- applyBuffers <$> dupHeader replyHeader CommOpenMessage
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

    Update widget value -> do
      let (newvalue,buffers,bp) = processBPs value $ getBufferPaths widget
      sendMessageHdr widget (toJSON $ UpdateState newvalue bp) (\h->h {mhBuffers=buffers})

    Custom widget value -> sendMessage widget (toJSON $ CustomContent value)

    JSONValue widget value -> sendMessage widget value

    DispMsg widget disp -> do
      dispHeader <- dupHeader replyHeader DisplayDataMessage
      let dmsg = WidgetDisplay dispHeader $ unwrap disp
      sendMessage widget (toJSON $ CustomContent $ toJSON dmsg)

    ClrOutput w -> do
      hdr <- dupHeader replyHeader ClearOutputMessage
      send $ ClearOutput hdr w
      return state

  where
    oldComms = openComms state
    sendMessage widget value = sendMessageHdr widget value id
    sendMessageHdr widget value hdrf = do
      let uuid = getCommUUID widget
          present = isJust $ Map.lookup uuid oldComms

      -- If the widget is present, we send an update message on its comm.
      when present $ do
        hdr <- hdrf <$> dupHeader replyHeader CommDataMessage
        send $ CommData hdr uuid value
      return state

    unwrap :: Display -> [DisplayData]
    unwrap (ManyDisplay ds) = concatMap unwrap ds
    unwrap (Display ddatas) = ddatas

    -- Removes the values that are buffers and puts them in the third value of the tuple
    -- The returned bufferpaths are the bufferpaths used
    processBPs :: Value -> [BufferPath] -> (Value, [ByteString], [BufferPath])
    -- Searching if the BufferPath key is in the Object is O(log n) or O(1) depending on implementation
    -- For this reason we fold on the bufferpaths
    processBPs val = foldl f (val,[],[])
      where
#if MIN_VERSION_aeson(2,0,0)
        nestedLookupRemove :: BufferPath -> Value -> (Value, Maybe Value)
        nestedLookupRemove [] v = (v,Just v)
        nestedLookupRemove [b] v =
          case v of
            Object o -> (Object $ KM.delete (Key.fromText b) o, KM.lookup (Key.fromText b) o)
            _ -> (v, Nothing)
        nestedLookupRemove (b:bp) v =
          case v of
            Object o -> maybe (v,Nothing) (upd . nestedLookupRemove bp) (KM.lookup (Key.fromText b) o)
            _ -> (v,Nothing)
            where upd :: (Value, Maybe Value) -> (Value, Maybe Value)
                  upd (Object v', Just (Object u)) = (Object $ KM.insert (Key.fromText b) (Object u) v', Just $ Object u)
                  upd r = r
#else
        nestedLookupRemove :: BufferPath -> Value -> (Value, Maybe Value)
        nestedLookupRemove [] v = (v,Just v)
        nestedLookupRemove [b] v =
          case v of
            Object o -> (Object $ HM.delete b o, HM.lookup b o)
            _ -> (v, Nothing)
        nestedLookupRemove (b:bp) v =
          case v of
            Object o -> maybe (v,Nothing) (upd . nestedLookupRemove bp) (HM.lookup b o)
            _ -> (v,Nothing)
            where upd :: (Value, Maybe Value) -> (Value, Maybe Value)
                  upd (Object v', Just (Object u)) = (Object $ HM.insert b (Object u) v', Just $ Object u)
                  upd r = r
#endif

        f :: (Value, [ByteString], [BufferPath]) -> BufferPath -> (Value, [ByteString], [BufferPath])
        f r@(v,bs,bps) bp =
          case nestedLookupRemove bp v of
            (newv, Just (String b)) -> (newv, B64.decodeLenient (encodeUtf8 b) : bs, bp:bps)
            _ -> r

-- Override toJSON for PublishDisplayData for sending Display messages through [method .= custom]
data WidgetDisplay = WidgetDisplay MessageHeader [DisplayData]

instance ToJSON WidgetDisplay where
  toJSON (WidgetDisplay replyHeader ddata) =
    let pbval = toJSON $ PublishDisplayData replyHeader ddata Nothing
    in toJSON $ IPythonMessage replyHeader pbval DisplayDataMessage

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
