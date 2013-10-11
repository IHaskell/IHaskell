module IHaskell.Types (
  Profile (..),
  Message (..),
  MessageHeader (..),
  MessageType(..),
  Username,
  Metadata,
  Port,
  replyType,
  ExecutionState (..),
  StreamType(..),
  MimeType(..),
  DisplayData(..),
  ExecuteReplyStatus(..),
  ) where

import ClassyPrelude
import Data.Aeson
import IHaskell.Message.UUID


-- | A TCP port.
type Port = Int

-- | A kernel profile, specifying how the kernel communicates.
data Profile = Profile {
  ip :: String,        -- ^ The IP on which to listen.
  transport :: String, -- ^ The transport mechanism.
  stdinPort :: Port,   -- ^ The stdin channel port. 
  controlPort :: Port, -- ^ The control channel port.
  hbPort :: Port,      -- ^ The heartbeat channel port.
  shellPort :: Port,   -- ^ The shell command port.
  iopubPort :: Port,   -- ^ The Iopub port.
  key :: ByteString    -- ^ The HMAC encryption key.
  } deriving Show

-- Convert the kernel profile to and from JSON.
instance FromJSON Profile where
  parseJSON (Object v) = 
    Profile <$> v .: "ip"
            <*> v .: "transport"
            <*> v .: "stdin_port"
            <*> v .: "control_port"
            <*> v .: "hb_port"
            <*> v .: "shell_port"
            <*> v .: "iopub_port"
            <*> v .: "key"
instance ToJSON Profile where
  toJSON profile = object [
                    "ip"          .= ip profile,
                    "transport"   .= transport profile,
                    "stdin_port"  .= stdinPort profile,
                    "control_port".= controlPort profile,
                    "hb_port"     .= hbPort profile,
                    "shell_port"  .= shellPort profile,
                    "iopub_port"  .= iopubPort profile,
                    "key"         .= key profile
                   ]

-- | A message header with some metadata.  
data MessageHeader = MessageHeader {
  identifiers :: [ByteString],         -- ^ The identifiers sent with the message.
  parentHeader :: Maybe MessageHeader, -- ^ The parent header, if present.
  metadata :: Metadata,                -- ^ A dict of metadata.
  messageId :: UUID,                   -- ^ A unique message UUID.
  sessionId :: UUID,                   -- ^ A unique session UUID.
  username :: Username,                -- ^ The user who sent this message.
  msgType :: MessageType               -- ^ The message type.
  } deriving Show

-- Convert a message header into the JSON field for the header.
-- This field does not actually have all the record fields.
instance ToJSON MessageHeader where
  toJSON header = object [
                    "msg_id"  .= messageId header,
                    "session" .= sessionId header,
                    "username" .= username header,
                    "msg_type" .= show (msgType header)
                  ]

-- | A username for the source of a message.
type Username = ByteString

-- | A metadata dictionary.
type Metadata = Map ByteString ByteString

-- | The type of a message, corresponding to IPython message types.
data MessageType = KernelInfoReplyMessage
                 | KernelInfoRequestMessage
                 | ExecuteReplyMessage
                 | ExecuteRequestMessage
                 | StatusMessage
                 | StreamMessage
                 | DisplayDataMessage
                 | OutputMessage
                 | InputMessage

instance Show MessageType where
  show KernelInfoReplyMessage     = "kernel_info_reply"
  show KernelInfoRequestMessage   = "kernel_info_request"
  show ExecuteReplyMessage        = "execute_reply"
  show ExecuteRequestMessage      = "execute_request"
  show StatusMessage              = "status"
  show StreamMessage              = "stream"
  show DisplayDataMessage         = "display_data"
  show OutputMessage              = "pyout"
  show InputMessage               = "pyin"

instance FromJSON MessageType where
  parseJSON (String s) = return $ case s of
    "kernel_info_reply"   -> KernelInfoReplyMessage
    "kernel_info_request" -> KernelInfoRequestMessage
    "execute_reply"       -> ExecuteReplyMessage
    "execute_request"     -> ExecuteRequestMessage
    "status"              -> StatusMessage
    "stream"              -> StreamMessage
    "display_data"        -> DisplayDataMessage
    "pyout"               -> OutputMessage
    "pyin"                -> InputMessage
  parseJSON _ = fail "Must be a string."


-- | A message used to communicate with the IPython frontend.
data Message 
  -- | A request from a frontend for information about the kernel.
  = KernelInfoRequest { header :: MessageHeader }
  -- | A response to a KernelInfoRequest.
  | KernelInfoReply { header :: MessageHeader }
               
  -- | A request from a frontend to execute some code.
  | ExecuteRequest {
      header :: MessageHeader,
      getCode :: ByteString,             -- ^ The code string.
      getSilent :: Bool,                 -- ^ Whether this should be silently executed.
      getStoreHistory :: Bool,           -- ^ Whether to store this in history.
      getAllowStdin :: Bool,             -- ^ Whether this code can use stdin.

      getUserVariables :: [ByteString],  -- ^ Unused.
      getUserExpressions :: [ByteString] -- ^ Unused.
    }

-- | A reply to an execute request.
  | ExecuteReply {
      header :: MessageHeader,
      status :: ExecuteReplyStatus,         -- ^ The status of the output.
      executionCounter :: Int               -- ^ The execution count, i.e. which output this is.
    }

  | PublishStatus {
      header :: MessageHeader,
      executionState :: ExecutionState      -- ^ The execution state of the kernel.
    }

  | PublishStream {
      header :: MessageHeader,
      streamType :: StreamType,             -- ^ Which stream to publish to.
      streamContent :: String               -- ^ What to publish.
    }

  | PublishDisplayData {
      header :: MessageHeader,
      source :: String,                     -- ^ The name of the data source.
      displayData :: [DisplayData]          -- ^ A list of data representations.
    }

  | PublishOutput {
      header :: MessageHeader,
      reprText :: String,                   -- ^ Printed output text.
      executionCount :: Int                 -- ^ Which output this is for.
    }

  | PublishInput {
      header :: MessageHeader,
      inCode :: String,                     -- ^ Submitted input code.
      executionCount :: Int                 -- ^ Which input this is.
    }
    deriving Show

-- | Possible statuses in the execution reply messages.
data ExecuteReplyStatus = Ok | Err | Abort

instance Show ExecuteReplyStatus where
  show Ok = "ok"
  show Err = "error"
  show Abort = "abort"

-- | The execution state of the kernel.
data ExecutionState = Busy | Idle | Starting deriving Show

-- | Data for display: a string with associated MIME type.
data DisplayData = Display MimeType String deriving Show

-- | Possible MIME types for the display data.
data MimeType = PlainText | MimeHtml deriving Eq

instance Show MimeType where
  show PlainText = "text/plain"
  show MimeHtml  = "text/html"

-- | Input and output streams.
data StreamType = Stdin | Stdout deriving Show

-- | Get the reply message type for a request message type.
replyType :: MessageType -> MessageType
replyType KernelInfoRequestMessage = KernelInfoReplyMessage
replyType ExecuteRequestMessage = ExecuteReplyMessage
replyType messageType = error $ "No reply for message type " ++ show messageType
