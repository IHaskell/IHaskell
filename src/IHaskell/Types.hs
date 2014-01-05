{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
-- | Description : All message type definitions.
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
  InitInfo(..),
  KernelState(..),
  LintStatus(..),
  Width, Height,
  defaultKernelState
  ) where

import ClassyPrelude
import Data.Aeson
import IHaskell.Message.UUID
import Data.Serialize
import GHC.Generics (Generic)



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
  parseJSON _ = fail "Expecting JSON object."

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

-- | All state stored in the kernel between executions.
data KernelState = KernelState
  { getExecutionCounter :: Int,
    getLintStatus :: LintStatus,  -- Whether to use hlint, and what arguments to pass it. 
    getCwd :: String,
    useSvg :: Bool
  }

defaultKernelState :: KernelState
defaultKernelState = KernelState
  { getExecutionCounter = 1,
    getLintStatus = LintOn,
    getCwd = ".",
    useSvg = True
  }

-- | Initialization information for the kernel.
data InitInfo = InitInfo {
  extensions :: [String],   -- ^ Extensions to enable at start.
  initCells :: [String],    -- ^ Code blocks to run before start.
  initDir :: String         -- ^ Which directory this kernel should pretend to operate in.
  }
  deriving (Show, Read)

-- | Current HLint status.
data LintStatus
     = LintOn
     | LintOff
     deriving (Eq, Show)

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
                 | CompleteRequestMessage
                 | CompleteReplyMessage
                 | ObjectInfoRequestMessage
                 | ObjectInfoReplyMessage
                 | ShutdownRequestMessage
                 | ShutdownReplyMessage
                 | ClearOutputMessage

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
  show CompleteRequestMessage     = "complete_request"
  show CompleteReplyMessage       = "complete_reply"
  show ObjectInfoRequestMessage   = "object_info_request"
  show ObjectInfoReplyMessage     = "object_info_reply"
  show ShutdownRequestMessage     = "shutdown_request"
  show ShutdownReplyMessage       = "shutdown_reply"
  show ClearOutputMessage         = "clear_output"

instance FromJSON MessageType where
  parseJSON (String s) = case s of
    "kernel_info_reply"   -> return KernelInfoReplyMessage
    "kernel_info_request" -> return KernelInfoRequestMessage
    "execute_reply"       -> return ExecuteReplyMessage
    "execute_request"     -> return ExecuteRequestMessage
    "status"              -> return StatusMessage
    "stream"              -> return StreamMessage
    "display_data"        -> return DisplayDataMessage
    "pyout"               -> return OutputMessage
    "pyin"                -> return InputMessage
    "complete_request"    -> return CompleteRequestMessage
    "complete_reply"      -> return CompleteReplyMessage
    "object_info_request" -> return ObjectInfoRequestMessage
    "object_info_reply"   -> return ObjectInfoReplyMessage
    "shutdown_request"    -> return ShutdownRequestMessage
    "shutdown_reply"      -> return ShutdownReplyMessage
    "clear_output"        -> return ClearOutputMessage

    _                     -> fail ("Unknown message type: " ++ show s)
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

  | CompleteRequest {
      header :: MessageHeader,
      getCode :: ByteString, {- ^
            The entire block of text where the line is.  This may be useful in the
            case of multiline completions where more context may be needed.  Note: if
            in practice this field proves unnecessary, remove it to lighten the
            messages. json field @block@  -}
      getCodeLine :: ByteString, -- ^ just the line with the cursor. json field @line@
      getCursorPos :: Int -- ^ position of the cursor (index into the line?). json field @cursor_pos@

    }

  | CompleteReply {
     header :: MessageHeader,
     completionMatches :: [ByteString],
     completionMatchedText :: ByteString,
     completionText :: ByteString,
     completionStatus :: Bool
  }

  | ObjectInfoRequest {
      header :: MessageHeader, 
      objectName :: ByteString,  -- ^ Name of object being searched for.
      detailLevel :: Int         -- ^ Level of detail desired (defaults to 0).
                                -- 0 is equivalent to foo?, 1 is equivalent
                                -- to foo??.
    }

  | ObjectInfoReply {
      header :: MessageHeader, 
      objectName :: ByteString,       -- ^ Name of object which was searched for.
      objectFound :: Bool,            -- ^ Whether the object was found.
      objectTypeString :: ByteString, -- ^ Object type.
      objectDocString  :: ByteString
    }

  | ShutdownRequest {
      header :: MessageHeader,
      restartPending :: Bool    -- ^ Whether this shutdown precedes a restart.
    }
  | ShutdownReply {
      header :: MessageHeader,
      restartPending :: Bool    -- ^ Whether this shutdown precedes a restart.
    }

  | ClearOutput {
      header :: MessageHeader,
      wait :: Bool -- ^ Whether to wait to redraw until there is more output.
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
data DisplayData = Display MimeType String deriving (Typeable, Generic)

-- We can't print the actual data, otherwise this will be printed every
-- time it gets computed because of the way the evaluator is structured.
-- See how `displayExpr` is computed.
instance Show DisplayData where
  show _ = "Display"

-- Allow DisplayData serialization
instance Serialize DisplayData
instance Serialize MimeType

-- | Possible MIME types for the display data.
type Width = Int
type Height = Int
data MimeType = PlainText
              | MimeHtml
              | MimePng Width Height
              | MimeJpg Width Height
              | MimeSvg
              | MimeLatex
              deriving (Eq, Typeable, Generic)


instance Show MimeType where
  show PlainText = "text/plain"
  show MimeHtml  = "text/html"
  show (MimePng _ _)   = "image/png" 
  show (MimeJpg _ _)   = "image/jpeg"
  show MimeSvg   = "image/svg+xml"
  show MimeLatex = "text/latex"

-- | Input and output streams.
data StreamType = Stdin | Stdout deriving Show

-- | Get the reply message type for a request message type.
replyType :: MessageType -> MessageType
replyType KernelInfoRequestMessage = KernelInfoReplyMessage
replyType ExecuteRequestMessage = ExecuteReplyMessage
replyType CompleteRequestMessage = CompleteReplyMessage
replyType ObjectInfoRequestMessage = ObjectInfoReplyMessage
replyType ShutdownRequestMessage = ShutdownReplyMessage
replyType messageType = error $ "No reply for message type " ++ show messageType
