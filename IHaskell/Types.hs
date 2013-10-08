module IHaskell.Types (
  Profile (..),
  Message (..),
  MessageHeader (..),
  MessageType,
  Username,
  Metadata,
  Port,
  replyType,
  ExecutionState (..),
  StreamType(..),
  MimeType(..),
  ) where

import BasicPrelude
import Data.Aeson
import Data.UUID (UUID)
import Data.ByteString.Char8 (unpack)
import Data.Char (toUpper)

import qualified Data.UUID as UUID (fromString, toString)

-- Allows reading and writing UUIDs as Strings in JSON. 
instance FromJSON UUID where
  parseJSON val@(String _) = do
    -- Parse the string into a String.
    str <- parseJSON val

    -- If there are no hyphens, insert them.
    -- This is an issue with IPython notebook.
    let hyphenated = if '-' `notElem` str
                     then hyphenate str
                     else str

    -- Attempt to parse string into UUID.
    case UUID.fromString hyphenated of
      Nothing -> fail $ "Could not parse UUID from " ++ hyphenated
      Just uuid -> return uuid

    where
      hyphenate str = concat [one, "-", two, "-", three, "-", four, "-", restFour]
        where
          (one, restOne) = splitAt 8 str
          (two, restTwo) = splitAt 4 restOne
          (three, restThree) = splitAt 4 restTwo
          (four, restFour) = splitAt 4 restThree

  -- UUIDs must be Strings.
  parseJSON _ = mzero

instance ToJSON UUID where
  -- Convert a UUID to [Char] and then to Text.
  toJSON = String . fromString . map toUpper . filter (/= '-') . UUID.toString

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
                    "msg_type" .= msgType header
                  ]

-- | A username for the source of a message.
type Username = ByteString

-- | A metadata dictionary.
type Metadata = Map ByteString ByteString

-- | The type of a message, currently just a string.
type MessageType = ByteString

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
  | ExecuteReply {
      header :: MessageHeader,
      status :: String,
      executionCounter :: Int
    }

  | IopubStatus {
      header :: MessageHeader,
      executionState :: ExecutionState
    }

  | IopubStream {
      header :: MessageHeader,
      streamType :: StreamType,
      streamContent :: String
    }

  | IopubDisplayData {
      header :: MessageHeader,
      source :: String,
      displayData :: [(MimeType, String)]
    }

  | IopubPythonOut {
      header :: MessageHeader,
      reprText :: String,
      executionCount :: Int
    }

  | IopubPythonIn {
      header :: MessageHeader,
      inCode :: String,
      executionCount :: Int
    }
    deriving Show

-- | The execution state of the kernel.
data ExecutionState = Busy | Idle | Starting deriving Show

-- | Possible MIME types for the display data.
data MimeType = PlainText | MimeHtml

instance Show MimeType where
  showsPrec prec PlainText str = str ++ "text/plain"
  showsPrec prec MimeHtml str = str ++ "text/html"

-- | Input and output streams.
data StreamType = Stdin | Stdout deriving Show

-- | Get the reply message type for a request message type.
replyType :: MessageType -> MessageType
replyType "kernel_info_request" = "kernel_info_reply"
replyType "execute_request" = "execute_reply"
replyType messageType = error $ "Unknown message type " ++ unpack messageType
