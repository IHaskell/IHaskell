module IHaskell.Types (
  Profile (..),
  Message (..),
  MessageHeader (..),
  MessageType,
  Username,
  Metadata,
  Port,
  replyType
  ) where

import BasicPrelude
import Data.Aeson
import Data.UUID (UUID)
import Data.ByteString.Char8 (unpack)

import qualified Data.UUID as UUID (fromString, toString)

-- Allos reading and writing UUIDs as Strings in JSON. 
instance FromJSON UUID where
  parseJSON val@(String _) = do
    -- Parse the string into a String.
    str <- parseJSON val

    -- Attempt to parse string into UUID.
    case UUID.fromString str of
      Nothing -> fail $ "Could not parse UUID from " ++ str
      Just uuid -> return uuid

  -- UUIDs must be Strings.
  parseJSON _ = mzero

instance ToJSON UUID where
  -- Convert a UUID to [Char] and then to Text.
  toJSON = String . fromString . UUID.toString

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
data Message = KernelInfoRequest { header :: MessageHeader } |
               KernelInfoReply { header :: MessageHeader }
               deriving Show

-- | Get the reply message type for a request message type.
replyType :: MessageType -> MessageType
replyType "kernel_info_request" = "kernel_info_reply"
replyType messageType = error $ "Unknown message type" ++ unpack messageType

-- Convert message bodies into JSON.
instance ToJSON Message where
  toJSON KernelInfoReply{} = object [
                             "protocol_version" .= [4, 0 :: Int], -- current protocol version, major and minor
                             "language_version" .= [7, 6, 2 :: Int],
                             "language" .= ("haskell" :: String)
                           ]
  toJSON body = error $ "Do not know how to convert to JSON for message" ++ textToString (show body)
