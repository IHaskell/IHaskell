{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, DeriveGeneric #-}

-- | This module contains all types used to create an IPython language kernel.
module IHaskell.IPython.Types (
    -- * IPython kernel profile
    Profile(..),
    Transport(..),
    Port(..),
    IP(..),

    -- * IPython kernelspecs
    KernelSpec(..),

    -- * IPython messaging protocol
    Message(..),
    MessageHeader(..),
    Username(..),
    Metadata(..),
    MessageType(..),
    Width(..),
    Height(..),
    StreamType(..),
    ExecutionState(..),
    ExecuteReplyStatus(..),
    HistoryAccessType(..),
    HistoryReplyElement(..),
    replyType,
    showMessageType,

    -- ** IPython display data message
    DisplayData(..),
    MimeType(..),
    extractPlain,
    ) where

import           Data.Aeson
import           Control.Applicative ((<$>), (<*>))
import           Data.ByteString (ByteString)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Text (Text)
import qualified Data.String as S
import           Data.Serialize
import           IHaskell.IPython.Message.UUID
import           GHC.Generics (Generic)
import           Data.Typeable
import           Data.List (find)
import           Data.Map (Map)

------------------ IPython Kernel Profile Types ----------------------
--
-- | A TCP port.
type Port = Int

-- | An IP address.
type IP = String

-- | The transport mechanism used to communicate with the IPython frontend.
data Transport = TCP -- ^ Default transport mechanism via TCP.
  deriving (Show, Read)

-- | A kernel profile, specifying how the kernel communicates.
data Profile =
       Profile
         { ip :: IP                     -- ^ The IP on which to listen.
         , transport :: Transport       -- ^ The transport mechanism.
         , stdinPort :: Port            -- ^ The stdin channel port.
         , controlPort :: Port          -- ^ The control channel port.
         , hbPort :: Port               -- ^ The heartbeat channel port.
         , shellPort :: Port            -- ^ The shell command port.
         , iopubPort :: Port            -- ^ The IOPub port.
         , signatureKey :: ByteString   -- ^ The HMAC encryption key.
         }
  deriving (Show, Read)

-- Convert the kernel profile to and from JSON.
instance FromJSON Profile where
  parseJSON (Object v) = do
    signatureScheme <- v .: "signature_scheme"
    case signatureScheme of
      "hmac-sha256" ->
        Profile <$> v .: "ip"
                <*> v .: "transport"
                <*> v .: "stdin_port"
                <*> v .: "control_port"
                <*> v .: "hb_port"
                <*> v .: "shell_port"
                <*> v .: "iopub_port"
                <*> (Text.encodeUtf8 <$> v .: "key")
      sig -> error $ "Unexpected signature scheme: " ++ sig
  parseJSON _ = fail "Expecting JSON object."

instance ToJSON Profile where
  toJSON profile = object
                     [ "ip" .= ip profile
                     , "transport" .= transport profile
                     , "stdin_port" .= stdinPort profile
                     , "control_port" .= controlPort profile
                     , "hb_port" .= hbPort profile
                     , "shell_port" .= shellPort profile
                     , "iopub_port" .= iopubPort profile
                     , "key" .= Text.decodeUtf8 (signatureKey profile)
                     ]

instance FromJSON Transport where
  parseJSON (String mech) =
    case mech of
      "tcp" -> return TCP
      _     -> fail $ "Unknown transport mechanism " ++ Text.unpack mech
  parseJSON _ = fail "Expected JSON string as transport."

instance ToJSON Transport where
  toJSON TCP = String "tcp"

-------------------- IPython Kernelspec Types ----------------------
data KernelSpec =
       KernelSpec
         { 
         -- | Name shown to users to describe this kernel (e.g. "Haskell")
         kernelDisplayName :: String
         -- | Name for the kernel; unique kernel identifier (e.g. "haskell")
         , kernelLanguage :: String
         -- | Command to run to start the kernel. One of the strings maybe @"{connection_file}"@, which will
         -- be replaced by the path to a kernel profile file (see @Profile@) when the command is run.
         , kernelCommand :: [String]
         }
  deriving (Eq, Show)

instance ToJSON KernelSpec where
  toJSON kernelspec = object
                        [ "argv" .= kernelCommand kernelspec
                        , "display_name" .= kernelDisplayName kernelspec
                        , "language" .= kernelLanguage kernelspec
                        ]

------------------ IPython Message Types --------------------
--
-- | A message header with some metadata.
data MessageHeader =
       MessageHeader
         { identifiers :: [ByteString]          -- ^ The identifiers sent with the message.
         , parentHeader :: Maybe MessageHeader  -- ^ The parent header, if present.
         , metadata :: Metadata                 -- ^ A dict of metadata.
         , messageId :: UUID                    -- ^ A unique message UUID.
         , sessionId :: UUID                    -- ^ A unique session UUID.
         , username :: Username                 -- ^ The user who sent this message.
         , msgType :: MessageType               -- ^ The message type.
         }
  deriving (Show, Read)

-- Convert a message header into the JSON field for the header. This field does not actually have
-- all the record fields.
instance ToJSON MessageHeader where
  toJSON header = object
                    [ "msg_id" .= messageId header
                    , "session" .= sessionId header
                    , "username" .= username header
                    , "version" .= ("5.0" :: String)
                    , "msg_type" .= showMessageType (msgType header)
                    ]

-- | A username for the source of a message.
type Username = Text

-- | A metadata dictionary.
type Metadata = Map Text Text

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
                 | InspectRequestMessage
                 | InspectReplyMessage
                 | ShutdownRequestMessage
                 | ShutdownReplyMessage
                 | ClearOutputMessage
                 | InputRequestMessage
                 | InputReplyMessage
                 | CommOpenMessage
                 | CommDataMessage
                 | CommCloseMessage
                 | HistoryRequestMessage
                 | HistoryReplyMessage
  deriving (Show, Read, Eq)

showMessageType :: MessageType -> String
showMessageType KernelInfoReplyMessage = "kernel_info_reply"
showMessageType KernelInfoRequestMessage = "kernel_info_request"
showMessageType ExecuteReplyMessage = "execute_reply"
showMessageType ExecuteRequestMessage = "execute_request"
showMessageType StatusMessage = "status"
showMessageType StreamMessage = "stream"
showMessageType DisplayDataMessage = "display_data"
showMessageType OutputMessage = "pyout"
showMessageType InputMessage = "pyin"
showMessageType CompleteRequestMessage = "complete_request"
showMessageType CompleteReplyMessage = "complete_reply"
showMessageType InspectRequestMessage = "inspect_request"
showMessageType InspectReplyMessage = "inspect_reply"
showMessageType ShutdownRequestMessage = "shutdown_request"
showMessageType ShutdownReplyMessage = "shutdown_reply"
showMessageType ClearOutputMessage = "clear_output"
showMessageType InputRequestMessage = "input_request"
showMessageType InputReplyMessage = "input_reply"
showMessageType CommOpenMessage = "comm_open"
showMessageType CommDataMessage = "comm_msg"
showMessageType CommCloseMessage = "comm_close"
showMessageType HistoryRequestMessage = "history_request"
showMessageType HistoryReplyMessage = "history_reply"

instance FromJSON MessageType where
  parseJSON (String s) =
    case s of
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
      "inspect_request"     -> return InspectRequestMessage
      "inspect_reply"       -> return InspectReplyMessage
      "shutdown_request"    -> return ShutdownRequestMessage
      "shutdown_reply"      -> return ShutdownReplyMessage
      "clear_output"        -> return ClearOutputMessage
      "input_request"       -> return InputRequestMessage
      "input_reply"         -> return InputReplyMessage
      "comm_open"           -> return CommOpenMessage
      "comm_msg"            -> return CommDataMessage
      "comm_close"          -> return CommCloseMessage
      "history_request"     -> return HistoryRequestMessage
      "history_reply"       -> return HistoryReplyMessage

      _                     -> fail ("Unknown message type: " ++ show s)
  parseJSON _ = fail "Must be a string."

-- | A message used to communicate with the IPython frontend.
data Message =
             -- | A request from a frontend for information about the kernel.
              KernelInfoRequest { header :: MessageHeader }
             |
             -- | A response to a KernelInfoRequest.
               KernelInfoReply
                 { header :: MessageHeader
                 , versionList :: [Int]      -- ^ The version of the language, e.g. [7, 6, 3] for GHC
                                             -- 7.6.3
                 , language :: String        -- ^ The language name, e.g. "haskell"
                 }
             |
             -- | A request from a frontend to execute some code.
               ExecuteRequest
                 { header :: MessageHeader
                 , getCode :: Text              -- ^ The code string.
                 , getSilent :: Bool                  -- ^ Whether this should be silently executed.
                 , getStoreHistory :: Bool            -- ^ Whether to store this in history.
                 , getAllowStdin :: Bool              -- ^ Whether this code can use stdin.
                 , getUserVariables :: [Text]   -- ^ Unused.
                 , getUserExpressions :: [Text] -- ^ Unused.
                 }
             |
             -- | A reply to an execute request.
               ExecuteReply
                 { header :: MessageHeader
                 , status :: ExecuteReplyStatus          -- ^ The status of the output.
                 , pagerOutput :: [DisplayData]          -- ^ The mimebundles to display in the pager.
                 , executionCounter :: Int               -- ^ The execution count, i.e. which output this is.
                 }
             |
               PublishStatus
                 { header :: MessageHeader
                 , executionState :: ExecutionState      -- ^ The execution state of the kernel.
                 }
             |
               PublishStream
                 { header :: MessageHeader
                 , streamType :: StreamType              -- ^ Which stream to publish to.
                 , streamContent :: String               -- ^ What to publish.
                 }
             |
               PublishDisplayData
                 { header :: MessageHeader
                 , source :: String                      -- ^ The name of the data source.
                 , displayData :: [DisplayData]          -- ^ A list of data representations.
                 }
             |
               PublishOutput
                 { header :: MessageHeader
                 , reprText :: String                    -- ^ Printed output text.
                 , executionCount :: Int                 -- ^ Which output this is for.
                 }
             |
               PublishInput
                 { header :: MessageHeader
                 , inCode :: String                      -- ^ Submitted input code.
                 , executionCount :: Int                 -- ^ Which input this is.
                 }
             |
               CompleteRequest
                 { header :: MessageHeader
                 , getCode :: Text  {- ^
            The entire block of text where the line is. This may be useful in the
            case of multiline completions where more context may be needed.  Note: if
            in practice this field proves unnecessary, remove it to lighten the
            messages. json field @code@  -}
                 , getCursorPos :: Int -- ^ Position of the cursor in unicode characters. json field
                                       -- @cursor_pos@
                 }
             |
               CompleteReply
                 { header :: MessageHeader
                 , completionMatches :: [Text]
                 , completionCursorStart :: Int
                 , completionCursorEnd :: Int
                 , completionMetadata :: Metadata
                 , completionStatus :: Bool
                 }
             |
               InspectRequest
                 { header :: MessageHeader
                 -- | The code context in which introspection is requested
                 , inspectCode :: Text
                 -- | Position of the cursor in unicode characters. json field @cursor_pos@
                 , inspectCursorPos :: Int
                 -- | Level of detail desired (defaults to 0). 0 is equivalent to foo?, 1 is equivalent to foo??.
                 , detailLevel :: Int
                 }
             |
               InspectReply
                 { header :: MessageHeader
                 -- | whether the request succeeded or failed
                 , inspectStatus :: Bool
                 -- | @inspectData@ can be empty if nothing is found
                 , inspectData :: [DisplayData]
                 }
             |
               ShutdownRequest
                 { header :: MessageHeader
                 , restartPending :: Bool    -- ^ Whether this shutdown precedes a restart.
                 }
             |
               ShutdownReply
                 { header :: MessageHeader
                 , restartPending :: Bool    -- ^ Whether this shutdown precedes a restart.
                 }
             |
               ClearOutput
                 { header :: MessageHeader
                 , wait :: Bool -- ^ Whether to wait to redraw until there is more output.
                 }
             | RequestInput { header :: MessageHeader, inputPrompt :: String }
             | InputReply { header :: MessageHeader, inputValue :: String }
             |
               CommOpen
                 { header :: MessageHeader
                 , commTargetName :: String
                 , commUuid :: UUID
                 , commData :: Value
                 }
             | CommData { header :: MessageHeader, commUuid :: UUID, commData :: Value }
             | CommClose { header :: MessageHeader, commUuid :: UUID, commData :: Value }
             |
               HistoryRequest
                 { header :: MessageHeader
                 , historyGetOutput :: Bool  -- ^ If True, also return output history in the resulting
                                             -- dict.
                 , historyRaw :: Bool        -- ^ If True, return the raw input history, else the
                                             -- transformed input.
                 , historyAccessType :: HistoryAccessType -- ^ What history is being requested.
                 }
             | HistoryReply { header :: MessageHeader, historyReply :: [HistoryReplyElement] }
             | SendNothing -- Dummy message; nothing is sent.
  deriving Show

-- | Ways in which the frontend can request history. TODO: Implement fields as described in
-- messaging spec.
data HistoryAccessType = HistoryRange
                       | HistoryTail
                       | HistorySearch
  deriving (Eq, Show)

-- | Reply to history requests.
data HistoryReplyElement =
       HistoryReplyElement
         { historyReplySession :: Int
         , historyReplyLineNumber :: Int
         , historyReplyContent :: Either String (String, String)
         }
  deriving (Eq, Show)

-- | Possible statuses in the execution reply messages.
data ExecuteReplyStatus = Ok
                        | Err
                        | Abort

instance Show ExecuteReplyStatus where
  show Ok = "ok"
  show Err = "error"
  show Abort = "abort"

-- | The execution state of the kernel.
data ExecutionState = Busy
                    | Idle
                    | Starting
  deriving Show

-- | Input and output streams.
data StreamType = Stdin
                | Stdout
  deriving Show

-- | Get the reply message type for a request message type.
replyType :: MessageType -> Maybe MessageType
replyType KernelInfoRequestMessage = Just KernelInfoReplyMessage
replyType ExecuteRequestMessage = Just ExecuteReplyMessage
replyType CompleteRequestMessage = Just CompleteReplyMessage
replyType InspectRequestMessage = Just InspectReplyMessage
replyType ShutdownRequestMessage = Just ShutdownReplyMessage
replyType HistoryRequestMessage = Just HistoryReplyMessage
replyType _ = Nothing

-- | Data for display: a string with associated MIME type.
data DisplayData = DisplayData MimeType Text
  deriving (Typeable, Generic)

-- We can't print the actual data, otherwise this will be printed every time it gets computed
-- because of the way the evaluator is structured. See how `displayExpr` is computed.
instance Show DisplayData where
  show _ = "DisplayData"

-- Allow DisplayData serialization
instance Serialize Text where
  put str = put (Text.encodeUtf8 str)
  get = Text.decodeUtf8 <$> get

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
              | MimeJavascript
  deriving (Eq, Typeable, Generic)

-- Extract the plain text from a list of displays.
extractPlain :: [DisplayData] -> String
extractPlain disps =
  case find isPlain disps of
    Nothing                              -> ""
    Just (DisplayData PlainText bytestr) -> Text.unpack bytestr
  where
    isPlain (DisplayData mime _) = mime == PlainText

instance Show MimeType where
  show PlainText = "text/plain"
  show MimeHtml = "text/html"
  show (MimePng _ _) = "image/png"
  show (MimeJpg _ _) = "image/jpeg"
  show MimeSvg = "image/svg+xml"
  show MimeLatex = "text/latex"
  show MimeJavascript = "application/javascript"
