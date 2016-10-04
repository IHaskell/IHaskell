{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-name-shadowing -fno-warn-unused-matches #-}

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
    CodeReview(..),
    Width(..),
    Height(..),
    StreamType(..),
    ExecutionState(..),
    ExecuteReplyStatus(..),
    HistoryAccessType(..),
    HistoryReplyElement(..),
    LanguageInfo(..),
    replyType,
    showMessageType,

    -- ** IPython display data message
    DisplayData(..),
    MimeType(..),
    extractPlain,
    ) where

import           Control.Applicative ((<$>), (<*>))
import           Data.Aeson
import           Data.ByteString (ByteString)
import           Data.List (find)
import           Data.Map (Map)
import           Data.Serialize
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Typeable
import           GHC.Generics (Generic)
import           IHaskell.IPython.Message.UUID

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
                 | ExecuteInputMessage
                 | ExecuteReplyMessage
                 | ExecuteErrorMessage
                 | ExecuteRequestMessage
                 | ExecuteResultMessage
                 | StatusMessage
                 | StreamMessage
                 | DisplayDataMessage
                 | OutputMessage
                 | InputMessage
                 | IsCompleteRequestMessage
                 | IsCompleteReplyMessage
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
                 | CommInfoRequestMessage
                 | CommInfoReplyMessage
                 | CommCloseMessage
                 | HistoryRequestMessage
                 | HistoryReplyMessage
  deriving (Show, Read, Eq)

showMessageType :: MessageType -> String
showMessageType KernelInfoReplyMessage = "kernel_info_reply"
showMessageType KernelInfoRequestMessage = "kernel_info_request"
showMessageType ExecuteInputMessage = "execute_input"
showMessageType ExecuteReplyMessage = "execute_reply"
showMessageType ExecuteErrorMessage = "error"
showMessageType ExecuteRequestMessage = "execute_request"
showMessageType ExecuteResultMessage = "execute_result"
showMessageType StatusMessage = "status"
showMessageType StreamMessage = "stream"
showMessageType DisplayDataMessage = "display_data"
showMessageType OutputMessage = "pyout"
showMessageType InputMessage = "pyin"
showMessageType IsCompleteRequestMessage = "is_complete_request"
showMessageType IsCompleteReplyMessage = "is_complete_reply"
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
showMessageType CommInfoRequestMessage = "comm_info_request"
showMessageType CommInfoReplyMessage = "comm_info_reply"
showMessageType CommCloseMessage = "comm_close"
showMessageType HistoryRequestMessage = "history_request"
showMessageType HistoryReplyMessage = "history_reply"

instance FromJSON MessageType where
  parseJSON (String s) =
    case s of
      "kernel_info_reply"   -> return KernelInfoReplyMessage
      "kernel_info_request" -> return KernelInfoRequestMessage
      "execute_input"       -> return ExecuteInputMessage
      "execute_reply"       -> return ExecuteReplyMessage
      "error"               -> return ExecuteErrorMessage
      "execute_request"     -> return ExecuteRequestMessage
      "execute_result"      -> return ExecuteResultMessage
      "status"              -> return StatusMessage
      "stream"              -> return StreamMessage
      "display_data"        -> return DisplayDataMessage
      "pyout"               -> return OutputMessage
      "pyin"                -> return InputMessage
      "is_complete_request" -> return IsCompleteRequestMessage
      "is_complete_reply"   -> return IsCompleteReplyMessage
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
      "comm_info_request"   -> return CommInfoRequestMessage
      "comm_info_reply"     -> return CommInfoReplyMessage
      "comm_close"          -> return CommCloseMessage
      "history_request"     -> return HistoryRequestMessage
      "history_reply"       -> return HistoryReplyMessage
      "status_message"      -> return StatusMessage

      _                     -> fail ("Unknown message type: " ++ show s)
  parseJSON _ = fail "Must be a string."

data LanguageInfo =
       LanguageInfo
         { languageName :: String        -- ^ The language name, e.g. "haskell"
         , languageVersion :: String        -- ^ GHC 7.6.3
         , languageFileExtension :: String        -- ^ .hs
         , languageCodeMirrorMode :: String        -- ^ 'ihaskell'. can be 'null'
         }
  deriving (Show, Eq)

data CodeReview = CodeComplete
                | CodeIncomplete String -- ^ String to be used to indent next line of input
                | CodeInvalid
                | CodeUnknown
  deriving Show

-- | A message used to communicate with the IPython frontend.
data Message =
             -- | A request from a frontend for information about the kernel.
              KernelInfoRequest { header :: MessageHeader }
             |
             -- | A response to a KernelInfoRequest.
               KernelInfoReply
                 { header :: MessageHeader
                 , protocolVersion :: String -- ^ current protocol version, major and minor
                 , banner :: String -- ^ Kernel information description e.g. (IHaskell 0.8.3.0 GHC
                                    -- 7.10.2)
                 , implementation :: String -- ^ e.g. IHaskell
                 , implementationVersion :: String -- ^ The version of the implementation
                 , languageInfo :: LanguageInfo
                 }
             |
             -- | A request from a frontend for information about the comms.
               CommInfoRequest { header :: MessageHeader }
             |
             -- | A response to a CommInfoRequest.
               CommInfoReply
                 { header   :: MessageHeader
                 , commInfo :: Map String String -- ^ A dictionary of the comms, indexed by uuids.
                 }
             |
             -- | A request from a frontend to execute some code.
               ExecuteInput
                 { header :: MessageHeader
                 , getCode :: Text         -- ^ The code string.
                 , executionCounter :: Int -- ^ The execution count, i.e. which output this is.
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
             -- | A reply to an execute request.
               ExecuteResult
                 { header :: MessageHeader
                 , dataResult :: [DisplayData]           -- ^ Key/value pairs (keys are MIME types)
                 , metadataResult :: Map String String   -- ^ Any metadata that describes the data
                 , executionCounter :: Int               -- ^ The execution count, i.e. which output this is.
                 }
             |
             -- | An error reply to an execute request
               ExecuteError
                 { header :: MessageHeader
                 , pagerOutput :: [DisplayData]          -- ^ The mimebundles to display in the pager.
                 , traceback :: [Text]
                 , ename :: Text
                 , evalue :: Text
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
             | Input { header :: MessageHeader, getCode :: Text, executionCount :: Int }
             | Output { header :: MessageHeader, getText :: [DisplayData], executionCount :: Int }
             |
               IsCompleteRequest
                 { header :: MessageHeader
                 , inputToReview :: String               -- ^ The code entered in the repl.
                 }
             |
               IsCompleteReply
                 { header :: MessageHeader
                 , reviewResult :: CodeReview            -- ^ The result of reviewing the code.
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
                 , commTargetModule :: String
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

instance FromJSON ExecuteReplyStatus where
  parseJSON (String "ok") = return Ok
  parseJSON (String "error") = return Err
  parseJSON (String "abort") = return Abort

instance Show ExecuteReplyStatus where
  show Ok = "ok"
  show Err = "error"
  show Abort = "abort"

-- | The execution state of the kernel.
data ExecutionState = Busy
                    | Idle
                    | Starting
  deriving Show

instance FromJSON ExecutionState where
  parseJSON (String "busy") = return Busy
  parseJSON (String "idle") = return Idle
  parseJSON (String "starting") = return Starting

-- | Input and output streams.
data StreamType = Stdin
                | Stdout
                | Stderr
  deriving Show

instance FromJSON StreamType where
  parseJSON (String "stdin") = return Stdin
  parseJSON (String "stdout") = return Stdout
  parseJSON (String "stderr") = return Stderr

-- | Get the reply message type for a request message type.
replyType :: MessageType -> Maybe MessageType
replyType KernelInfoRequestMessage = Just KernelInfoReplyMessage
replyType ExecuteRequestMessage = Just ExecuteReplyMessage
replyType IsCompleteRequestMessage = Just IsCompleteReplyMessage
replyType CompleteRequestMessage = Just CompleteReplyMessage
replyType InspectRequestMessage = Just InspectReplyMessage
replyType ShutdownRequestMessage = Just ShutdownReplyMessage
replyType HistoryRequestMessage = Just HistoryReplyMessage
replyType CommOpenMessage = Just CommDataMessage
replyType CommInfoRequestMessage = Just CommInfoReplyMessage
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

instance Read MimeType where
  readsPrec _ "text/plain" = [(PlainText, "")]
  readsPrec _ "text/html" = [(MimeHtml, "")]
  readsPrec _ "image/png" = [(MimePng 50 50, "")]
  readsPrec _ "image/jpg" = [(MimeJpg 50 50, "")]
  readsPrec _ "image/svg+xml" = [(MimeSvg, "")]
  readsPrec _ "text/latex" = [(MimeLatex, "")]
  readsPrec _ "application/javascript" = [(MimeJavascript, "")]
