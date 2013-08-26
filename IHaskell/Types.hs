{-# LANGUAGE TemplateHaskell #-}
module IHaskell.Types (
  Profile (..),
  Message (..),
  MessageHeader (..),
  MessageBody (..),
  MessageType,
  Username,
  Metadata,
  Port,
  messageTypeForBody
  ) where

import BasicPrelude

import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Data.String.Utils (replace)
import Data.UUID (UUID)
import qualified Data.UUID as UUID (fromString, toString)

instance FromJSON UUID where
  parseJSON val@(String _) = do
    str <- parseJSON val
    case UUID.fromString str of
      Nothing -> fail $ "Could not parse UUID from " ++ str
      Just uuid -> return uuid
  parseJSON _ = mzero

instance ToJSON UUID where
  toJSON = String . fromString . UUID.toString

-- A kernel profile
type Port = Int
data Profile = Profile {
  ip :: String,
  transport :: String,
  stdinPort :: Port,
  controlPort :: Port,
  hbPort :: Port,
  shellPort :: Port,
  iopubPort :: Port,
  key :: ByteString
  }
$(deriveJSON (replace "Port" "_port") ''Profile)

data MessageHeader = MessageHeader {
  identifiers :: [ByteString],
  parentHeader :: Maybe MessageHeader,
  metadata :: Metadata,
  messageId :: UUID,
  sessionId :: UUID,
  username :: Username,
  msgType :: MessageType
  } deriving Show

instance ToJSON MessageHeader where
  toJSON header = object [
                    "msg_id"  .= messageId header,
                    "session" .= sessionId header,
                    "username" .= username header,
                    "msg_type" .= msgType header
                  ]

data Message = Message {
  header :: MessageHeader,
  body :: MessageBody
  } deriving Show
data MessageBody = KernelInfoRequest |
                   KernelInfoReply |
                   ExecuteRequest {
                     getCode :: ByteString,
                     isSilent :: Bool,
                     storeHistory :: Bool,
                     allowStdin :: Bool,
                     getUserVariables :: ByteString,
                     getUserExpressions :: ByteString
                   } deriving Show

messageTypeForBody :: MessageBody -> ByteString
messageTypeForBody KernelInfoRequest = "kernel_info_request"
messageTypeForBody KernelInfoReply = "kernel_info_reply"
messageTypeForBody _ = error "Unknown message type"

instance ToJSON MessageBody where
  toJSON KernelInfoReply = object [
                             "protocol_version" .= [4, 0 :: Int], -- current protocol version, major and minor
                             "language_version" .= [7, 6, 2 :: Int],
                             "language" .= ("haskell" :: String)
                           ]
  toJSON body = error $ "Do not know how to convert to JSON for message" ++ textToString (show body)
type MessageType = ByteString

type Username = ByteString
type Metadata = Map ByteString ByteString
