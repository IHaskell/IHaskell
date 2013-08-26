module IHaskell.MessageParser (parseMessage) where

import BasicPrelude
import Data.Aeson ((.:), decode, Result(..), Object)
import Data.Aeson.Types (parse)

import qualified Data.ByteString.Lazy as Lazy

import IHaskell.Types

parseHeader :: [ByteString] -> ByteString -> ByteString -> ByteString -> MessageHeader
parseHeader idents headerData parentHeader metadata = MessageHeader {
  identifiers = idents,
  parentHeader = parentResult,
  metadata = metadataMap,
  messageId = messageUUID,
  sessionId = sessionUUID,
  username = username,
  msgType = messageType
  } where
      -- Decode the header data and the parent header data into JSON objects.
      -- If the parent header data is absent, just have Nothing instead.
      Just result = decode $ Lazy.fromStrict headerData :: Maybe Object
      parentResult = if parentHeader == "{}"
                     then Nothing
                     else Just $ parseHeader idents parentHeader "{}" metadata

      -- Get the basic fields from the header.
      Success (messageType, username, messageUUID, sessionUUID) = flip parse result $ \obj -> do 
        messType <- obj .: "msg_type"
        username <- obj .: "username"
        message <- obj .: "msg_id"
        session <- obj .: "session"
        return (messType, username, message, session)

      -- Get metadata as a simple map.
      Just metadataMap = decode $ Lazy.fromStrict metadata :: Maybe (Map ByteString ByteString)


parseMessage :: [ByteString] -> ByteString -> ByteString -> ByteString -> ByteString -> Message
parseMessage idents headerData parentHeader metadata content = 
  let header = parseHeader idents headerData parentHeader metadata
      messageType = msgType header in
    parseMessageContent messageType header content

parseMessageContent :: MessageType -> MessageHeader -> ByteString -> Message
parseMessageContent "kernel_info_request" header _ = KernelInfoRequest header
parseMessageContent other _ _ = error $ "Unknown message type " ++ textToString (show other)

