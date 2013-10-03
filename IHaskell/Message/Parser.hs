-- | This module is responsible for converting from low-level ByteStrings
-- | obtained from the 0MQ sockets into Messages. The only exposed function is
-- | `parseMessage`, which should only be used in the low-level 0MQ interface.
module IHaskell.Message.Parser (parseMessage) where

import BasicPrelude
import Data.Aeson ((.:), decode, Result(..), Object)
import Data.Aeson.Types (parse)

import qualified Data.ByteString.Lazy as Lazy

import IHaskell.Types
import IHaskell.Message.BodyParser

import Debug.Trace

debug x = trace (textToString $ show x) x

----- External interface -----

-- | Parse a message from its ByteString components into a Message.
parseMessage :: [ByteString] -- ^ The list of identifiers sent with the message.
             -> ByteString   -- ^ The header data.
             -> ByteString   -- ^ The parent header, which is just "{}" if there is no header.
             -> ByteString   -- ^ The metadata map, also "{}" for an empty map.
             -> ByteString   -- ^ The message content.
             -> Message      -- ^ A parsed message.
parseMessage idents headerData parentHeader metadata content = 
  let header = parseHeader idents headerData parentHeader metadata
      messageType = msgType header in
    (parser messageType $ Lazy.fromStrict content) {
      header = header                      
    }

----- Module internals -----

-- | Parse a header from its ByteString components into a MessageHeader.
parseHeader :: [ByteString]  -- ^ The list of identifiers.
            -> ByteString    -- ^ The header data.
            -> ByteString    -- ^ The parent header, or "{}" for Nothing.
            -> ByteString    -- ^ The metadata, or "{}" for an empty map.
            -> MessageHeader -- The resulting message header.
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
      Just result = debug $ decode $ Lazy.fromStrict headerData :: Maybe Object
      parentResult = if parentHeader == "{}"
                     then Nothing
                     else Just $ parseHeader idents parentHeader "{}" metadata

      -- Get the basic fields from the header.
      Success (messageType, username, messageUUID, sessionUUID) = debug $ flip parse result $ \obj -> do 
        messType <- obj .: "msg_type"
        username <- obj .: "username"
        message <- obj .: "msg_id"
        session <- obj .: "session"
        return (debug messType, debug username, debug message, debug session)

      -- Get metadata as a simple map.
      Just metadataMap = decode $ Lazy.fromStrict metadata :: Maybe (Map ByteString ByteString)

parser :: MessageType            -- ^ The message type being parsed.
      -> LByteString -> Message   -- The parser that converts the body into a message.
                                -- This message should have an undefined
                                -- header.
parser "kernel_info_request"  = kernelInfoRequestParser
parser "execute_request"      = executeRequestParser
parser other = error $ "Unknown message type " ++ textToString (show other)
