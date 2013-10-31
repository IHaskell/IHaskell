{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- | Description : Parsing messages received from IPython
--
-- This module is responsible for converting from low-level ByteStrings
-- obtained from the 0MQ sockets into Messages. The only exposed function is
-- `parseMessage`, which should only be used in the low-level 0MQ interface.
module IHaskell.Message.Parser (parseMessage) where

import ClassyPrelude
import Data.Aeson ((.:), decode, Result(..), Object)
import Data.Aeson.Types (parse)

import qualified Data.ByteString.Lazy as Lazy

import IHaskell.Types

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
      messageType = msgType header
      messageWithoutHeader = parser messageType $ Lazy.fromStrict content in
    messageWithoutHeader { header = header }

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

noHeader :: MessageHeader
noHeader = error "No header created"

parser :: MessageType            -- ^ The message type being parsed.
       -> LByteString -> Message   -- The parser that converts the body into a message.
                                 -- This message should have an undefined
                                 -- header.
parser KernelInfoRequestMessage  = kernelInfoRequestParser
parser ExecuteRequestMessage     = executeRequestParser
parser CompleteRequestMessage    = completeRequestParser
parser other = error $ "Unknown message type " ++ show other

-- | Parse a kernel info request.
-- A kernel info request has no auxiliary information, so ignore the body.
kernelInfoRequestParser :: LByteString -> Message
kernelInfoRequestParser _ = KernelInfoRequest { header = noHeader }

-- | Parse an execute request.
-- Fields used are:
--  1. "code": the code to execute.
--  2. "silent": whether to execute silently.
--  3. "store_history": whether to include this in history.
--  4. "allow_stdin": whether to allow reading from stdin for this code.
executeRequestParser :: LByteString -> Message
executeRequestParser content = 
  let parser obj = do
        code <- obj .: "code"
        silent <- obj .: "silent"
        storeHistory <- obj .: "store_history"
        allowStdin <- obj .: "allow_stdin"

        return (code, silent, storeHistory, allowStdin)
      Just decoded = decode content
      Success (code, silent, storeHistory, allowStdin) = parse parser decoded in
    ExecuteRequest {
      header = noHeader,
      getCode = code,
      getSilent = silent,
      getAllowStdin = allowStdin,
      getStoreHistory = storeHistory,
      getUserVariables = [],
      getUserExpressions = []
    }

completeRequestParser :: LByteString -> Message
completeRequestParser content = parsed
  where
  Success parsed = flip parse decoded $ \ obj -> do
        code     <- obj .: "block" <|> return ""
        codeLine <- obj .: "line"
        pos      <- obj .: "cursor_pos"
        return $ CompleteRequest noHeader code codeLine pos

  Just decoded = decode content

