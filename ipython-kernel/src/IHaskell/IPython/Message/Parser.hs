{-# LANGUAGE OverloadedStrings #-}

-- | Description : Parsing messages received from IPython
--
-- This module is responsible for converting from low-level ByteStrings obtained from the 0MQ
-- sockets into Messages. The only exposed function is `parseMessage`, which should only be used in
-- the low-level 0MQ interface.
module IHaskell.IPython.Message.Parser (parseMessage) where

import           Data.Aeson ((.:), decode, Result(..), Object)
import           Control.Applicative ((<|>), (<$>), (<*>))
import           Data.Aeson.Types (parse)
import           Data.ByteString
import           Data.Map (Map)
import           Data.Text (Text)
import qualified Data.ByteString.Lazy as Lazy
import           IHaskell.IPython.Types

type LByteString = Lazy.ByteString

-- --- External interface ----- | Parse a message from its ByteString components into a Message.
parseMessage :: [ByteString] -- ^ The list of identifiers sent with the message.
             -> ByteString   -- ^ The header data.
             -> ByteString   -- ^ The parent header, which is just "{}" if there is no header.
             -> ByteString   -- ^ The metadata map, also "{}" for an empty map.
             -> ByteString   -- ^ The message content.
             -> Message      -- ^ A parsed message.
parseMessage idents headerData parentHeader metadata content =
  let header = parseHeader idents headerData parentHeader metadata
      messageType = msgType header
      messageWithoutHeader = parser messageType $ Lazy.fromStrict content
  in messageWithoutHeader { header = header }

-- --- Module internals ----- | Parse a header from its ByteString components into a MessageHeader.
parseHeader :: [ByteString]  -- ^ The list of identifiers.
            -> ByteString    -- ^ The header data.
            -> ByteString    -- ^ The parent header, or "{}" for Nothing.
            -> ByteString    -- ^ The metadata, or "{}" for an empty map.
            -> MessageHeader -- The resulting message header.
parseHeader idents headerData parentHeader metadata =
  MessageHeader
    { identifiers = idents
    , parentHeader = parentResult
    , metadata = metadataMap
    , messageId = messageUUID
    , sessionId = sessionUUID
    , username = username
    , msgType = messageType
    }
  where
    -- Decode the header data and the parent header data into JSON objects. If the parent header data is
    -- absent, just have Nothing instead.
    Just result = decode $ Lazy.fromStrict headerData :: Maybe Object
    parentResult = if parentHeader == "{}"
                     then Nothing
                     else Just $ parseHeader idents parentHeader "{}" metadata

    Success (messageType, username, messageUUID, sessionUUID) = flip parse result $ \obj -> do
      messType <- obj .: "msg_type"
      username <- obj .: "username"
      message <- obj .: "msg_id"
      session <- obj .: "session"
      return (messType, username, message, session)

    -- Get metadata as a simple map.
    Just metadataMap = decode $ Lazy.fromStrict metadata :: Maybe (Map Text Text)

noHeader :: MessageHeader
noHeader = error "No header created"

parser :: MessageType            -- ^ The message type being parsed.
       -> LByteString -> Message -- ^ The parser that converts the body into a message. This message
                                 -- should have an undefined header.
parser KernelInfoRequestMessage = kernelInfoRequestParser
parser ExecuteRequestMessage = executeRequestParser
parser CompleteRequestMessage = completeRequestParser
parser InspectRequestMessage = inspectRequestParser
parser ShutdownRequestMessage = shutdownRequestParser
parser InputReplyMessage = inputReplyParser
parser CommOpenMessage = commOpenParser
parser CommDataMessage = commDataParser
parser CommCloseMessage = commCloseParser
parser HistoryRequestMessage = historyRequestParser
parser other = error $ "Unknown message type " ++ show other

-- | Parse a kernel info request. A kernel info request has no auxiliary information, so ignore the
-- body.
kernelInfoRequestParser :: LByteString -> Message
kernelInfoRequestParser _ = KernelInfoRequest { header = noHeader }

-- | Parse an execute request. Fields used are:
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
      Success (code, silent, storeHistory, allowStdin) = parse parser decoded
  in ExecuteRequest
    { header = noHeader
    , getCode = code
    , getSilent = silent
    , getAllowStdin = allowStdin
    , getStoreHistory = storeHistory
    , getUserVariables = []
    , getUserExpressions = []
    }

requestParser parser content = parsed
  where
    Success parsed = parse parser decoded
    Just decoded = decode content

historyRequestParser :: LByteString -> Message
historyRequestParser = requestParser $ \obj ->
  HistoryRequest noHeader <$> obj .: "output" <*> obj .: "raw" <*> historyAccessType obj
  where
    -- TODO: Implement full history access type parsing from message spec
    historyAccessType obj = do
      accessTypeStr <- obj .: "hist_access_type"
      return $
        case accessTypeStr of
          "range"  -> HistoryRange
          "tail"   -> HistoryTail
          "search" -> HistorySearch
          str      -> error $ "Unknown history access type: " ++ str

completeRequestParser :: LByteString -> Message
completeRequestParser = requestParser $ \obj -> do
  code <- obj .: "code"
  pos <- obj .: "cursor_pos"
  return $ CompleteRequest noHeader code pos

inspectRequestParser :: LByteString -> Message
inspectRequestParser = requestParser $ \obj -> do
  code <- obj .: "code"
  pos <- obj .: "cursor_pos"
  dlevel <- obj .: "detail_level"
  return $ InspectRequest noHeader code pos dlevel

shutdownRequestParser :: LByteString -> Message
shutdownRequestParser = requestParser $ \obj -> do
  code <- obj .: "restart"
  return $ ShutdownRequest noHeader code

inputReplyParser :: LByteString -> Message
inputReplyParser = requestParser $ \obj -> do
  value <- obj .: "value"
  return $ InputReply noHeader value

commOpenParser :: LByteString -> Message
commOpenParser = requestParser $ \obj -> do
  uuid <- obj .: "comm_id"
  name <- obj .: "target_name"
  value <- obj .: "data"
  return $ CommOpen noHeader name uuid value

commDataParser :: LByteString -> Message
commDataParser = requestParser $ \obj -> do
  uuid <- obj .: "comm_id"
  value <- obj .: "data"
  return $ CommData noHeader uuid value

commCloseParser :: LByteString -> Message
commCloseParser = requestParser $ \obj -> do
  uuid <- obj .: "comm_id"
  value <- obj .: "data"
  return $ CommClose noHeader uuid value