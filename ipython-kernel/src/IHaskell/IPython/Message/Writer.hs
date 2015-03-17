{-# LANGUAGE OverloadedStrings #-}
-- | Description : @ToJSON@ for Messages
--
-- This module contains the @ToJSON@ instance for @Message@.
module IHaskell.IPython.Message.Writer (
  ToJSON(..)
)  where

import Data.Aeson
import Data.Map     (Map)
import Data.Text    (Text, pack)
import Data.Monoid  (mempty)

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import Data.Text.Encoding

import IHaskell.IPython.Types

-- Convert message bodies into JSON.
instance ToJSON Message where
  toJSON KernelInfoReply{ versionList = vers, language = language } = object [
                             "protocol_version" .= string "5.0", -- current protocol version, major and minor
                             "language_version" .= vers,
                             "language" .= language
                           ]

  toJSON ExecuteReply{ status = status, executionCounter = counter, pagerOutput = pager} = object [
                             "status" .= show status,
                             "execution_count" .= counter,
                             "payload" .= 
                               if null pager
                               then []
                               else [object [
                                    "source" .= string "page",
                                    "text" .= pager
                                ]],
                             "user_variables" .= emptyMap,
                             "user_expressions" .= emptyMap
                           ]
  toJSON PublishStatus{ executionState = executionState } = object [
                             "execution_state" .= executionState
                           ]
  toJSON PublishStream{ streamType = streamType, streamContent = content } = object [
                             "data" .= content,
                             "name" .= streamType
                           ]
  toJSON PublishDisplayData{ source = src, displayData = datas } = object [
                             "source" .= src,
                             "metadata" .= object [],
                             "data" .= object  (map displayDataToJson datas)
                           ]

  toJSON PublishOutput{ executionCount = execCount, reprText = reprText } = object [
                             "data" .= object ["text/plain" .= reprText],
                             "execution_count" .= execCount,
                             "metadata" .= object []
                           ]
  toJSON PublishInput{ executionCount = execCount, inCode = code } = object [
                             "execution_count" .= execCount,
                             "code" .= code
                           ]
  toJSON (CompleteReply _ matches start end metadata status) = object [
                             "matches" .= matches,
                             "cursor_start" .= start,
                             "cursor_end" .= end,
                             "metadata" .= metadata,
                             "status" .= if status then string "ok" else "error"
                           ]
  toJSON o@ObjectInfoReply{} = object [
                            "oname" .= objectName o,
                            "found" .= objectFound o,
                            "ismagic" .= False,
                            "isalias" .= False,
                            "type_name" .= objectTypeString o,
                            "docstring" .= objectDocString o
                           ]

  toJSON ShutdownReply{restartPending = restart} = object [
                            "restart" .= restart
                           ]

  toJSON ClearOutput{wait = wait} = object [
                            "wait" .= wait
                           ]

  toJSON RequestInput{inputPrompt = prompt} = object [
                            "prompt" .= prompt
                           ]

  toJSON req@CommOpen{} = object [
                            "comm_id" .= commUuid req,
                            "target_name" .= commTargetName req,
                            "data" .= commData req
                           ]

  toJSON req@CommData{} = object [
                            "comm_id" .= commUuid req,
                            "data" .= commData req
                           ]

  toJSON req@CommClose{} = object [
                            "comm_id" .= commUuid req,
                            "data" .= commData req
                           ]

  toJSON req@HistoryReply{} = object [ "history" .= map tuplify (historyReply req) ]
    where tuplify (HistoryReplyElement sess linum res) = (sess, linum, case res of 
            Left inp -> toJSON inp
            Right (inp, out) -> toJSON out)

  toJSON body = error $ "Do not know how to convert to JSON for message " ++ show body


-- | Print an execution state as "busy", "idle", or "starting".
instance ToJSON ExecutionState where
    toJSON Busy = String "busy"
    toJSON Idle = String "idle"
    toJSON Starting = String "starting"

-- | Print a stream as "stdin" or "stdout" strings.
instance ToJSON StreamType where
    toJSON Stdin = String "stdin"
    toJSON Stdout = String "stdout"

-- | Convert a MIME type and value into a JSON dictionary pair.
displayDataToJson :: DisplayData -> (Text, Value) 
displayDataToJson (DisplayData mimeType dataStr) =
    pack (show mimeType) .= String dataStr

----- Constants -----

emptyMap :: Map String String
emptyMap = mempty

emptyList :: [Int]
emptyList = []

ints :: [Int] -> [Int]
ints = id

string :: String -> String
string = id
