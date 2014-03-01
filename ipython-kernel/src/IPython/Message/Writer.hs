{-# LANGUAGE OverloadedStrings #-}
-- | Description : @ToJSON@ for Messages
--
-- This module contains the @ToJSON@ instance for @Message@.
module IPython.Message.Writer (
  ToJSON(..)
)  where

import Data.Aeson
import Data.Map     (Map)
import Data.Text    (Text, pack)
import Data.Monoid  (mempty)

import qualified  Data.ByteString.Char8       as Char


import IPython.Types

-- Convert message bodies into JSON.
instance ToJSON Message where
  toJSON KernelInfoReply{ versionList = vers, language = language } = object [
                             "protocol_version" .= ints [4, 0], -- current protocol version, major and minor
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
  toJSON (CompleteReply _ m mt t s) = object [
                             "matches" .= m,
                             "matched_text" .= mt,
                             "text" .= t,
                             "status" .= if s then string "ok" else "error"
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
displayDataToJson (DisplayData mimeType dataStr) = pack (show mimeType) .= Char.unpack dataStr

----- Constants -----

emptyMap :: Map String String
emptyMap = mempty

emptyList :: [Int]
emptyList = []

ints :: [Int] -> [Int]
ints = id

string :: String -> String
string = id
