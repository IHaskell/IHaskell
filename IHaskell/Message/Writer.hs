-- | This module contains the @ToJSON@ instance for @Message@.
module IHaskell.Message.Writer (
  ToJSON(..)
)  where

import ClassyPrelude
import Data.Aeson

import IHaskell.Types

-- Convert message bodies into JSON.
instance ToJSON Message where
  toJSON KernelInfoReply{} = object [
                             "protocol_version" .= ints [4, 0], -- current protocol version, major and minor
                             "language_version" .= ints [7, 6, 2],
                             "language" .= string "haskell"
                           ]

  toJSON ExecuteReply{ status = status, executionCounter = counter} = object [
                             "status" .= show status,
                             "execution_count" .= counter,
                             "payload" .= emptyList,
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
displayDataToJson (Display mimeType dataStr) = pack (show mimeType) .= dataStr

----- Constants -----

emptyMap :: Map String String
emptyMap = mempty

emptyList :: [Int]
emptyList = []

ints :: [Int] -> [Int]
ints = id

string :: String -> String
string = id
