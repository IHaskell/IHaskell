-- | This module contains the @ToJSON@ instance for @Message@.
module IHaskell.Message.Writer (
  ToJSON(..)
)  where

import BasicPrelude
import Data.Aeson
import Data.Map (fromList)

import IHaskell.Types

-- Convert message bodies into JSON.
instance ToJSON Message where
  toJSON KernelInfoReply{} = object [
                             "protocol_version" .= ints [4, 0], -- current protocol version, major and minor
                             "language_version" .= ints [7, 6, 2],
                             "language" .= string "haskell"
                           ]

  toJSON ExecuteReply{ status = status, executionCounter = counter} = object [
                             "status" .= status,
                             "execution_count" .= counter,
                             "payload" .= emptyList,
                             "user_variables" .= emptyMap,
                             "user_expressions" .= emptyMap
                           ]
  toJSON IopubStatus{ executionState = executionState } = object [
                             "execution_state" .= executionState
                           ]
  toJSON IopubStream{ streamType = streamType, streamContent = content } = object [
                             "data" .= content,
                             "name" .= streamType
                           ]

  toJSON body = error $ "Do not know how to convert to JSON for message " ++ textToString (show body)

instance ToJSON ExecutionState where
    toJSON Busy = String "busy"
    toJSON Idle = String "idle"
    toJSON Starting = String "starting"

instance ToJSON StreamType where
    toJSON Stdin = String "stdin"
    toJSON Stdout = String "stdout"

----- Constants -----

emptyMap :: Map String String
emptyMap = fromList []

emptyList :: [Int]
emptyList = []

ints :: [Int] -> [Int]
ints = id

string :: String -> String
string = id
