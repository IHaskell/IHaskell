{-# LANGUAGE CPP #-}
-- | Description : @ToJSON@ for Messages
--
-- This module contains the @ToJSON@ instance for @Message@.
module IHaskell.Message.Writer (
  ToJSON(..)
)  where

import Prelude (read)
import ClassyPrelude
import Data.Aeson

import IHaskell.Types

-- ghc (api) version number like ints [7,6,2]. Could be done at compile
-- time, but for now there's no template haskell in IHaskell
ghcVersionInts = ints $
    map read $ words $
    map (\x -> case x of '.' -> ' '; _ -> x)
    (VERSION_ghc :: String)

-- Convert message bodies into JSON.
instance ToJSON Message where
  toJSON KernelInfoReply{} = object [
                             "protocol_version" .= ints [4, 0], -- current protocol version, major and minor
                             "language_version" .= ghcVersionInts,
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
  toJSON (CompleteReply _ m mt t s) = object [
                             "matches" .= m,
                             "matched_text" .= mt,
                             "text" .= t,
                             "status" .= if s then "ok" :: String else "error"
                           ]
  toJSON o@ObjectInfoReply{} = object [
                            "oname" .= objectName o,
                            "found" .= objectFound o,
                            "ismagic" .= False,
                            "isalias" .= False,
                            "type_name" .= objectTypeString o,
                            "docstring" .= objectDocString o
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
