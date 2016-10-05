{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-name-shadowing -fno-warn-unused-matches #-}

-- | Description : @ToJSON@ for Messages
--
-- This module contains the @ToJSON@ instance for @Message@.
module IHaskell.IPython.Message.Writer (ToJSON(..)) where

import           Data.Aeson
import           Data.Aeson.Types (Pair)
import           Data.Map (Map)
import           Data.Monoid (mempty)
import           Data.Text (Text, pack)
import qualified Data.Map               as Map
import           IHaskell.IPython.Types

instance ToJSON LanguageInfo where
  toJSON info = object
                  [ "name" .= languageName info
                  , "version" .= languageVersion info
                  , "file_extension" .= languageFileExtension info
                  , "codemirror_mode" .= languageCodeMirrorMode info
                  ]

-- Convert message bodies into JSON.
instance ToJSON Message where
  toJSON rep@KernelInfoReply{} =
    object
      [ "protocol_version" .= protocolVersion rep
      , "banner" .= banner rep
      , "implementation" .= implementation rep
      , "implementation_version" .= implementationVersion rep
      , "language_info" .= languageInfo rep
      ]

  toJSON CommInfoReply
    { header = header
    , commInfo = commInfo
    } =
    object
      [ "comms" .= Map.map (\comm -> object ["target_name" .= comm]) commInfo ]

  toJSON ExecuteRequest
    { getCode = code
    , getSilent = silent
    , getStoreHistory = storeHistory
    , getAllowStdin = allowStdin
    , getUserExpressions = userExpressions
    } =
    object
      [ "code" .= code
      , "silent" .= silent
      , "store_history" .= storeHistory
      , "allow_stdin" .= allowStdin
      , "user_expressions" .= userExpressions
      ]

  toJSON ExecuteReply { status = status, executionCounter = counter, pagerOutput = pager } =
    object
      [ "status" .= show status
      , "execution_count" .= counter
      , "payload" .=
        if null pager
          then []
          else mkPayload pager
      , "user_expressions" .= emptyMap
      ]
    where
      mkPayload o = [ object
                        [ "source" .= string "page"
                        , "start" .= Number 0
                        , "data" .= object (map displayDataToJson o)
                        ]
                    ]
  toJSON PublishStatus { executionState = executionState } =
    object ["execution_state" .= executionState]
  toJSON PublishStream { streamType = streamType, streamContent = content } =
    object ["data" .= content, "name" .= streamType]
  toJSON PublishDisplayData { source = src, displayData = datas } =
    object
      ["source" .= src, "metadata" .= object [], "data" .= object (map displayDataToJson datas)]

  toJSON PublishOutput { executionCount = execCount, reprText = reprText } =
    object
      [ "data" .= object ["text/plain" .= reprText]
      , "execution_count" .= execCount
      , "metadata" .= object []
      ]
  toJSON PublishInput { executionCount = execCount, inCode = code } =
    object ["execution_count" .= execCount, "code" .= code]
  toJSON (CompleteReply _ matches start end metadata status) =
    object
      [ "matches" .= matches
      , "cursor_start" .= start
      , "cursor_end" .= end
      , "metadata" .= metadata
      , "status" .= if status
                      then string "ok"
                      else "error"
      ]
  toJSON i@InspectReply{} =
    object
      [ "status" .= if inspectStatus i
                      then string "ok"
                      else "error"
      , "data" .= object (map displayDataToJson . inspectData $ i)
      , "metadata" .= object []
      , "found" .= inspectStatus i
      ]

  toJSON ShutdownReply { restartPending = restart } =
    object ["restart" .= restart]

  toJSON ClearOutput { wait = wait } =
    object ["wait" .= wait]

  toJSON RequestInput { inputPrompt = prompt } =
    object ["prompt" .= prompt]

  toJSON req@CommOpen{} =
    object
      [ "comm_id" .= commUuid req
      , "target_name" .= commTargetName req
      , "target_module" .= commTargetModule req
      , "data" .= commData req
      ]

  toJSON req@CommData{} =
    object ["comm_id" .= commUuid req, "data" .= commData req]

  toJSON req@CommClose{} =
    object ["comm_id" .= commUuid req, "data" .= commData req]

  toJSON req@HistoryReply{} =
    object ["history" .= map tuplify (historyReply req)]
    where
      tuplify (HistoryReplyElement sess linum res) = (sess, linum, case res of
                                                                     Left inp         -> toJSON inp
                                                                     Right (inp, out) -> toJSON out)

  toJSON req@IsCompleteReply{} =
    object pairs
    where
      pairs =
        case reviewResult req of
          CodeComplete       -> status "complete"
          CodeIncomplete ind -> status "incomplete" ++ indent ind
          CodeInvalid        -> status "invalid"
          CodeUnknown        -> status "unknown"
      status x = ["status" .= pack x]
      indent x = ["indent" .= pack x]

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
