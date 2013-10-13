import ClassyPrelude hiding (liftIO)
import Control.Concurrent.Chan
import Data.Aeson

import qualified Data.Map as Map

import IHaskell.Types
import IHaskell.ZeroMQ
import qualified IHaskell.Message.UUID as UUID
import IHaskell.Eval.Evaluate
import qualified Data.ByteString.Char8 as Chars

data KernelState = KernelState
  { getExecutionCounter :: Int
  }

main ::  IO ()
main = do
  -- Read the profile JSON file from the argument list.
  [profileSrc] <- getArgs

  -- Parse the profile file.
  Just profile <- liftM decode $ readFile $ fpFromText profileSrc

  -- Serve on all sockets and ports defined in the profile.
  interface <- serveProfile profile

  state <- initialKernelState

  -- Receive and reply to all messages on the shell socket.
  interpret $ forever $ do
    -- Read the request from the request channel.
    request <- liftIO $ readChan $ shellRequestChannel interface

    -- Create a header for the reply.
    replyHeader <- liftIO $ createReplyHeader (header request)

    -- Create the reply, possibly modifying kernel state.
    oldState <- liftIO $ takeMVar state
    (newState, reply) <- replyTo interface request replyHeader oldState 
    liftIO $ putMVar state newState

    -- Write the reply to the reply channel.
    liftIO $ writeChan (shellReplyChannel interface) reply

-- Initial kernel state.
initialKernelState :: IO (MVar KernelState)
initialKernelState =
  newMVar KernelState {
    getExecutionCounter = 1
  }

-- | Duplicate a message header, giving it a new UUID and message type.
dupHeader :: MessageHeader -> MessageType -> Interpreter MessageHeader
dupHeader header messageType = do
  uuid <- liftIO UUID.random

  return header { messageId = uuid, msgType = messageType }

-- | Create a new message header, given a parent message header.
createReplyHeader :: MessageHeader -> IO MessageHeader
createReplyHeader parent = do
  -- Generate a new message UUID.
  newMessageId <- UUID.random

  return MessageHeader {
    identifiers = identifiers parent,
    parentHeader = Just parent,
    metadata = Map.fromList [],
    messageId = newMessageId,
    sessionId = sessionId parent,
    username = username parent,
    msgType = replyType $ msgType parent
  }

replyTo :: ZeroMQInterface -> Message -> MessageHeader -> KernelState -> Interpreter (KernelState, Message)
replyTo _ KernelInfoRequest{} replyHeader state = return (state, KernelInfoReply { header = replyHeader })

replyTo interface ExecuteRequest{ getCode = code } replyHeader state = do
  let execCount = getExecutionCounter state
      send msg = liftIO $ writeChan (iopubChannel interface) msg

  idleHeader <- dupHeader replyHeader StatusMessage
  send $ PublishStatus idleHeader Idle

  busyHeader <- dupHeader replyHeader StatusMessage
  send $ PublishStatus busyHeader Busy

  outputs <- evaluate $ Chars.unpack code

  let isPlain (Display mime _) = mime == PlainText
  case find isPlain outputs of
    Just (Display PlainText text) -> do
      outHeader <- dupHeader replyHeader OutputMessage
      send $ PublishOutput outHeader text execCount
    Nothing -> return ()

  displayHeader <- dupHeader replyHeader DisplayDataMessage
  send $ PublishDisplayData displayHeader "haskell" $ filter (not . isPlain) outputs

  let newState = state { getExecutionCounter = execCount + 1 }
  return (newState, ExecuteReply {
    header = replyHeader,
    executionCounter = execCount,
    status = Ok
  })
