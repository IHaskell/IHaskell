import ClassyPrelude
import Control.Concurrent.Chan
import Data.Aeson

import qualified Data.Map as Map

import IHaskell.Types
import IHaskell.ZeroMQ
import qualified IHaskell.Message.UUID as UUID

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
  forever $ do
    -- Read the request from the request channel.
    request <- readChan $ shellRequestChannel interface

    -- Create a header for the reply.
    replyHeader <- createReplyHeader (header request)

    -- Create the reply, possibly modifying kernel state.
    reply <- modifyMVar state $ replyTo interface request replyHeader

    -- Write the reply to the reply channel.
    writeChan (shellReplyChannel interface) reply

-- Initial kernel state.
initialKernelState :: IO (MVar KernelState)
initialKernelState = newMVar KernelState {
  getExecutionCounter = 1
  }

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

replyTo :: ZeroMQInterface -> Message -> MessageHeader -> KernelState -> IO (KernelState, Message)
replyTo _ KernelInfoRequest{} replyHeader state = return (state, KernelInfoReply { header = replyHeader })

replyTo interface ExecuteRequest{} replyHeader state = do
  -- Queue up a response on the iopub socket 
  uuid1 : uuid2 : uuid3 : uuid4 : uuid5 : uuid6 : []   <- UUID.randoms 6

  let header =  MessageHeader {
    identifiers = identifiers replyHeader,
    parentHeader = parentHeader replyHeader,
    metadata = Map.fromList [],
    messageId = uuid1,
    sessionId = sessionId replyHeader,
    username = username replyHeader,
    msgType = "status"
  }
  let busyHeader = header { messageId = uuid5 }
  let statusMsg = IopubStatus {
    header = header,
    executionState = Idle
  }
  let busyMsg = IopubStatus {
    header = busyHeader,
    executionState = Busy
  }
  let streamHeader =  MessageHeader {
    identifiers = identifiers replyHeader,
    parentHeader = parentHeader replyHeader,
    metadata = Map.fromList [],
    messageId = uuid2,
    sessionId = sessionId replyHeader,
    username = username replyHeader,
    msgType = "stream"
  }
  let dispHeader =  MessageHeader {
    identifiers = identifiers replyHeader,
    parentHeader = parentHeader replyHeader,
    metadata = Map.fromList [],
    messageId = uuid3,
    sessionId = sessionId replyHeader,
    username = username replyHeader,
    msgType = "display_data"
  }
  let pyoutHeader = dispHeader { messageId = uuid4, msgType = "pyout" }
  let pyinHeader = dispHeader { messageId = uuid6, msgType = "pyin" }

  let things = "$a+b=c$"
  let streamMsg = IopubStream streamHeader Stdout $ "Hello! " ++ show (getExecutionCounter state)
  let displayMsg = IopubDisplayData dispHeader "haskell" [Display PlainText things, Display MimeHtml things]
      pyoutMsg = IopubPythonOut pyoutHeader ("Iopub python out " ++ (show (getExecutionCounter state))) (getExecutionCounter state)
      pyinMsg = IopubPythonIn pyinHeader "Who the fuck cares?!" (getExecutionCounter state)
  mapM_ (writeChan $ iopubChannel interface) [pyinMsg, busyMsg, displayMsg, pyoutMsg, statusMsg]

  let counter = getExecutionCounter state
      newState = state { getExecutionCounter = getExecutionCounter state + 1 }
  return (newState, ExecuteReply {
    header = replyHeader,
    executionCounter = counter,
    status = "ok"
  })
