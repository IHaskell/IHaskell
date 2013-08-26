import BasicPrelude
import Control.Concurrent
import Data.Aeson

import qualified Data.Map as Map
import qualified Data.UUID.V4 as UUID (nextRandom)
import qualified Data.ByteString.Lazy as ByteString

import IHaskell.Types
import IHaskell.ZeroMQ

type KernelState = Int

main ::  IO ()
main = do
  -- Read the profile JSON file from the argument list.
  [profileSrc] <- getArgs

  -- Parse the profile file.
  Just profile <- liftM decode $ ByteString.readFile (textToString profileSrc)

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
initialKernelState = newMVar 0

-- | Create a new message header, given a parent message header.
createReplyHeader :: MessageHeader -> IO MessageHeader
createReplyHeader parent = do
  -- Generate a new message UUID.
  newMessageId <- UUID.nextRandom

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

replyTo _ message@(ExecuteRequest{}) replyHeader state = do
  return (state, ExecuteReply { header = replyHeader })
