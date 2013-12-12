{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Description : Argument parsing and basic messaging loop, using Haskell
--                 Chans to communicate with the ZeroMQ sockets. 
module Main where
import ClassyPrelude hiding (liftIO)
import Control.Concurrent.Chan
import Data.Aeson
import Text.Printf
import System.Exit (exitSuccess)

import qualified Data.Map as Map

import IHaskell.Types
import IHaskell.ZeroMQ
import qualified IHaskell.Message.UUID as UUID
import IHaskell.Eval.Evaluate
import IHaskell.Eval.Completion (makeCompletions)
import IHaskell.Eval.Info
import qualified Data.ByteString.Char8 as Chars
import IHaskell.IPython

import GHC
import Outputable (showSDoc, ppr)

data KernelState = KernelState
  { getExecutionCounter :: Int
  }

main ::  IO ()
main = do
  (major, minor, patch) <- ipythonVersion
  when (major < 1) $ do
    void $ printf "Expecting IPython version 1.*, found version %d.%d.%d.\n" major minor patch
    error "Incorrect ipython --version."

  args <- map unpack <$> getArgs
  case args of
    -- Create the "haskell" profile.
    ["setup"] -> setupIPythonProfile "haskell"

    -- Run the ipython <cmd> --profile haskell <args> command.
    "notebook":ipythonArgs -> runIHaskell "haskell" "notebook" ipythonArgs
    "console":ipythonArgs -> runIHaskell "haskell" "console" ipythonArgs

    -- Read the profile JSON file from the argument list.
    ["kernel", profileSrc] -> kernel profileSrc

    -- Bad arguments.
    [] -> putStrLn $ "Provide command to run ('setup', 'kernel <profile-file.json>', " ++
                                           "'notebook [args]', 'console [args]')."
    cmd:_ -> putStrLn $ "Unknown command: " ++ pack cmd


-- | Run the IHaskell language kernel.
kernel :: String -- ^ Filename of profile JSON file.
       -> IO ()
kernel profileSrc = do
  -- Parse the profile file.
  Just profile <- liftM decode . readFile . fpFromText $ pack profileSrc

  -- Serve on all sockets and ports defined in the profile.
  interface <- serveProfile profile

  state <- initialKernelState

  -- Receive and reply to all messages on the shell socket.
  interpret $ forever $ do
    -- Read the request from the request channel.
    request <- liftIO $ readChan $ shellRequestChannel interface

    -- Create a header for the reply.
    replyHeader <- createReplyHeader (header request)

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
createReplyHeader :: MessageHeader -> Interpreter MessageHeader
createReplyHeader parent = do
  -- Generate a new message UUID.
  newMessageId <- liftIO UUID.random

  return MessageHeader {
    identifiers = identifiers parent,
    parentHeader = Just parent,
    metadata = Map.fromList [],
    messageId = newMessageId,
    sessionId = sessionId parent,
    username = username parent,
    msgType = replyType $ msgType parent
  }

-- | Compute a reply to a message. 
replyTo :: ZeroMQInterface -> Message -> MessageHeader -> KernelState -> Interpreter (KernelState, Message)

-- Reply to kernel info requests with a kernel info reply. No computation
-- needs to be done, as a kernel info reply is a static object (all info is
-- hard coded into the representation of that message type).
replyTo _ KernelInfoRequest{} replyHeader state = return (state, KernelInfoReply { header = replyHeader })

-- Reply to a shutdown request by exiting the main thread.
-- Before shutdown, reply to the request to let the frontend know shutdown
-- is happening.
replyTo interface ShutdownRequest{restartPending = restartPending} replyHeader _ = liftIO $ do
    writeChan (shellReplyChannel interface) $ ShutdownReply replyHeader restartPending
    exitSuccess

-- Reply to an execution request. The reply itself does not require
-- computation, but this causes messages to be sent to the IOPub socket
-- with the output of the code in the execution request.
replyTo interface ExecuteRequest{ getCode = code } replyHeader state = do
  let execCount = getExecutionCounter state
      -- Convenience function to send a message to the IOPub socket.
      send msg = liftIO $ writeChan (iopubChannel interface) msg

  -- Notify the frontend that the kernel is busy computing.
  -- All the headers are copies of the reply header with a different
  -- message type, because this preserves the session ID, parent header,
  -- and other important information.
  busyHeader <- dupHeader replyHeader StatusMessage
  send $ PublishStatus busyHeader Busy

  -- Construct a function for publishing output as this is going.
  let publish :: [DisplayData] -> Interpreter ()
      publish outputs = do
        header <- dupHeader replyHeader DisplayDataMessage
        send $ PublishDisplayData header "haskell" outputs

  -- Get display data outputs of evaluating the code.
  evaluate execCount (Chars.unpack code) publish

  {-
  -- Get display data outputs of evaluating the code.
  outputs <- evaluate execCount (Chars.unpack code) publish

  -- Find all the plain text outputs.
  -- Send plain text output via an output message, because we are just
  -- publishing output and not some representation of data.
  let isPlain (Display mime _) = mime == PlainText
  case find isPlain outputs of
    Just (Display PlainText text) -> do
      outHeader <- dupHeader replyHeader OutputMessage
      send $ PublishOutput outHeader text execCount
    Nothing -> return ()

  -- Send all the non-plain-text representations of data to the frontend.
  displayHeader <- dupHeader replyHeader DisplayDataMessage
  send $ PublishDisplayData displayHeader "haskell" $ filter (not . isPlain) outputs
  -}

  -- Notify the frontend that we're done computing.
  idleHeader <- dupHeader replyHeader StatusMessage
  send $ PublishStatus idleHeader Idle

  -- Increment the execution counter in the kernel state.
  let newState = state { getExecutionCounter = execCount + 1 }
  return (newState, ExecuteReply {
    header = replyHeader,
    executionCounter = execCount,
    status = Ok
  })


replyTo _ creq@CompleteRequest{} replyHeader state = do
    cr <- makeCompletions replyHeader creq
    return (state,  cr)

-- | Reply to the object_info_request message. Given an object name, return
-- | the associated type calculated by GHC.
replyTo _ ObjectInfoRequest{objectName=oname} replyHeader state = do
         docs <- info $ Chars.unpack oname
         let reply = ObjectInfoReply {
                        header = replyHeader,
                        objectName = oname, 
                        objectFound = docs == "",
                        objectTypeString = Chars.pack docs,
                        objectDocString  = Chars.pack docs                    
                      }
         return (state, reply)


 
