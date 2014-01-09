{-# LANGUAGE NoImplicitPrelude, CPP, OverloadedStrings, ScopedTypeVariables #-}
-- | Description : Argument parsing and basic messaging loop, using Haskell
--                 Chans to communicate with the ZeroMQ sockets. 
module Main where
import ClassyPrelude hiding (liftIO)
import Prelude (last, read)
import Control.Concurrent.Chan
import Control.Concurrent (threadDelay)
import Data.Aeson
import Text.Printf
import System.Exit (exitSuccess)
import System.Directory

import qualified Data.Map as Map

import IHaskell.Types
import IPython.ZeroMQ
import qualified IPython.Message.UUID as UUID
import IHaskell.Eval.Evaluate
import IHaskell.Eval.Completion (complete)
import IHaskell.Eval.Info
import qualified Data.ByteString.Char8 as Chars
import IHaskell.IPython
import qualified IPython.Stdin as Stdin
import IHaskell.Flags

import GHC hiding (extensions, language)
import Outputable (showSDoc, ppr)

-- | Compute the GHC API version number using the dist/build/autogen/cabal_macros.h
ghcVersionInts :: [Int]
ghcVersionInts = map read . words . map dotToSpace $ VERSION_ghc
  where dotToSpace '.' = ' '
        dotToSpace x = x


main ::  IO ()
main = do
  args <- parseFlags <$> map unpack <$> getArgs
  case args of
    Left errorMessage -> 
      hPutStrLn stderr errorMessage
    Right args ->
      ihaskell args

ihaskell :: Args -> IO ()
-- If no mode is specified, print help text.
ihaskell (Args (ShowHelp help) _) = 
  putStrLn $ pack help

-- Update IPython: remove then reinstall.
-- This is in case cabal updates IHaskell but the corresponding IPython
-- isn't updated. This is hard to detect since versions of IPython might
-- not change!
ihaskell (Args UpdateIPython _) = do
  setupIPython
  putStrLn "IPython updated."
    
ihaskell (Args Console flags) = showingHelp Console flags $ do
  setupIPython

  flags <- addDefaultConfFile flags
  info <- initInfo IPythonConsole flags
  runConsole info

ihaskell (Args (View (Just fmt) (Just name)) []) =
  nbconvert fmt name

ihaskell (Args Notebook flags) = showingHelp Notebook flags $ do
  setupIPython

  let server = case mapMaybe serveDir flags of
                 [] -> Nothing
                 xs -> Just $ last xs

  flags <- addDefaultConfFile flags

  undirInfo <- initInfo IPythonNotebook flags
  curdir <- getCurrentDirectory
  let info = undirInfo { initDir = curdir }

  runNotebook info server
  where
    serveDir (ServeFrom dir) = Just dir
    serveDir _ = Nothing

ihaskell (Args (Kernel (Just filename)) _) = do
  initInfo <- readInitInfo
  runKernel filename initInfo

-- | Add a conf file to the arguments if none exists.
addDefaultConfFile :: [Argument] -> IO [Argument]
addDefaultConfFile flags = do
  def <- defaultConfFile
  case (find isConfFile flags, def) of
    (Nothing, Just file) -> return $ ConfFile file : flags
    _ -> return flags
  where
    isConfFile (ConfFile _) = True
    isConfFile _ = False

showingHelp :: IHaskellMode -> [Argument] -> IO () -> IO ()
showingHelp mode flags act =
  case find (==Help) flags of
    Just _ ->
      putStrLn $ pack $ help mode
    Nothing ->
      act
 
-- | Parse initialization information from the flags.
initInfo :: FrontendType -> [Argument] -> IO InitInfo
initInfo front [] = return InitInfo { extensions = [], initCells = [], initDir = ".", frontend = front }
initInfo front (flag:flags) = do
  info <- initInfo front flags
  case flag of
    Extension ext -> return info { extensions = ext:extensions info }
    ConfFile filename -> do
      cell <- readFile (fpFromText $ pack filename)
      return info { initCells = cell:initCells info }
    _ -> return info

-- | Run the IHaskell language kernel.
runKernel :: String    -- ^ Filename of profile JSON file.
          -> InitInfo  -- ^ Initialization information from the invocation.
          -> IO ()
runKernel profileSrc initInfo = do
  setCurrentDirectory $ initDir initInfo

  -- Parse the profile file.
  Just profile <- liftM decode . readFile . fpFromText $ pack profileSrc

  -- Necessary for `getLine` and their ilk to work.
  dir <- getIHaskellDir
  Stdin.recordKernelProfile dir profile

  -- Serve on all sockets and ports defined in the profile.
  interface <- serveProfile profile

  -- Create initial state in the directory the kernel *should* be in.
  state <- initialKernelState
  modifyMVar_ state $ \kernelState -> return $
    kernelState { getFrontend = frontend initInfo }

  -- Receive and reply to all messages on the shell socket.
  interpret True $ do
    -- Initialize the context by evaluating everything we got from the  
    -- command line flags. This includes enabling some extensions and also
    -- running some code.
    let extLines = map (":extension " ++) $ extensions initInfo
        noPublish _ = return ()       
        evaluator line = do
          -- Create a new state each time.
          stateVar <- liftIO initialKernelState
          state <- liftIO $ takeMVar stateVar
          evaluate state line noPublish

    mapM_ evaluator extLines
    mapM_ evaluator $ initCells initInfo

    forever $ do
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
  newMVar defaultKernelState

-- | Duplicate a message header, giving it a new UUID and message type.
dupHeader :: MessageHeader -> MessageType -> IO MessageHeader
dupHeader header messageType = do
  uuid <- liftIO UUID.random

  return header { messageId = uuid, msgType = messageType }

-- | Create a new message header, given a parent message header.
createReplyHeader :: MessageHeader -> Interpreter MessageHeader
createReplyHeader parent = do
  -- Generate a new message UUID.
  newMessageId <- liftIO UUID.random
  let repType = fromMaybe err (replyType $ msgType parent)
      err = error $ "No reply for message " ++ show (msgType parent)

  return MessageHeader {
    identifiers = identifiers parent,
    parentHeader = Just parent,
    metadata = Map.fromList [],
    messageId = newMessageId,
    sessionId = sessionId parent,
    username = username parent,
    msgType = repType
  }

-- | Compute a reply to a message. 
replyTo :: ZeroMQInterface -> Message -> MessageHeader -> KernelState -> Interpreter (KernelState, Message)

-- Reply to kernel info requests with a kernel info reply. No computation
-- needs to be done, as a kernel info reply is a static object (all info is
-- hard coded into the representation of that message type).
replyTo _ KernelInfoRequest{} replyHeader state =
  return (state, KernelInfoReply {
    header = replyHeader,
    language = "haskell",
    versionList = ghcVersionInts
   })

-- Reply to a shutdown request by exiting the main thread.
-- Before shutdown, reply to the request to let the frontend know shutdown
-- is happening.
replyTo interface ShutdownRequest{restartPending = restartPending} replyHeader _ = liftIO $ do
    writeChan (shellReplyChannel interface) $ ShutdownReply replyHeader restartPending
    exitSuccess

-- Reply to an execution request. The reply itself does not require
-- computation, but this causes messages to be sent to the IOPub socket
-- with the output of the code in the execution request.
replyTo interface req@ExecuteRequest{ getCode = code } replyHeader state = do
 -- Convenience function to send a message to the IOPub socket.
  let send msg = liftIO $ writeChan (iopubChannel interface) msg

  -- Log things so that we can use stdin.
  dir <- liftIO getIHaskellDir
  liftIO $ Stdin.recordParentHeader dir $ header req

  -- Notify the frontend that the kernel is busy computing.
  -- All the headers are copies of the reply header with a different
  -- message type, because this preserves the session ID, parent header,
  -- and other important information.
  busyHeader <- liftIO $ dupHeader replyHeader StatusMessage
  send $ PublishStatus busyHeader Busy

  -- Construct a function for publishing output as this is going.
  -- This function accepts a boolean indicating whether this is the final
  -- output and the thing to display. Store the final outputs in a list so
  -- that when we receive an updated non-final output, we can clear the
  -- entire output and re-display with the updated output.
  displayed    <- liftIO $ newMVar []
  updateNeeded <- liftIO $ newMVar False
  pagerOutput  <- liftIO $ newMVar ""
  let clearOutput = do
        header <- dupHeader replyHeader ClearOutputMessage
        send $ ClearOutput header True

      sendOutput outs = do
        header <- dupHeader replyHeader DisplayDataMessage
        send $ PublishDisplayData header "haskell" outs

      publish :: EvaluationResult -> IO ()
      publish result = do
        let final = case result of
                      IntermediateResult {} -> False
                      FinalResult {} -> True
            outs = outputs result

        -- If necessary, clear all previous output and redraw.
        clear <- readMVar updateNeeded
        when clear $ do
          clearOutput
          disps <- readMVar displayed
          mapM_ sendOutput $ reverse disps

        -- Draw this message.
        sendOutput outs

        -- If this is the final message, add it to the list of completed
        -- messages. If it isn't, make sure we clear it later by marking
        -- update needed as true.
        modifyMVar_ updateNeeded (const $ return $ not final)
        when final $ do
          modifyMVar_ displayed (return . (outs:))

          -- If this has some pager output, store it for later.
          let pager = pagerOut result
          unless (null pager) $
            modifyMVar_ pagerOutput (return . (++ pager ++ "\n"))

  -- Run code and publish to the frontend as we go.
  let execCount = getExecutionCounter state
  updatedState <- evaluate state (Chars.unpack code) publish

  -- Notify the frontend that we're done computing.
  idleHeader <- liftIO $ dupHeader replyHeader StatusMessage
  send $ PublishStatus idleHeader Idle

  pager <- liftIO $ readMVar pagerOutput
  return (updatedState, ExecuteReply {
    header = replyHeader,
    pagerOutput = pager,
    executionCounter = execCount,
    status = Ok
  })


replyTo _ req@CompleteRequest{} replyHeader state = do
    (matchedText, completions) <- complete (Chars.unpack $ getCodeLine req) (getCursorPos req)

    let reply =  CompleteReply replyHeader (map Chars.pack completions) (Chars.pack matchedText) (getCodeLine req) True
    return (state,  reply)

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


 
