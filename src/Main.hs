{-# LANGUAGE NoImplicitPrelude, CPP, OverloadedStrings, ScopedTypeVariables #-}
-- | Description : Argument parsing and basic messaging loop, using Haskell
--                 Chans to communicate with the ZeroMQ sockets.
module Main where

-- Prelude imports.
import           ClassyPrelude hiding (last, liftIO, readChan, writeChan)
import           Prelude (last, read)

-- Standard library imports.
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Chan
import           Data.Aeson
import           Data.Text (strip)
import           System.Directory
import           System.Exit (exitSuccess)
import           Text.Printf
import           System.Posix.Signals
import qualified Data.Map as Map

-- IHaskell imports.
import           IHaskell.Convert (convert)
import           IHaskell.Eval.Completion (complete)
import           IHaskell.Eval.Evaluate
import           IHaskell.Display
import           IHaskell.Eval.Info
import           IHaskell.Flags
import           IHaskell.IPython
import           IHaskell.Types
import           IHaskell.IPython.ZeroMQ
import           IHaskell.IPython.Types
import qualified Data.ByteString.Char8 as Chars
import qualified IHaskell.IPython.Message.UUID as UUID
import qualified IHaskell.IPython.Stdin as Stdin

-- GHC API imports.
import           GHC hiding (extensions, language)
import qualified GHC.Paths

-- | Compute the GHC API version number using the dist/build/autogen/cabal_macros.h
ghcVersionInts :: [Int]
ghcVersionInts = map read . words . map dotToSpace $ VERSION_ghc
  where
    dotToSpace '.' = ' '
    dotToSpace x = x


main :: IO ()
main = do
  args <- parseFlags <$> map unpack <$> getArgs
  case args of
    Left errorMessage -> hPutStrLn stderr errorMessage
    Right args        -> ihaskell args

chooseIPython [] = return DefaultIPython
chooseIPython (IPythonFrom path:_) = ExplicitIPython <$> subHome path
chooseIPython (_:xs) = chooseIPython xs

ihaskell :: Args -> IO ()
-- If no mode is specified, print help text.
ihaskell (Args (ShowHelp help) _) = putStrLn $ pack help

ihaskell (Args ConvertLhs args) = showingHelp ConvertLhs args $ convert args

ihaskell (Args Console flags) = showingHelp Console flags $ do
  ipython <- chooseIPython flags
  setupIPython ipython

  flags <- addDefaultConfFile flags
  info <- initInfo IPythonConsole flags
  runConsole ipython info

ihaskell (Args mode@(View (Just fmt) (Just name)) args) = showingHelp mode args $ do
  ipython <- chooseIPython args
  nbconvert ipython fmt name

ihaskell (Args Notebook flags) = showingHelp Notebook flags $ do
  ipython <- chooseIPython flags
  setupIPython ipython

  let server = case mapMaybe serveDir flags of
                 [] -> Nothing
                 xs -> Just $ last xs

  flags <- addDefaultConfFile flags

  undirInfo <- initInfo IPythonNotebook flags
  curdir <- getCurrentDirectory
  let info = undirInfo { initDir = curdir }

  runNotebook ipython info server
  where
    serveDir (ServeFrom dir) = Just dir
    serveDir _ = Nothing

ihaskell (Args (Kernel (Just filename)) flags) = do
  initInfo <- readInitInfo
  runKernel libdir filename initInfo

  where
    libdir = case flags of
      []              -> GHC.Paths.libdir
      [GhcLibDir dir] -> dir


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
runKernel :: String -- ^ GHC libdir.
          -> String    -- ^ Filename of profile JSON file.
          -> InitInfo  -- ^ Initialization information from the invocation.
          -> IO ()
runKernel libdir profileSrc initInfo = do
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
  interpret libdir True $ do
    -- Ignore Ctrl-C the first time.  This has to go inside the
    -- `interpret`, because GHC API resets the signal handlers for some
    -- reason (completely unknown to me).
    liftIO ignoreCtrlC

    -- Initialize the context by evaluating everything we got from the
    -- command line flags. This includes enabling some extensions and also
    -- running some code.
    let extLines = map (":extension " ++) $ extensions initInfo
        noPublish _ = return ()
        evaluator line = void $ do
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

      -- We handle comm messages and normal ones separately.
      -- The normal ones are a standard request/response style, while comms
      -- can be anything, and don't necessarily require a response.
      if isCommMessage request
      then liftIO $ do
        oldState <- takeMVar state
        let replier = writeChan (iopubChannel interface)
        newState <- handleComm replier oldState request replyHeader
        putMVar state newState
        writeChan (shellReplyChannel interface) SendNothing
      else do
        -- Create the reply, possibly modifying kernel state.
        oldState <- liftIO $ takeMVar state
        (newState, reply) <- replyTo interface request replyHeader oldState
        liftIO $ putMVar state newState

        -- Write the reply to the reply channel.
        liftIO $ writeChan (shellReplyChannel interface) reply
  where
    ignoreCtrlC =
      installHandler keyboardSignal (CatchOnce $ putStrLn "Press Ctrl-C again to quit kernel.") Nothing

    isCommMessage req = msgType (header req) `elem` [CommDataMessage, CommCloseMessage]

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

      sendOutput (ManyDisplay manyOuts) = mapM_ sendOutput manyOuts
      sendOutput (Display outs) = do
        header <- dupHeader replyHeader DisplayDataMessage
        send $ PublishDisplayData header "haskell" $ map convertSvgToHtml outs

      convertSvgToHtml (DisplayData MimeSvg svg) = html $ makeSvgImg $ base64 $ encodeUtf8 svg
      convertSvgToHtml x = x
      makeSvgImg base64data = unpack $ "<img src=\"data:image/svg+xml;base64," ++ base64data ++ "\"/>"

      startComm :: CommInfo -> IO ()
      startComm (CommInfo widget uuid target) = do
        -- Send the actual comm open.
        header <- dupHeader replyHeader CommOpenMessage
        send $ CommOpen header target uuid (Object mempty)

        -- Send anything else the widget requires.
        let communicate value = do
              head <- dupHeader replyHeader CommDataMessage
              writeChan (iopubChannel interface) $ CommData head uuid value
        open widget communicate

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

          -- Start all comms that need to be started.
          mapM_ startComm $ startComms result

          -- If this has some pager output, store it for later.
          let pager = pagerOut result
          unless (null pager) $
            if usePager state
            then modifyMVar_ pagerOutput (return . (++ pager ++ "\n"))
            else sendOutput $ Display [html pager]

  let execCount = getExecutionCounter state
  -- Let all frontends know the execution count and code that's about to run
  inputHeader <- liftIO $ dupHeader replyHeader InputMessage
  send $ PublishInput inputHeader (unpack code) execCount

  -- Run code and publish to the frontend as we go.
  updatedState <- evaluate state (unpack code) publish

  -- Notify the frontend that we're done computing.
  idleHeader <- liftIO $ dupHeader replyHeader StatusMessage
  send $ PublishStatus idleHeader Idle

  -- Take pager output if we're using the pager.
  pager <- if usePager state
          then liftIO $ readMVar pagerOutput
          else return ""
  return (updatedState, ExecuteReply {
    header = replyHeader,
    pagerOutput = pager,
    executionCounter = execCount,
    status = Ok
  })


replyTo _ req@CompleteRequest{} replyHeader state = do
  let line = getCodeLine req
  (matchedText, completions) <- complete (unpack line) (getCursorPos req)

  let reply =  CompleteReply replyHeader (map pack completions) (pack matchedText) line True
  return (state,  reply)

-- | Reply to the object_info_request message. Given an object name, return
-- | the associated type calculated by GHC.
replyTo _ ObjectInfoRequest{objectName = oname} replyHeader state = do
  docs <- pack <$> info (unpack oname)
  let reply = ObjectInfoReply {
                header = replyHeader,
                objectName = oname,
                objectFound = strip docs /= "",
                objectTypeString = docs,
                objectDocString  = docs
              }
  return (state, reply)

handleComm :: (Message -> IO ()) -> KernelState -> Message -> MessageHeader -> IO KernelState
handleComm replier kernelState req replyHeader = do
  let widgets = openComms kernelState
      uuid = commUuid req
      dat = commData req
      communicate value = do
        head <- dupHeader replyHeader CommDataMessage
        replier $ CommData head uuid value
  case lookup uuid widgets of
    Nothing -> fail $ "no widget with uuid " ++ show uuid
    Just (Widget widget) ->
      case msgType $ header req of
        CommDataMessage -> do
          comm widget dat communicate
          return kernelState
        CommCloseMessage -> do
          close widget dat
          return kernelState { openComms = Map.delete uuid widgets }
