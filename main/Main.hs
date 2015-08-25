{-# LANGUAGE CPP, ScopedTypeVariables #-}

-- | Description : Argument parsing and basic messaging loop, using Haskell
--                 Chans to communicate with the ZeroMQ sockets.
module Main (main) where

import           IHaskellPrelude
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as CBS

-- Standard library imports.
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Chan
import           Data.Aeson
import           System.Directory
import           System.Process (readProcess, readProcessWithExitCode)
import           System.Exit (exitSuccess, ExitCode(ExitSuccess))
import           System.Environment (getArgs)
#if MIN_VERSION_ghc(7,8,0)
import           System.Environment (setEnv)
#endif
import           System.Posix.Signals
import qualified Data.Map as Map
import           Data.String.Here (hereFile)
import qualified Data.Text.Encoding as E
import           Data.List (break)

-- IHaskell imports.
import           IHaskell.Convert (convert)
import           IHaskell.Eval.Completion (complete)
import           IHaskell.Eval.Inspect (inspect)
import           IHaskell.Eval.Evaluate
import           IHaskell.Display
import           IHaskell.Eval.Info
import           IHaskell.Eval.Widgets (widgetHandler)
import           IHaskell.Flags
import           IHaskell.IPython
import           IHaskell.Types
import           IHaskell.Publish
import           IHaskell.IPython.ZeroMQ
import           IHaskell.IPython.Types
import qualified IHaskell.IPython.Message.UUID as UUID
import qualified IHaskell.IPython.Stdin as Stdin

-- GHC API imports.
import           GHC hiding (extensions, language)

-- | Compute the GHC API version number using the dist/build/autogen/cabal_macros.h
ghcVersionInts :: [Int]
ghcVersionInts = map (fromJust . readMay) . words . map dotToSpace $ VERSION_ghc
  where
    dotToSpace '.' = ' '
    dotToSpace x = x

consoleBanner :: Text
consoleBanner =
  "Welcome to IHaskell! Run `IHaskell --help` for more information.\n" <>
  "Enter `:help` to learn more about IHaskell built-ins."

main :: IO ()
main = do
  args <- parseFlags <$> getArgs
  case args of
    Left errorMessage -> hPutStrLn stderr errorMessage
    Right args        -> ihaskell args

ihaskell :: Args -> IO ()
ihaskell (Args (ShowHelp help) _) = putStrLn help
ihaskell (Args ConvertLhs args) = showingHelp ConvertLhs args $ convert args
ihaskell (Args InstallKernelSpec args) = showingHelp InstallKernelSpec args $ do
  let kernelSpecOpts = parseKernelArgs args
  replaceIPythonKernelspec kernelSpecOpts
ihaskell (Args (Kernel (Just filename)) args) = do
  let kernelSpecOpts = parseKernelArgs args
  runKernel kernelSpecOpts filename
ihaskell a@(Args (Kernel Nothing) _) = do
  hPutStrLn stderr "No kernel profile JSON specified."
  hPutStrLn stderr "This may be a bug!"
  hPrint stderr a

showingHelp :: IHaskellMode -> [Argument] -> IO () -> IO ()
showingHelp mode flags act =
  case find (== Help) flags of
    Just _ ->
      putStrLn $ help mode
    Nothing ->
      act

-- | Parse initialization information from the flags.
parseKernelArgs :: [Argument] -> KernelSpecOptions
parseKernelArgs = foldl' addFlag defaultKernelSpecOptions
  where
    addFlag kernelSpecOpts (ConfFile filename) =
      kernelSpecOpts { kernelSpecConfFile = return (Just filename) }
    addFlag kernelSpecOpts KernelDebug =
      kernelSpecOpts { kernelSpecDebug = True }
    addFlag kernelSpecOpts (GhcLibDir libdir) =
      kernelSpecOpts { kernelSpecGhcLibdir = libdir }
    addFlag kernelSpecOpts flag = error $ "Unknown flag" ++ show flag

-- | Run the IHaskell language kernel.
runKernel :: KernelSpecOptions -- ^ Various options from when the kernel was installed.
          -> String            -- ^ File with kernel profile JSON (ports, etc).
          -> IO ()
runKernel kernelOpts profileSrc = do
  let debug = kernelSpecDebug kernelOpts
      libdir = kernelSpecGhcLibdir kernelOpts

  -- Parse the profile file.
  Just profile <- liftM decode $ LBS.readFile profileSrc

  -- Necessary for `getLine` and their ilk to work.
  dir <- getIHaskellDir
  Stdin.recordKernelProfile dir profile

  -- Detect if we have stack
  (exitCode, stackStdout, _) <- readProcessWithExitCode "stack" [] ""
  let stack = exitCode == ExitSuccess && "The Haskell Tool Stack" `isInfixOf` stackStdout

#if MIN_VERSION_ghc(7,8,0)
  -- If we're in a stack directory, use `stack` to set the environment
  -- We can't do this with base <= 4.6 because setEnv doesn't exist.
  when stack $ do
    stackEnv <- lines <$> readProcess "stack" ["exec", "env"] ""
    forM_ stackEnv $ \line ->
      let (var, val) = break (== '=') line
      in case tailMay val of
           Nothing -> return ()
           Just val' -> setEnv var val'
#endif

  -- Serve on all sockets and ports defined in the profile.
  interface <- serveProfile profile debug

  -- Create initial state in the directory the kernel *should* be in.
  state <- initialKernelState
  modifyMVar_ state $ \kernelState -> return $
    kernelState { kernelDebug = debug }

  -- Receive and reply to all messages on the shell socket.
  interpret libdir True $ \hasSupportLibraries -> do
    -- Ignore Ctrl-C the first time. This has to go inside the `interpret`, because GHC API resets the
    -- signal handlers for some reason (completely unknown to me).
    liftIO ignoreCtrlC

    liftIO $ modifyMVar_ state $ \kernelState -> return $
              kernelState { supportLibrariesAvailable = hasSupportLibraries }

    -- Initialize the context by evaluating everything we got from the command line flags.
    let noPublish _ = return ()
        noWidget s _ = return s
        evaluator line = void $ do
          -- Create a new state each time.
          stateVar <- liftIO initialKernelState
          state <- liftIO $ takeMVar stateVar
          evaluate state line noPublish noWidget

    confFile <- liftIO $ kernelSpecConfFile kernelOpts
    case confFile of
      Just filename -> liftIO (readFile filename) >>= evaluator
      Nothing       -> return ()

    forever $ do
      -- Read the request from the request channel.
      request <- liftIO $ readChan $ shellRequestChannel interface

      -- Create a header for the reply.
      replyHeader <- createReplyHeader (header request)

      -- We handle comm messages and normal ones separately. The normal ones are a standard
      -- request/response style, while comms can be anything, and don't necessarily require a response.
      if isCommMessage request
        then do
          oldState <- liftIO $ takeMVar state
          let replier = writeChan (iopubChannel interface)
              widgetMessageHandler = widgetHandler replier replyHeader
          tempState <- handleComm replier oldState request replyHeader
          newState <- flushWidgetMessages tempState [] widgetMessageHandler
          liftIO $ putMVar state newState
          liftIO $ writeChan (shellReplyChannel interface) SendNothing
        else do
          -- Create the reply, possibly modifying kernel state.
          oldState <- liftIO $ takeMVar state
          (newState, reply) <- replyTo interface request replyHeader oldState
          liftIO $ putMVar state newState

          -- Write the reply to the reply channel.
          liftIO $ writeChan (shellReplyChannel interface) reply

  where
    ignoreCtrlC =
      installHandler keyboardSignal (CatchOnce $ putStrLn "Press Ctrl-C again to quit kernel.")
        Nothing

    isCommMessage req = msgType (header req) `elem` [CommDataMessage, CommCloseMessage]

-- Initial kernel state.
initialKernelState :: IO (MVar KernelState)
initialKernelState = newMVar defaultKernelState

-- | Create a new message header, given a parent message header.
createReplyHeader :: MessageHeader -> Interpreter MessageHeader
createReplyHeader parent = do
  -- Generate a new message UUID.
  newMessageId <- liftIO UUID.random
  let repType = fromMaybe err (replyType $ msgType parent)
      err = error $ "No reply for message " ++ show (msgType parent)

  return
    MessageHeader
      { identifiers = identifiers parent
      , parentHeader = Just parent
      , metadata = Map.fromList []
      , messageId = newMessageId
      , sessionId = sessionId parent
      , username = username parent
      , msgType = repType
      }

-- | Compute a reply to a message.
replyTo :: ZeroMQInterface -> Message -> MessageHeader -> KernelState -> Interpreter (KernelState, Message)
-- Reply to kernel info requests with a kernel info reply. No computation needs to be done, as a
-- kernel info reply is a static object (all info is hard coded into the representation of that
-- message type).
replyTo _ KernelInfoRequest{} replyHeader state =
  return
    (state, KernelInfoReply
              { header = replyHeader
              , language = "haskell"
              , versionList = ghcVersionInts
              })

-- Reply to a shutdown request by exiting the main thread. Before shutdown, reply to the request to
-- let the frontend know shutdown is happening.
replyTo interface ShutdownRequest { restartPending = restartPending } replyHeader _ = liftIO $ do
  writeChan (shellReplyChannel interface) $ ShutdownReply replyHeader restartPending
  exitSuccess

-- Reply to an execution request. The reply itself does not require computation, but this causes
-- messages to be sent to the IOPub socket with the output of the code in the execution request.
replyTo interface req@ExecuteRequest { getCode = code } replyHeader state = do
  -- Convenience function to send a message to the IOPub socket.
  let send msg = liftIO $ writeChan (iopubChannel interface) msg

  -- Log things so that we can use stdin.
  dir <- liftIO getIHaskellDir
  liftIO $ Stdin.recordParentHeader dir $ header req

  -- Notify the frontend that the kernel is busy computing. All the headers are copies of the reply
  -- header with a different message type, because this preserves the session ID, parent header, and
  -- other important information.
  busyHeader <- liftIO $ dupHeader replyHeader StatusMessage
  send $ PublishStatus busyHeader Busy

  -- Construct a function for publishing output as this is going. This function accepts a boolean
  -- indicating whether this is the final output and the thing to display. Store the final outputs in
  -- a list so that when we receive an updated non-final output, we can clear the entire output and
  -- re-display with the updated output.
  displayed <- liftIO $ newMVar []
  updateNeeded <- liftIO $ newMVar False
  pagerOutput <- liftIO $ newMVar []

  let execCount = getExecutionCounter state
  -- Let all frontends know the execution count and code that's about to run
  inputHeader <- liftIO $ dupHeader replyHeader InputMessage
  send $ PublishInput inputHeader (T.unpack code) execCount

  -- Run code and publish to the frontend as we go.
  let widgetMessageHandler = widgetHandler send replyHeader
      publish = publishResult send replyHeader displayed updateNeeded pagerOutput (usePager state)
  updatedState <- evaluate state (T.unpack code) publish widgetMessageHandler

  -- Notify the frontend that we're done computing.
  idleHeader <- liftIO $ dupHeader replyHeader StatusMessage
  send $ PublishStatus idleHeader Idle

  -- Take pager output if we're using the pager.
  pager <- if usePager state
             then liftIO $ readMVar pagerOutput
             else return []
  return
    (updatedState, ExecuteReply
                     { header = replyHeader
                     , pagerOutput = pager
                     , executionCounter = execCount
                     , status = Ok
                     })


replyTo _ req@CompleteRequest{} replyHeader state = do
  let code = getCode req
      pos = getCursorPos req
  (matchedText, completions) <- complete (T.unpack code) pos

  let start = pos - length matchedText
      end = pos
      reply = CompleteReply replyHeader (map T.pack completions) start end Map.empty True
  return (state, reply)

replyTo _ req@InspectRequest{} replyHeader state = do
  result <- inspect (T.unpack $ inspectCode req) (inspectCursorPos req)
  let reply =
        case result of
          Just (Display datas) -> InspectReply
            { header = replyHeader
            , inspectStatus = True
            , inspectData = datas
            }
          _ -> InspectReply { header = replyHeader, inspectStatus = False, inspectData = [] }
  return (state, reply)

-- TODO: Implement history_reply.
replyTo _ HistoryRequest{} replyHeader state = do
  let reply = HistoryReply
        { header = replyHeader
        -- FIXME
        , historyReply = []
        }
  return (state, reply)

-- TODO: What else can be implemented?
replyTo _ message _ state = do
  liftIO $ hPutStrLn stderr $ "Unimplemented message: " ++ show message
  return (state, SendNothing)

-- | Handle comm messages
handleComm :: (Message -> IO ()) -> KernelState -> Message -> MessageHeader -> Interpreter KernelState
handleComm send kernelState req replyHeader = do
  -- MVars to hold intermediate data during publishing
  displayed <- liftIO $ newMVar []
  updateNeeded <- liftIO $ newMVar False
  pagerOutput <- liftIO $ newMVar []

  let widgets = openComms kernelState
      uuid = commUuid req
      dat = commData req
      communicate value = do
        head <- dupHeader replyHeader CommDataMessage
        send $ CommData head uuid value
      toUsePager = usePager kernelState

  -- Create a publisher according to current state, use that to build
  -- a function that executes an IO action and publishes the output to
  -- the frontend simultaneously.
  let run = capturedIO publish kernelState
      publish = publishResult send replyHeader displayed updateNeeded pagerOutput toUsePager

  -- Notify the frontend that the kernel is busy
  busyHeader <- liftIO $ dupHeader replyHeader StatusMessage
  liftIO . send $ PublishStatus busyHeader Busy

  newState <- case Map.lookup uuid widgets of
    Nothing -> return kernelState
    Just (Widget widget) ->
      case msgType $ header req of
        CommDataMessage -> do
          disp <- run $ comm widget dat communicate
          pgrOut <- liftIO $ readMVar pagerOutput
          liftIO $ publish $ FinalResult disp (if toUsePager then pgrOut else []) []
          return kernelState
        CommCloseMessage -> do
          disp <- run $ close widget dat
          pgrOut <- liftIO $ readMVar pagerOutput
          liftIO $ publish $ FinalResult disp (if toUsePager then pgrOut else []) []
          return kernelState { openComms = Map.delete uuid widgets }

  -- Notify the frontend that the kernel is idle once again
  idleHeader <- liftIO $ dupHeader replyHeader StatusMessage
  liftIO . send $ PublishStatus idleHeader Idle

  return newState
