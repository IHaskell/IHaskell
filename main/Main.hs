{-# language NoImplicitPrelude, DoAndIfThenElse, OverloadedStrings, ExtendedDefaultRules #-}
{-# LANGUAGE CPP, ScopedTypeVariables #-}

-- | Description : Argument parsing and basic messaging loop, using Haskell
--                 Chans to communicate with the ZeroMQ sockets.
module Main (main) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import           IHaskellPrelude

-- Standard library imports.
import           Control.Concurrent.Chan
import           Control.Arrow (second)
import           Data.Aeson hiding (Success)
import           System.Process (readProcess, readProcessWithExitCode)
import           System.Exit (exitSuccess, ExitCode)
import           Control.Exception (try)
import           System.Environment (getArgs)
import           System.Environment (setEnv)
import           System.Posix.Signals
import qualified Data.Map as Map
import           Data.List (break, last)
import           Data.Version (showVersion)

-- IHaskell imports.
import           IHaskell.Convert (convert)
import           IHaskell.Eval.Completion (complete)
import           IHaskell.Eval.Inspect (inspect)
import           IHaskell.Eval.Evaluate
import           IHaskell.Display
import           IHaskell.Eval.Widgets (widgetHandler)
import           IHaskell.Flags
import           IHaskell.IPython
import           IHaskell.Types
import           IHaskell.Publish
import           IHaskell.IPython.ZeroMQ
import           IHaskell.IPython.Types
import qualified IHaskell.IPython.Message.UUID as UUID
import qualified IHaskell.IPython.Stdin as Stdin

-- Cabal imports.
import           Paths_ihaskell(version)

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Key as Key
#else
import qualified Data.HashMap.Strict as HashMap
#endif

main :: IO ()
main = do
  args <- parseFlags <$> getArgs
  case args of
    Left errorMessage -> hPutStrLn stderr errorMessage
    Right xs          -> ihaskell xs

ihaskell :: Args -> IO ()
ihaskell (Args (ShowDefault helpStr) args) = showDefault helpStr args
ihaskell (Args ConvertLhs args) = showingHelp ConvertLhs args $ convert args
ihaskell (Args InstallKernelSpec args) = showingHelp InstallKernelSpec args $ do
  let kernelSpecOpts = parseKernelArgs args
  replaceIPythonKernelspec kernelSpecOpts
  installLabextension (kernelSpecDebug kernelSpecOpts)
ihaskell (Args (Kernel (Just filename)) args) = do
  let kernelSpecOpts = parseKernelArgs args
  runKernel kernelSpecOpts filename
ihaskell a@(Args (Kernel Nothing) _) = do
  hPutStrLn stderr "No kernel profile JSON specified."
  hPutStrLn stderr "This may be a bug!"
  hPrint stderr a

showDefault :: String -> [Argument] -> IO ()
showDefault helpStr flags =
  case find (== Version) flags of
  Just _ ->
    putStrLn (showVersion version)
  Nothing ->
    putStrLn helpStr

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
    addFlag kernelSpecOpts (CodeMirror codemirror) =
      kernelSpecOpts { kernelSpecCodeMirror = codemirror }
    addFlag kernelSpecOpts (HtmlCodeWrapperClass clazz) =
      kernelSpecOpts { kernelSpecHtmlCodeWrapperClass = Just clazz }
    addFlag kernelSpecOpts (HtmlCodeTokenPrefix prefix) =
      kernelSpecOpts { kernelSpecHtmlCodeTokenPrefix = prefix }
    addFlag kernelSpecOpts (GhcLibDir libdir) =
      kernelSpecOpts { kernelSpecGhcLibdir = libdir }
    addFlag kernelSpecOpts (KernelName name) =
      kernelSpecOpts { kernelSpecKernelName = name}
    addFlag kernelSpecOpts (DisplayName name) =
      kernelSpecOpts { kernelSpecDisplayName = name}
    addFlag kernelSpecOpts (RTSFlags rts) =
      kernelSpecOpts { kernelSpecRTSOptions = rts }
    addFlag kernelSpecOpts (KernelspecInstallPrefix prefix) =
      kernelSpecOpts { kernelSpecInstallPrefix = Just prefix }
    addFlag kernelSpecOpts KernelspecUseStack =
      kernelSpecOpts { kernelSpecUseStack = True }
    addFlag kernelSpecOpts (KernelspecStackFlag flag) =
      kernelSpecOpts { kernelSpecStackFlags = flag : (kernelSpecStackFlags kernelSpecOpts) }
    addFlag kernelSpecOpts (KernelspecEnvFile fp) =
      kernelSpecOpts { kernelSpecEnvFile = Just fp }
    addFlag _kernelSpecOpts flag = error $ "Unknown flag" ++ show flag

-- | Run the IHaskell language kernel.
runKernel :: KernelSpecOptions -- ^ Various options from when the kernel was installed.
          -> String            -- ^ File with kernel profile JSON (ports, etc).
          -> IO ()
runKernel kOpts profileSrc = do
  let debug = kernelSpecDebug kOpts
      libdir = kernelSpecGhcLibdir kOpts
      useStack = kernelSpecUseStack kOpts
      stackFlags = kernelSpecStackFlags kOpts

  -- Parse the profile file.
  let profileErr = error $ "ihaskell: "++profileSrc++": Failed to parse profile file"
  profile <- liftM (fromMaybe profileErr . decode) $ LBS.readFile profileSrc

  -- Necessary for `getLine` and their ilk to work.
  dir <- getIHaskellDir
  Stdin.recordKernelProfile dir profile

  when useStack $ do
    -- Detect if we have stack
    runResult <- try $ readProcessWithExitCode "stack" [] ""
    let stack =
          case runResult :: Either SomeException (ExitCode, String, String) of
            Left _ -> False
            Right (_, stackStdout, stackStderr) -> "The Haskell Tool Stack" `isInfixOf` (stackStdout ++ stackStderr)

    when debug $ putStrLn ("Using stack: " <> show stack)

    -- If we're in a stack directory, use `stack` to set the environment
    -- We can't do this with base <= 4.6 because setEnv doesn't exist.
    when stack $ do
      when debug $ putStrLn "Using environment from stack:"
      readProcess "stack" (["exec", "env"] <> stackFlags) "" >>= parseAndSetEnv debug

  case kernelSpecEnvFile kOpts of
    Nothing -> return ()
    Just envFile -> do
      when debug $ putStrLn "Using environment from env file: "
      readFile envFile >>= parseAndSetEnv debug

  -- Serve on all sockets and ports defined in the profile.
  interface <- serveProfile profile debug

  -- Create initial state in the directory the kernel *should* be in.
  state <- initialKernelState kOpts
  modifyMVar_ state $ \kernelState -> return $
    kernelState { kernelDebug = debug }

  -- Receive and reply to all messages on the shell socket.
  interpret libdir True True $ \hasSupportLibraries -> do
    -- Ignore Ctrl-C the first time. This has to go inside the `interpret`, because GHC API resets the
    -- signal handlers for some reason (completely unknown to me).
    _ <- liftIO ignoreCtrlC

    liftIO $ modifyMVar_ state $ \kernelState -> return $
              kernelState { supportLibrariesAvailable = hasSupportLibraries }

    -- Initialize the context by evaluating everything we got from the command line flags.
    let noPublish _ _ = return ()
        noWidget s _ = return s
        evaluator line = void $ do
          -- Create a new state each time.
          stateVar <- liftIO $ initialKernelState kOpts
          st <- liftIO $ takeMVar stateVar
          evaluate st line noPublish noWidget

    confFile <- liftIO $ kernelSpecConfFile kOpts
    case confFile of
      Just filename -> liftIO (readFile filename) >>= evaluator
      Nothing       -> return ()

    forever $ do
      -- Read the request from the request channel.
      request <- liftIO $ readChan $ shellRequestChannel interface

      -- Create a header for the reply.
      replyHeader <- createReplyHeader (header request)

      -- Notify the frontend that the kernel is busy computing. All the headers are copies of the reply
      -- header with a different message type, because this preserves the session ID, parent header, and
      -- other important information.
      busyHeader <- liftIO $ dupHeader replyHeader StatusMessage
      liftIO $ writeChan (iopubChannel interface) $ PublishStatus busyHeader Busy

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
          (newState, reply) <- replyTo kOpts interface request replyHeader oldState
          liftIO $ putMVar state newState

          -- Write the reply to the reply channel.
          liftIO $ writeChan (shellReplyChannel interface) reply

      -- Notify the frontend that we're done computing.
      idleHeader <- liftIO $ dupHeader replyHeader StatusMessage
      liftIO $ writeChan (iopubChannel interface) $ PublishStatus idleHeader Idle

  where
    ignoreCtrlC =
      installHandler keyboardSignal (CatchOnce $ putStrLn "Press Ctrl-C again to quit kernel.")
        Nothing

    isCommMessage req = mhMsgType (header req) `elem` [CommDataMessage, CommCloseMessage]

    parseAndSetEnv debug envLines =
      forM_ (lines envLines) $ \line -> do
        case break (== '=') line of
          (_, []) -> return ()
          (key, _:val) -> do
            when debug $ putStrLn ("\t" <> line)
            setEnv key val

-- Initial kernel state.
initialKernelState :: KernelSpecOptions -> IO (MVar KernelState)
initialKernelState kOpts = newMVar (
  defaultKernelState {
    htmlCodeWrapperClass = kernelSpecHtmlCodeWrapperClass kOpts
    , htmlCodeTokenPrefix = kernelSpecHtmlCodeTokenPrefix kOpts
    })

-- | Create a new message header, given a parent message header.
createReplyHeader :: MessageHeader -> Interpreter MessageHeader
createReplyHeader parent = do
  -- Generate a new message UUID.
  newMessageId <- liftIO UUID.random
  let repType = fromMaybe err (replyType $ mhMsgType parent)
      err = error $ "No reply for message " ++ show (mhMsgType parent)

  return $ MessageHeader (mhIdentifiers parent) (Just parent) mempty
            newMessageId (mhSessionId parent) (mhUsername parent) repType []

-- | Compute a reply to a message.
replyTo :: KernelSpecOptions -> ZeroMQInterface -> Message -> MessageHeader -> KernelState -> Interpreter (KernelState, Message)
-- Reply to kernel info requests with a kernel info reply. No computation needs to be done, as a
-- kernel info reply is a static object (all info is hard coded into the representation of that
-- message type).
replyTo kOpts interface KernelInfoRequest{} replyHeader state = do
  let send msg = liftIO $ writeChan (iopubChannel interface) msg

  -- Notify the frontend that the Kernel is idle
  idleHeader <- liftIO $ dupHeader replyHeader StatusMessage
  send $ PublishStatus idleHeader Idle

  return
    (state, KernelInfoReply
              { header = replyHeader
              , protocolVersion = "5.0"
              , banner = "IHaskell " ++ (showVersion version) ++ " GHC " ++ VERSION_ghc
              , implementation = "IHaskell"
              , implementationVersion = showVersion version
              , languageInfo = LanguageInfo
                { languageName = "haskell"
                , languageVersion = VERSION_ghc
                , languageFileExtension = ".hs"
                , languageCodeMirrorMode = kernelSpecCodeMirror kOpts
                , languagePygmentsLexer = "Haskell"
                , languageMimeType = "text/x-haskell" -- https://jupyter-client.readthedocs.io/en/stable/wrapperkernels.html#MyKernel.language_info
                }
              , status = Ok
              })

replyTo _ _ CommInfoRequest{} replyHeader state =
  let comms = Map.mapKeys (UUID.uuidToString) (openComms state) in
  return
    (state, CommInfoReply
              { header = replyHeader
              , commInfo = Map.map (\(Widget w) -> targetName w) comms
              })

-- Reply to a shutdown request by exiting the main thread. Before shutdown, reply to the request to
-- let the frontend know shutdown is happening.
replyTo _ interface ShutdownRequest { restartPending = pending } replyHeader _ = liftIO $ do
  writeChan (shellReplyChannel interface) $ ShutdownReply replyHeader pending
  exitSuccess

-- Reply to an execution request. The reply itself does not require computation, but this causes
-- messages to be sent to the IOPub socket with the output of the code in the execution request.
replyTo _ interface req@ExecuteRequest { getCode = code } replyHeader state = do
  -- Convenience function to send a message to the IOPub socket.
  let send msg = liftIO $ writeChan (iopubChannel interface) msg

  -- Log things so that we can use stdin.
  dir <- liftIO getIHaskellDir
  liftIO $ Stdin.recordParentHeader dir $ header req

  -- Construct a function for publishing output as this is going. This function accepts a boolean
  -- indicating whether this is the final output and the thing to display. Store the final outputs in
  -- a list so that when we receive an updated non-final output, we can clear the entire output and
  -- re-display with the updated output.
  displayed <- liftIO $ newMVar []
  updateNeeded <- liftIO $ newMVar False
  pOut <- liftIO $ newMVar []

  let execCount = getExecutionCounter state
  -- Let all frontends know the execution count and code that's about to run
  inputHeader <- liftIO $ dupHeader replyHeader ExecuteInputMessage
  send $ PublishInput inputHeader (T.unpack code) execCount

  -- Run code and publish to the frontend as we go.
  let widgetMessageHandler = widgetHandler send replyHeader
      publish = publishResult send replyHeader displayed updateNeeded pOut (usePager state)
  (updatedState, errorOccurred) <- evaluate state (T.unpack code) publish widgetMessageHandler
  let executeReplyStatus = case errorOccurred of
        Success -> Ok
        Failure -> Err

  -- Take pager output if we're using the pager.
  pager <- if usePager state
             then liftIO $ readMVar pOut
             else return []
  return
    (updatedState, ExecuteReply
                     { header = replyHeader
                     , pagerOutput = pager
                     , executionCounter = execCount
                     , status = executeReplyStatus
                     })

-- Check for a trailing empty line. If it doesn't exist, we assume the code is incomplete,
-- otherwise we assume the code is complete. Todo: Implement a mechanism that only requests
-- a trailing empty line, when multiline code is entered.
replyTo _ _ req@IsCompleteRequest{} replyHeader state = do
  isComplete <- isInputComplete
  let reply  = IsCompleteReply { header = replyHeader, reviewResult = isComplete }
  return (state, reply)

  where
    isInputComplete = do
      let code = lines $ inputToReview req
      if nub (last code) == " "
         then return CodeComplete
         else return $ CodeIncomplete $ indent 4
    indent n = take n $ repeat ' '

replyTo _ _ req@CompleteRequest{} replyHeader state = do
  let code = getCode req
      pos = getCursorPos req
  (matchedText, completions) <- complete (T.unpack code) pos

  let start = pos - length matchedText
      end = pos
#if MIN_VERSION_aeson(2,0,0)
      reply = CompleteReply replyHeader (map T.pack completions) start end (Metadata KeyMap.empty) True
#else
      reply = CompleteReply replyHeader (map T.pack completions) start end (Metadata HashMap.empty) True
#endif
  return (state, reply)

replyTo _ _ req@InspectRequest{} replyHeader state = do
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
replyTo _ _ HistoryRequest{} replyHeader state = do
  let reply = HistoryReply
        { header = replyHeader
        -- FIXME
        , historyReply = []
        }
  return (state, reply)

-- Accomodating the workaround for retrieving list of open comms from the kernel
--
-- The main idea is that the frontend opens a comm at kernel startup, whose target is a widget that
-- sends back the list of live comms and commits suicide.
--
-- The message needs to be written to the iopub channel, and not returned from here. If returned,
-- the same message also gets written to the shell channel, which causes issues due to two messages
-- having the same identifiers in their headers.
--
-- Sending the message only on the shell_reply channel doesn't work, so we send it as a comm message
-- on the iopub channel and return the SendNothing message.
replyTo _ interface ocomm@CommOpen{} replyHeader state = do
  let send = liftIO . writeChan (iopubChannel interface)

      incomingUuid = commUuid ocomm
      target = commTargetName ocomm

      targetMatches = target == "ipython.widget"
      valueMatches = commData ocomm == object ["widget_class" .= ("ipywidgets.CommInfo" :: Text)]

      commMap = openComms state
      uuidTargetPairs = map (second targetName) $ Map.toList commMap

#if MIN_VERSION_aeson(2,0,0)
      pairProcessor (x, y) = (Key.fromText $ T.pack (UUID.uuidToString x)) .= object ["target_name" .= T.pack y]
#else
      pairProcessor (x, y) = T.pack (UUID.uuidToString x) .= object ["target_name" .= T.pack y]
#endif

      currentComms = object $ map pairProcessor $ (incomingUuid, "comm") : uuidTargetPairs

      replyValue = object [ "method" .= ("custom" :: Text)
                          , "content" .= object ["comms" .= currentComms]
                          ]

      msg = CommData replyHeader (commUuid ocomm) replyValue

  -- To the iopub channel you go
  when (targetMatches && valueMatches) $ send msg

  return (state, SendNothing)

-- TODO: What else can be implemented?
replyTo _ _ message _ state = do
  liftIO $ hPutStrLn stderr $ "Unimplemented message: " ++ show message
  return (state, SendNothing)

-- | Handle comm messages
handleComm :: (Message -> IO ()) -> KernelState -> Message -> MessageHeader -> Interpreter KernelState
handleComm send kernelState req replyHeader = do
  -- MVars to hold intermediate data during publishing
  displayed <- liftIO $ newMVar []
  updateNeeded <- liftIO $ newMVar False
  pOut <- liftIO $ newMVar []

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
      publish = publishResult send replyHeader displayed updateNeeded pOut toUsePager

  newState <- case Map.lookup uuid widgets of
    Nothing -> return kernelState
    Just (Widget widget) ->
      case mhMsgType $ header req of
        CommDataMessage -> do
          disp <- run $ comm widget dat communicate
          pgrOut <- liftIO $ readMVar pOut
          liftIO $ publish (FinalResult disp (if toUsePager then pgrOut else []) []) Success
          return kernelState
        CommCloseMessage -> do
          disp <- run $ close widget dat
          pgrOut <- liftIO $ readMVar pOut
          liftIO $ publish (FinalResult disp (if toUsePager then pgrOut else []) []) Success
          return kernelState { openComms = Map.delete uuid widgets }
        _ ->
          -- Only sensible thing to do.
          return kernelState

  return newState
