{-# LANGUAGE CPP, ScopedTypeVariables, QuasiQuotes #-}

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
import           System.Exit (exitSuccess)
import           System.Environment (getArgs)
import           System.Posix.Signals
import qualified Data.Map as Map
import           Data.String.Here (hereFile)
import qualified Data.Text.Encoding as E

-- IHaskell imports.
import           IHaskell.Convert (convert)
import           IHaskell.Eval.Completion (complete)
import           IHaskell.Eval.Inspect (inspect)
import           IHaskell.Eval.Evaluate
import           IHaskell.Display
import           IHaskell.Eval.Info
import           IHaskell.Flags
import           IHaskell.IPython
import           IHaskell.Types
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

ihaskellCSS :: String
ihaskellCSS = [hereFile|html/custom.css|]

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

  -- Serve on all sockets and ports defined in the profile.
  interface <- serveProfile profile debug

  -- Create initial state in the directory the kernel *should* be in.
  state <- initialKernelState
  modifyMVar_ state $ \kernelState -> return $
    kernelState { kernelDebug = debug }

  -- Receive and reply to all messages on the shell socket.
  interpret libdir True $ do
    -- Ignore Ctrl-C the first time. This has to go inside the `interpret`, because GHC API resets the
    -- signal handlers for some reason (completely unknown to me).
    liftIO ignoreCtrlC

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
      installHandler keyboardSignal (CatchOnce $ putStrLn "Press Ctrl-C again to quit kernel.")
        Nothing

    isCommMessage req = msgType (header req) `elem` [CommDataMessage, CommCloseMessage]

-- Initial kernel state.
initialKernelState :: IO (MVar KernelState)
initialKernelState = newMVar defaultKernelState

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
  let clearOutput = do
        header <- dupHeader replyHeader ClearOutputMessage
        send $ ClearOutput header True

      sendOutput (ManyDisplay manyOuts) = mapM_ sendOutput manyOuts
      sendOutput (Display outs) = do
        header <- dupHeader replyHeader DisplayDataMessage
        send $ PublishDisplayData header "haskell" $ map (convertSvgToHtml . prependCss) outs

      convertSvgToHtml (DisplayData MimeSvg svg) = html $ makeSvgImg $ base64 $ E.encodeUtf8 svg
      convertSvgToHtml x = x

      makeSvgImg :: Base64 -> String
      makeSvgImg base64data = T.unpack $ "<img src=\"data:image/svg+xml;base64," <>
                                         base64data <>
                                         "\"/>"

      prependCss (DisplayData MimeHtml html) =
        DisplayData MimeHtml $ mconcat ["<style>", T.pack ihaskellCSS, "</style>", html]
      prependCss x = x

      -- Publish outputs, ignore any CommMsgs
      publish :: EvaluationResult -> IO ()
      publish result = do
        let final =
              case result of
                IntermediateResult{} -> False
                FinalResult{}        -> True
            outs = outputs result

        -- If necessary, clear all previous output and redraw.
        clear <- readMVar updateNeeded
        when clear $ do
          clearOutput
          disps <- readMVar displayed
          mapM_ sendOutput $ reverse disps

        -- Draw this message.
        sendOutput outs

        -- If this is the final message, add it to the list of completed messages. If it isn't, make sure we
        -- clear it later by marking update needed as true.
        modifyMVar_ updateNeeded (const $ return $ not final)
        when final $ do
          modifyMVar_ displayed (return . (outs :))

          -- If this has some pager output, store it for later.
          let pager = pagerOut result
          unless (null pager) $
            if usePager state
              then modifyMVar_ pagerOutput (return . (++ pager))
              else sendOutput $ Display pager

      handleMessage :: KernelState -> WidgetMsg -> IO KernelState
      handleMessage state (Open widget initVal stateVal) = do
        -- Check whether the widget is already present in the state
        let oldComms = openComms state
            uuid     = getCommUUID widget
            present  = isJust $ Map.lookup uuid oldComms

            newComms = Map.insert uuid widget $ openComms state
            newState = state { openComms = newComms }

            target   = targetName widget

            communicate val = do
              head <- dupHeader replyHeader CommDataMessage
              writeChan (iopubChannel interface) $ CommData head uuid val

        if present
          then return state
          else do -- Send the comm open
                  header <- dupHeader replyHeader CommOpenMessage
                  send $ CommOpen header target uuid initVal

                  -- Initial state update
                  communicate . toJSON $ UpdateState stateVal

                  -- Send anything else the widget requires.
                  open widget communicate

                  -- Store the widget in the kernelState
                  return newState

      handleMessage state (Close widget value) = do
        let oldComms = openComms state
            present  = isJust $ Map.lookup (getCommUUID widget) oldComms

            target   = targetName widget
            uuid     = getCommUUID widget

            newComms = Map.delete uuid $ openComms state
            newState = state { openComms = newComms }

        if present
          then do header <- dupHeader replyHeader CommCloseMessage
                  send $ CommClose header uuid value
                  return newState
          else return state

      handleMessage state (View widget) = do
        let oldComms = openComms state
            uuid     = getCommUUID widget
            present  = isJust $ Map.lookup (getCommUUID widget) oldComms

        when present $ do
          header <- dupHeader replyHeader CommDataMessage
          send . CommData header uuid $ toJSON DisplayWidget

        return state

      -- Assume that a state update means that it is time the stored widget also gets updated.
      -- Thus replace the stored widget with the copy passed in the CommMsg.
      handleMessage state (Update widget value) = do
        let oldComms = openComms state
            present  = isJust $ Map.lookup (getCommUUID widget) oldComms

            target   = targetName widget
            uuid     = getCommUUID widget

            newComms = Map.insert uuid widget $ openComms state
            newState = state { openComms = newComms }

        if present
          then do header <- dupHeader replyHeader CommDataMessage
                  send . CommData header uuid . toJSON $ UpdateState value
                  return newState
          else return state

      widgetHandler :: KernelState -> [WidgetMsg] -> IO KernelState
      widgetHandler state []     = return state
      widgetHandler state (x:xs) = do
        newState <- handleMessage state x
        widgetHandler newState xs

  let execCount = getExecutionCounter state
  -- Let all frontends know the execution count and code that's about to run
  inputHeader <- liftIO $ dupHeader replyHeader InputMessage
  send $ PublishInput inputHeader (T.unpack code) execCount

  -- Run code and publish to the frontend as we go.
  updatedState <- evaluate state (T.unpack code) publish widgetHandler

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

handleComm :: (Message -> IO ()) -> KernelState -> Message -> MessageHeader -> IO KernelState
handleComm replier kernelState req replyHeader = do
  let widgets = openComms kernelState
      uuid = commUuid req
      dat = commData req
      communicate value = do
        head <- dupHeader replyHeader CommDataMessage
        replier $ CommData head uuid value
  case Map.lookup uuid widgets of
    Nothing -> fail $ "no widget with uuid " ++ show uuid
    Just (Widget widget) ->
      case msgType $ header req of
        CommDataMessage -> do
          comm widget dat communicate
          return kernelState
        CommCloseMessage -> do
          close widget dat
          return kernelState { openComms = Map.delete uuid widgets }
