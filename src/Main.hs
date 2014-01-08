{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Description : Argument parsing and basic messaging loop, using Haskell
--                 Chans to communicate with the ZeroMQ sockets. 
module Main where
import ClassyPrelude hiding (liftIO)
import Prelude (last)
import Control.Concurrent.Chan
import Control.Concurrent (threadDelay)
import Data.Aeson
import Text.Printf
import System.Exit (exitSuccess)
import System.Directory
import System.Console.CmdArgs.Explicit hiding (complete)

import qualified Data.Map as Map

import IHaskell.Types
import IHaskell.ZeroMQ
import qualified IHaskell.Message.UUID as UUID
import IHaskell.Eval.Evaluate
import IHaskell.Eval.Completion (complete)
import IHaskell.Eval.Info
import qualified Data.ByteString.Char8 as Chars
import IHaskell.IPython
import qualified IHaskell.Eval.Stdin as Stdin

import GHC hiding (extensions)
import Outputable (showSDoc, ppr)

-- Command line arguments to IHaskell.  A set of aruments is annotated with
-- the mode being invoked.
data Args = Args IHaskellMode [Argument]

data Argument
  = ServeFrom String    -- ^ Which directory to serve notebooks from.
  | Extension String    -- ^ An extension to load at startup.
  | ConfFile String     -- ^ A file with commands to load at startup.
  | Help                -- ^ Display help text.
  deriving (Eq, Show)

-- Which mode IHaskell is being invoked in.
-- `None` means no mode was specified.
data IHaskellMode
  = None
  | Notebook
  | Console
  | UpdateIPython
  | Kernel (Maybe String)
  | View (Maybe ViewFormat) (Maybe String)
  deriving (Eq, Show)

main ::  IO ()
main = do
  stringArgs <- map unpack <$> getArgs
  case process ihaskellArgs stringArgs of
    Left errmsg -> putStrLn $ pack errmsg
    Right args ->
      ihaskell args

universalFlags :: [Flag Args]
universalFlags = [
  flagReq ["extension","e", "X"] (store Extension) "<ghc-extension>" "Extension to enable at start.",
  flagReq ["conf","c"] (store ConfFile) "<file.hs>" "File with commands to execute at start.",
  flagHelpSimple (add Help)
  ]
  where 
    add flag (Args mode flags) = Args mode $ flag : flags

store :: (String -> Argument) -> String -> Args -> Either String Args
store constructor str (Args mode prev) = Right $ Args mode $ constructor str : prev

notebook :: Mode Args
notebook = mode "notebook" (Args Notebook []) "Browser-based notebook interface." noArgs $
  flagReq ["serve","s"] (store ServeFrom) "<dir>" "Directory to serve notebooks from.":
  universalFlags

console :: Mode Args
console = mode "console" (Args Console []) "Console-based interactive repl." noArgs universalFlags

kernel = mode "kernel" (Args (Kernel Nothing) []) "Invoke the IHaskell kernel." kernelArg []
  where
    kernelArg = flagArg update "<json-kernel-file>"
    update filename (Args _ flags) = Right $ Args (Kernel $ Just filename) flags

update :: Mode Args
update = mode "update" (Args UpdateIPython []) "Update IPython frontends." noArgs []

view :: Mode Args
view = (modeEmpty $ Args (View Nothing Nothing) []) {
      modeNames = ["view"],
      modeCheck  =
        \a@(Args (View fmt file) _) ->
          if not (isJust fmt && isJust file)
          then Left "Syntax: IHaskell view <format> <name>[.ipynb]"
          else Right a,
      modeHelp = concat [
        "Convert an IHaskell notebook to another format.\n",
        "Notebooks are searched in the IHaskell directory and the current directory.\n",
        "Available formats are " ++ intercalate ", " (map show 
          ["pdf", "html", "ipynb", "markdown", "latex"]),
        "."
      ],
      modeArgs = ([formatArg, filenameArg], Nothing)
                                                    
  }
  where
    formatArg = flagArg updateFmt "<format>"
    filenameArg = flagArg updateFile "<name>[.ipynb]"
    updateFmt fmtStr (Args (View _ s) flags) = 
      case readMay fmtStr of
        Just fmt -> Right $ Args (View (Just fmt) s) flags
        Nothing -> Left $ "Invalid format '" ++ fmtStr ++ "'."
    updateFile name (Args (View f _) flags) = Right $ Args (View f (Just name)) flags
  

ihaskellArgs :: Mode Args
ihaskellArgs =
  let descr = "Haskell for Interactive Computing." 
      onlyHelp = [flagHelpSimple (add Help)]
      noMode = mode "IHaskell" (Args None []) descr noArgs onlyHelp in
    noMode { modeGroupModes = toGroup [console, notebook, view, update, kernel] }
  where 
    add flag (Args mode flags) = Args mode $ flag : flags

noArgs = flagArg unexpected ""
  where
    unexpected a = error $ "Unexpected argument: " ++ a

ihaskell :: Args -> IO ()
-- If no mode is specified, print help text.
ihaskell (Args None _) = 
  print $ helpText [] HelpFormatAll ihaskellArgs

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
      print $ helpText [] HelpFormatAll $ chooseMode mode
    Nothing ->
      act
  where
    chooseMode Console = console
    chooseMode Notebook = notebook
    chooseMode (Kernel _) = kernel
    chooseMode UpdateIPython = update
 
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
  Stdin.recordKernelProfile profile

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
replyTo _ KernelInfoRequest{} replyHeader state =
  return (state, KernelInfoReply { header = replyHeader })

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
  liftIO $ Stdin.recordParentHeader $ header req

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


 
