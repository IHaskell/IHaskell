{-# LANGUAGE OverloadedStrings #-}

-- | Description : Easy IPython kernels = Overview This module provides automation for writing
-- simple IPython kernels. In particular, it provides a record type that defines configurations and
-- a function that interprets a configuration as an action in some monad that can do IO.
--
-- The configuration consists primarily of functions that implement the various features of a
-- kernel, such as running code, looking up documentation, and performing completion. An example for
-- a simple language that nevertheless has side effects, global state, and timing effects is
-- included in the examples directory.
--
-- = Kernel Specs
--
-- To run your kernel, you will need to install the kernelspec into the Jupyter namespace. If your
-- kernel name is `kernel`, you will need to run the command:
--
-- > kernel install
--
-- This will inform Jupyter of the kernel so that it may be used.
--
-- == Further profile improvements Consult the IPython documentation along with the generated
-- profile source code for further configuration of the frontend, including syntax highlighting,
-- logos, help text, and so forth.
module IHaskell.IPython.EasyKernel (easyKernel, installKernelspec, KernelConfig(..)) where

import           Data.Aeson (decode, encode)

import qualified Data.ByteString.Lazy as BL

import           System.IO.Temp (withTempDirectory)
import           System.Process (rawSystem)

import           Control.Concurrent (MVar, readChan, writeChan, newMVar, readMVar, modifyMVar_)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad (forever, when, unless, void)

import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T

import           IHaskell.IPython.Kernel
import           IHaskell.IPython.Message.UUID as UUID
import           IHaskell.IPython.Types

import           System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist,
                                   getHomeDirectory, getTemporaryDirectory)
import           System.FilePath ((</>))
import           System.Exit (exitSuccess)
import           System.IO (openFile, IOMode(ReadMode))

-- | The kernel configuration specifies the behavior that is specific to your language. The type
-- parameters provide the monad in which your kernel will run, the type of intermediate outputs from
-- running cells, and the type of final results of cells, respectively.
data KernelConfig m output result =
       KernelConfig
         { 
         -- | Info on the language of the kernel.
         kernelLanguageInfo :: LanguageInfo
         -- | Write all the files into the kernel directory, including `kernel.js`, `logo-64x64.png`, and any
         -- other required files. The directory to write to will be passed to this function, and the return
         -- value should be the kernelspec to be written to `kernel.json`.
         , writeKernelspec :: FilePath -> IO KernelSpec
         -- | How to render intermediate output
         , displayOutput :: output -> [DisplayData]
         -- | How to render final cell results
         , displayResult :: result -> [DisplayData]
         -- | Perform completion. The returned tuple consists of the matched text and completions. The
         -- arguments are the code in the cell and the position of the cursor in the cell.
         , completion :: T.Text -> Int -> m (T.Text, [T.Text])
         -- | Return the information or documentation for its argument, described by the cell contents and
         -- cursor position. The returned value is simply the data to display.
         , inspectInfo :: T.Text -> Int -> m (Maybe [DisplayData])
         -- | Execute a cell. The arguments are the contents of the cell, an IO action that will clear the
         -- current intermediate output, and an IO action that will add a new item to the intermediate
         -- output. The result consists of the actual result, the status to be sent to IPython, and the
         -- contents of the pager. Return the empty string to indicate that there is no pager output. Errors
         -- should be handled by defining an appropriate error constructor in your result type.
         , run :: T.Text -> IO () -> (output -> IO ()) -> m (result, ExecuteReplyStatus, String)
         , debug :: Bool -- ^ Whether to print extra debugging information to
         -- | A One-line description of the kernel
         , kernelBanner :: String
         -- | The version of the messaging specification used by the kernel
         , kernelProtocolVersion :: String
         -- | Name of the kernel implementation
         , kernelImplName :: String
         -- | Version of the kernel implementation
         , kernelImplVersion :: String
         }

-- Install the kernelspec, using the `writeKernelspec` field of the kernel configuration.
installKernelspec :: MonadIO m
                  => KernelConfig m output result -- ^ Kernel configuration to install
                  -> Bool                         -- ^ Whether to use Jupyter `--replace`
                  -> Maybe FilePath               -- ^ (Optional) prefix to install into for Jupyter `--prefix`
                  -> m ()
installKernelspec config replace installPrefixMay =
  liftIO $ withTmpDir $ \tmp -> do
    let kernelDir = tmp </> languageName (kernelLanguageInfo config)
    createDirectoryIfMissing True kernelDir
    kernelSpec <- writeKernelspec config kernelDir

    let filename = kernelDir </> "kernel.json"
    BL.writeFile filename $ encode $ toJSON kernelSpec

    let replaceFlag = ["--replace" | replace]
        installPrefixFlag = maybe ["--user"] (\prefix -> ["--prefix", prefix]) installPrefixMay
        cmd = concat [["kernelspec", "install"], installPrefixFlag, [kernelDir], replaceFlag]
    void $ rawSystem "ipython" cmd
  where
    withTmpDir act = do
      tmp <- getTemporaryDirectory
      withTempDirectory tmp "easyKernel" act

getProfile :: FilePath -> IO Profile
getProfile fn = do
  profData <- openFile fn ReadMode >>= BL.hGetContents
  case decode profData of
    Just prof -> return prof
    Nothing   -> error "Invalid profile data"

createReplyHeader :: MonadIO m => MessageHeader -> m MessageHeader
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

-- | Execute an IPython kernel for a config. Your 'main' action should call this as the last thing
-- it does.
easyKernel :: MonadIO m
           => FilePath -- ^ The connection file provided by the IPython frontend
           -> KernelConfig m output result -- ^ The kernel configuration specifying how to react to
                                           -- messages
           -> m ()
easyKernel profileFile config = do
  prof <- liftIO $ getProfile profileFile
  zmq@(Channels shellReqChan shellRepChan ctrlReqChan ctrlRepChan iopubChan _) <- liftIO $ serveProfile
                                                                                             prof
                                                                                             False
  execCount <- liftIO $ newMVar 0
  forever $ do
    req <- liftIO $ readChan shellReqChan
    repHeader <- createReplyHeader (header req)
    when (debug config) . liftIO $ print req
    reply <- replyTo config execCount zmq req repHeader
    liftIO $ writeChan shellRepChan reply

replyTo :: MonadIO m
        => KernelConfig m output result
        -> MVar Integer
        -> ZeroMQInterface
        -> Message
        -> MessageHeader
        -> m Message
replyTo config _ _ KernelInfoRequest{} replyHeader =
  return
    KernelInfoReply
      { header = replyHeader
      , languageInfo = kernelLanguageInfo config
      , implementation = kernelImplName config
      , implementationVersion = kernelImplVersion config
      , banner = kernelBanner config
      , protocolVersion = kernelProtocolVersion config
      }

replyTo config _ _ CommInfoRequest{} replyHeader =
  return
    CommInfoReply
      { header = replyHeader
      , commInfo = Map.empty }

replyTo config _ interface ShutdownRequest { restartPending = pending } replyHeader = do
  liftIO $ writeChan (shellReplyChannel interface) $ ShutdownReply replyHeader pending
  liftIO exitSuccess

replyTo config execCount interface req@ExecuteRequest { getCode = code } replyHeader = do
  let send = writeChan (iopubChannel interface)

  busyHeader <- dupHeader replyHeader StatusMessage
  liftIO . send $ PublishStatus busyHeader Busy

  outputHeader <- dupHeader replyHeader DisplayDataMessage
  (res, replyStatus, pagerOut) <- let clearOutput = do
                                                      clearHeader <- dupHeader replyHeader
                                                                       ClearOutputMessage
                                                      send $ ClearOutput clearHeader False
                                      sendOutput x =
                                                      send $ PublishDisplayData
                                                               outputHeader
                                                               (languageName $ kernelLanguageInfo
                                                                                 config)
                                                               (displayOutput config x)
                                  in run config code clearOutput sendOutput
  liftIO . send $ PublishDisplayData outputHeader (languageName $ kernelLanguageInfo config)
                    (displayResult config res)


  idleHeader <- dupHeader replyHeader StatusMessage
  liftIO . send $ PublishStatus idleHeader Idle

  liftIO $ modifyMVar_ execCount (return . (+ 1))
  counter <- liftIO $ readMVar execCount

  return
    ExecuteReply
      { header = replyHeader
      , pagerOutput = [DisplayData PlainText $ T.pack pagerOut]
      , executionCounter = fromIntegral counter
      , status = replyStatus
      }

replyTo config _ _ req@CompleteRequest{} replyHeader = do
  let code = getCode req
      pos = getCursorPos req
  (matchedText, completions) <- completion config code pos

  let start = pos - T.length matchedText
      end = pos
      reply = CompleteReply replyHeader completions start end Map.empty True
  return reply

replyTo config _ _ req@InspectRequest{} replyHeader = do
  result <- inspectInfo config (inspectCode req) (inspectCursorPos req)
  let reply =
        case result of
          Just datas -> InspectReply
            { header = replyHeader
            , inspectStatus = True
            , inspectData = datas
            }
          _ -> InspectReply { header = replyHeader, inspectStatus = False, inspectData = [] }
  return reply

replyTo _ _ _ msg _ = do
  liftIO $ putStrLn "Unknown message: "
  liftIO $ print msg
  return msg

dupHeader :: MonadIO m => MessageHeader -> MessageType -> m MessageHeader
dupHeader hdr mtype =
  do
    uuid <- liftIO UUID.random
    return hdr { messageId = uuid, msgType = mtype }
