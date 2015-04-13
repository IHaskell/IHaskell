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
-- = Profiles To run your kernel, you will need an IPython profile that causes the frontend to run
-- it. To generate a fresh profile, run the command
--
-- > ipython profile create NAME
--
-- This will create a fresh IPython profile in @~\/.ipython\/profile_NAME@. This profile must be
-- modified in two ways:
--
-- 1. It needs to run your kernel instead of the default ipython 2. It must have message signing
-- turned off, because 'easyKernel' doesn't support it
--
-- == Setting the executable To set the executable, modify the configuration object's
-- @KernelManager.kernel_cmd@ property. For example:
--
-- > c.KernelManager.kernel_cmd = ['my_kernel', '{connection_file}']
--
-- Your own main should arrange to parse command line arguments such
-- that the connection file is passed to easyKernel.
--
-- == Message signing
-- To turn off message signing, use the following snippet:
--
-- > c.Session.key = b''
-- > c.Session.keyfile = b''
--
-- == Further profile improvements
-- Consult the IPython documentation along with the generated profile
-- source code for further configuration of the frontend, including
-- syntax highlighting, logos, help text, and so forth.
module IHaskell.IPython.EasyKernel (easyKernel, installProfile, KernelConfig(..)) where

import           Data.Aeson (decode)

import qualified Data.ByteString.Lazy as BL

import qualified Codec.Archive.Tar as Tar

import           Control.Concurrent (MVar, readChan, writeChan, newMVar, readMVar, modifyMVar_)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad (forever, when, unless)

import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           Data.String (IsString(..))

import           IHaskell.IPython.Kernel
import           IHaskell.IPython.Message.UUID as UUID

import           System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist,
                                   getHomeDirectory)
import           System.FilePath ((</>))
import           System.Exit (exitSuccess)
import           System.IO (openFile, IOMode(ReadMode))

-- | The kernel configuration specifies the behavior that is specific to your language. The type
-- parameters provide the monad in which your kernel will run, the type of intermediate outputs from
-- running cells, and the type of final results of cells, respectively.
data KernelConfig m output result =
       KernelConfig
         { 
         -- | The name of the language. This field is used to calculate the name of the profile,
         -- so it should contain characters that are reasonable to have in file names.
         languageName :: String
         -- | The version of the language
         , languageVersion :: [Int]
         -- | Determine the source of a profile to install using 'installProfile'. The source should be a
         -- tarball whose contents will be unpacked directly into the profile directory. For example, the
         -- file whose name is @ipython_config.py@ in the tar file for a language named @lang@ will end up in
         -- @~/.ipython/profile_lang/ipython_config.py@.
         , profileSource :: IO (Maybe FilePath)
         -- | How to render intermediate output
         , displayOutput :: output -> [DisplayData]
         -- | How to render final cell results
         , displayResult :: result -> [DisplayData]
         -- | Perform completion. The returned tuple consists of the matches, the matched text, and the
         -- completion text. The arguments are the code in the cell, the current line as text, and the column
         -- at which the cursor is placed.
         , completion :: T.Text -> T.Text -> Int -> Maybe ([T.Text], T.Text, T.Text)
         -- | Return the information or documentation for its argument. The returned tuple consists of the
         -- name, the documentation, and the type, respectively.
         , objectInfo :: T.Text -> Maybe (T.Text, T.Text, T.Text)
         -- | Execute a cell. The arguments are the contents of the cell, an IO action that will clear the
         -- current intermediate output, and an IO action that will add a new item to the intermediate
         -- output. The result consists of the actual result, the status to be sent to IPython, and the
         -- contents of the pager. Return the empty string to indicate that there is no pager output. Errors
         -- should be handled by defining an appropriate error constructor in your result type.
         , run :: T.Text -> IO () -> (output -> IO ()) -> m (result, ExecuteReplyStatus, String)
         , debug :: Bool -- ^ Whether to print extra debugging information to
         }

-- the console | Attempt to install the IPython profile from the .tar file indicated by the
-- 'profileSource' field of the configuration, if it is not already installed.
installProfile :: MonadIO m => KernelConfig m output result -> m ()
installProfile config = do
  installed <- isInstalled
  unless installed $ do
    profSrc <- liftIO $ profileSource config
    case profSrc of
      Nothing -> liftIO (putStrLn "No IPython profile is installed or specified")
      Just tar -> do
        profExists <- liftIO $ doesFileExist tar
        profTgt <- profDir
        if profExists
          then do
            liftIO $ createDirectoryIfMissing True profTgt
            liftIO $ Tar.extract profTgt tar
          else liftIO . putStrLn $
            "The supplied profile source '" ++ tar ++ "' does not exist"

  where
    profDir = do
      home <- liftIO getHomeDirectory
      return $ home </> ".ipython" </> ("profile_" ++ languageName config)
    isInstalled = do
      prof <- profDir
      dirThere <- liftIO $ doesDirectoryExist prof
      isProf <- liftIO . doesFileExist $ prof </> "ipython_config.py"
      return $ dirThere && isProf

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
easyKernel :: (MonadIO m, IsString output)
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

replyTo :: (MonadIO m, IsString output)
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
      , language = languageName config
      , versionList = languageVersion config
      }
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
                                                               (languageName config)
                                                               (displayOutput config x)
                                  in run config code clearOutput sendOutput
  liftIO . send $ PublishDisplayData outputHeader (languageName config) (displayResult config res)


  idleHeader <- dupHeader replyHeader StatusMessage
  liftIO . send $ PublishStatus idleHeader Idle

  liftIO $ modifyMVar_ execCount (return . (+ 1))
  counter <- liftIO $ readMVar execCount

  return
    ExecuteReply
      { header = replyHeader
      , pagerOutput = [fromString pagerOut]
      , executionCounter = fromIntegral counter
      , status = replyStatus
      }

replyTo config _ _ req@CompleteRequest{} replyHeader =
  -- TODO: FIX
  error "Unimplemented in IPython 3.0"

replyTo config _ _ ObjectInfoRequest { objectName = obj } replyHeader =
  return $
    case objectInfo config obj of
      Just (name, docs, ty) -> ObjectInfoReply
        { header = replyHeader
        , objectName = obj
        , objectFound = True
        , objectTypeString = ty
        , objectDocString = docs
        }
      Nothing -> ObjectInfoReply
        { header = replyHeader
        , objectName = obj
        , objectFound = False
        , objectTypeString = ""
        , objectDocString = ""
        }

replyTo _ _ _ msg _ = do
  liftIO $ putStrLn "Unknown message: "
  liftIO $ print msg
  return msg

dupHeader :: MonadIO m => MessageHeader -> MessageType -> m MessageHeader
dupHeader hdr mtype =
  do
    uuid <- liftIO UUID.random
    return hdr { messageId = uuid, msgType = mtype }
