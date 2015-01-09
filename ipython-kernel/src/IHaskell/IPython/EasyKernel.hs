{-# LANGUAGE OverloadedStrings #-}

-- | This module provides automation for writing simple IPython
-- kernels. In particular, it provides a record type that defines
-- configurations and a function that interprets a configuration as an
-- action in some monad that can do IO.
--
-- The configuration consists primarily of functions that implement
-- the various features of a kernel, such as running code, looking up
-- documentation, and performing completion. An example for a simple
-- language that nevertheless has side effects, global state, and
-- timing effects is included in the examples directory.
--
-- Presently, there is no automation for creating the profile in the
-- .ipython directory. One should follow the IPython instructions for
-- this.
module IHaskell.IPython.EasyKernel (easyKernel, KernelConfig(..)) where

import Data.Aeson (decode)

import qualified Data.ByteString.Lazy as BL

import Control.Concurrent (MVar, readChan, writeChan, newMVar, readMVar, modifyMVar_)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad (forever, when)

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

import IHaskell.IPython.Kernel
import IHaskell.IPython.Message.UUID as UUID


import System.Exit (exitSuccess)
import System.IO (openFile, IOMode(ReadMode))

-- | The kernel configuration specifies the behavior that is specific
-- to your language. The type parameters provide the monad in which
-- your kernel will run, the type of intermediate outputs from running
-- cells, and the type of final results of cells, respectively.
data KernelConfig m output result = KernelConfig
  { languageName :: String -- ^ The name of the language
  , languageVersion :: [Int] -- ^ The version of the language
  , displayOutput :: output -> [DisplayData] -- ^ How to render intermediate output
  , displayResult :: result -> [DisplayData] -- ^ How to render final cell results
  , completion :: T.Text -> T.Text -> Int -> Maybe ([T.Text], T.Text, T.Text) 
  -- ^ Perform completion. The returned tuple consists of the matches,
  -- the matched text, and the completion text. The arguments are the
  -- code in the cell, the current line as text, and the column at
  -- which the cursor is placed.
  , objectInfo :: T.Text -> Maybe (T.Text, T.Text, T.Text)
  -- ^ Return the information or documentation for its argument. The
  -- returned tuple consists of the name, the documentation, and the
  -- type, respectively.
  , run :: T.Text -> IO () -> (output -> IO ()) -> m (result, ExecuteReplyStatus, String)
  -- ^ Execute a cell. The arguments are the contents of the cell, an
  -- IO action that will clear the current intermediate output, and an
  -- IO action that will add a new item to the intermediate
  -- output. The result consists of the actual result, the status to
  -- be sent to IPython, and the contents of the pager. Return the
  -- empty string to indicate that there is no pager output. Errors
  -- should be handled by defining an appropriate error constructor in
  -- your result type.
  , debug :: Bool -- ^ Whether to print extra debugging information to
                  -- the console
  }

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

  return MessageHeader {
    identifiers = identifiers parent,
    parentHeader = Just parent,
    metadata = Map.fromList [],
    messageId = newMessageId,
    sessionId = sessionId parent,
    username = username parent,
    msgType = repType
  }



-- | Execute an IPython kernel for a config. Your 'main' action should
-- call this as the last thing it does.
easyKernel :: (MonadIO m) => FilePath -> KernelConfig m output result -> m ()
easyKernel profileFile config = do
  prof <- liftIO $ getProfile profileFile
  zmq@(Channels shellReqChan shellRepChan ctrlReqChan ctrlRepChan iopubChan) <-
    liftIO $ serveProfile prof
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
  return KernelInfoReply
           { header = replyHeader
           , language = languageName config
           , versionList = languageVersion config
           }
replyTo config _ interface ShutdownRequest{restartPending=pending} replyHeader = do
  liftIO $ writeChan (shellReplyChannel interface) $ ShutdownReply replyHeader pending
  liftIO exitSuccess

replyTo config execCount interface req@ExecuteRequest { getCode = code } replyHeader = do
  let send msg = writeChan (iopubChannel interface) msg

  busyHeader <- dupHeader replyHeader StatusMessage
  liftIO . send $ PublishStatus busyHeader Busy

  outputHeader <- dupHeader replyHeader DisplayDataMessage
  (res, replyStatus, pagerOut) <-
    let clearOutput = do
          clearHeader <- dupHeader replyHeader ClearOutputMessage
          send $ ClearOutput clearHeader False
        sendOutput x =
          send $ PublishDisplayData outputHeader (languageName config)
                   (displayOutput config x)
    in run config code clearOutput sendOutput
  liftIO . send $ PublishDisplayData outputHeader (languageName config) (displayResult config res)


  idleHeader <- dupHeader replyHeader StatusMessage
  liftIO . send $ PublishStatus idleHeader Idle

  liftIO $ modifyMVar_ execCount (return . (+1))
  counter <- liftIO $ readMVar execCount

  return ExecuteReply
           { header = replyHeader
           , pagerOutput = pagerOut
           , executionCounter = fromIntegral counter
           , status = replyStatus
           }

replyTo config _ _ req@CompleteRequest{} replyHeader = do
  let code = getCode req
      line = getCodeLine req
      col = getCursorPos req

  return $ case completion config code line col of
             Nothing ->
               CompleteReply
                 { header = replyHeader
                 , completionMatches = []
                 , completionMatchedText = ""
                 , completionText = ""
                 , completionStatus = False
                 }
             Just (matches, matchedText, cmplText) ->
               CompleteReply
                 { header = replyHeader
                 , completionMatches = matches
                 , completionMatchedText = matchedText
                 , completionText = cmplText
                 , completionStatus = True
                 }

replyTo config _ _ ObjectInfoRequest { objectName = obj } replyHeader =
  return $ case objectInfo config obj of
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
  do uuid <- liftIO UUID.random
     return hdr { messageId = uuid , msgType = mtype }
