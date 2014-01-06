{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, DoAndIfThenElse #-}
module IHaskell.Eval.Stdin (fixStdin, recordParentHeader, recordKernelProfile) where

import ClassyPrelude  hiding (hPutStrLn, readFile, writeFile)

import Prelude (read)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan
import Control.Monad
import GHC.IO.Handle
import GHC.IO.Handle.Types
import System.IO
import System.Posix.IO
import System.IO.Unsafe
import qualified Data.Map as Map

import IHaskell.Types
import IHaskell.IPython
import IHaskell.ZeroMQ
import IHaskell.Message.UUID as UUID

stdinInterface :: MVar ZeroMQStdin
stdinInterface = unsafePerformIO newEmptyMVar

-- | Manipulate standard input so that it is sourced from the IPython
-- frontend. This function is build on layers of deep magical hackery, so
-- be careful modifying it.
fixStdin :: IO ()
fixStdin = do
  -- Initialize the stdin interface.
  dir <- getIHaskellDir
  profile <- read <$> readFile (dir ++ "/.kernel-profile")
  interface <- serveStdin profile
  putMVar stdinInterface interface
  void $ forkIO stdinOnce

stdinOnce :: IO ()
stdinOnce = do
  -- Create a pipe using and turn it into handles.
  (readEnd, writeEnd) <- createPipe
  newStdin <- fdToHandle readEnd
  stdinInput <- fdToHandle writeEnd
  hSetBuffering newStdin NoBuffering
  hSetBuffering stdinInput NoBuffering

  -- Store old stdin and swap in new stdin.
  oldStdin <- hDuplicate stdin
  hDuplicateTo newStdin stdin

  loop stdinInput oldStdin newStdin
  where
    loop stdinInput oldStdin newStdin = do
      let FileHandle _ mvar = stdin
      threadDelay $ 150 * 1000
      empty <- isEmptyMVar mvar
      if not empty
      then loop stdinInput oldStdin newStdin
      else do
        line <- getInputLine
        hPutStr stdinInput $ line ++ "\n"
        loop stdinInput oldStdin newStdin

-- | Get a line of input from the IPython frontend.
getInputLine :: IO String
getInputLine = do
  StdinChannel req rep <- readMVar stdinInterface

  -- Send a request for input.
  uuid <- UUID.random
  dir <- getIHaskellDir
  parentHeader <- read <$> readFile (dir ++ "/.last-req-header")
  let header = MessageHeader {
      username = username parentHeader,
      identifiers = identifiers parentHeader,
      parentHeader = Just parentHeader,
      messageId = uuid,
      sessionId = sessionId parentHeader,
      metadata = Map.fromList [],
      msgType = InputRequestMessage
    }
  let msg = RequestInput header ""
  writeChan req msg

  -- Get the reply.
  InputReply _ value <- readChan rep
  hPrint stderr value
  return value

recordParentHeader :: MessageHeader -> IO ()
recordParentHeader header = do
  dir <- getIHaskellDir
  writeFile (dir ++ "/.last-req-header") $ show header

recordKernelProfile :: Profile -> IO ()
recordKernelProfile profile = do
  dir <- getIHaskellDir
  writeFile (dir ++ "/.kernel-profile") $ show profile
