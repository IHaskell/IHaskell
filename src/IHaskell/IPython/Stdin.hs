{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, DoAndIfThenElse #-}

-- | This module provides a way in which the Haskell standard input may be forwarded to the IPython
-- frontend and thus allows the notebook to use the standard input.
--
-- This relies on the implementation of file handles in GHC, and is generally unsafe and terrible.
-- However, it is difficult to find another way to do it, as file handles are generally meant to
-- point to streams and files, and not networked communication protocols.
--
-- In order to use this module, it must first be initialized with two things. First of all, in order
-- to know how to communicate with the IPython frontend, it must know the kernel profile used for
-- communication. For this, use @recordKernelProfile@ once the profile is known. Both this and
-- @recordParentHeader@ take a directory name where they can store this data.
--
--
-- Finally, the module must know what @execute_request@ message is currently being replied to (which
-- will request the input). Thus, every time the language kernel receives an @execute_request@
-- message, it should inform this module via @recordParentHeader@, so that the module may generate
-- messages with an appropriate parent header set. If this is not done, the IPython frontends will
-- not recognize the target of the communication.
--
-- Finally, in order to activate this module, @fixStdin@ must be called once. It must be passed the
-- same directory name as @recordParentHeader@ and @recordKernelProfile@. Note that if this is being
-- used from within the GHC API, @fixStdin@ /must/ be called from within the GHC session not from
-- the host code.
module IHaskell.IPython.Stdin (fixStdin, recordParentHeader, recordKernelProfile) where

import           IHaskellPrelude
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as CBS

import           Control.Concurrent
import           Control.Applicative ((<$>))
import           Control.Concurrent.Chan
import           Control.Monad
import           GHC.IO.Handle
import           GHC.IO.Handle.Types
import           System.Posix.IO
import           System.IO.Unsafe
import qualified Data.Map as Map

import           IHaskell.IPython.Types
import           IHaskell.IPython.ZeroMQ
import           IHaskell.IPython.Message.UUID as UUID

stdinInterface :: MVar ZeroMQStdin
{-# NOINLINE stdinInterface #-}
stdinInterface = unsafePerformIO newEmptyMVar

-- | Manipulate standard input so that it is sourced from the IPython frontend. This function is
-- build on layers of deep magical hackery, so be careful modifying it.
fixStdin :: String -> IO ()
fixStdin dir = do
  -- Initialize the stdin interface.
  profile <- fromJust . readMay <$> readFile (dir ++ "/.kernel-profile")
  interface <- serveStdin profile
  putMVar stdinInterface interface
  void $ forkIO $ stdinOnce dir

stdinOnce :: String -> IO ()
stdinOnce dir = do
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
          line <- getInputLine dir
          hPutStr stdinInput $ line ++ "\n"
          loop stdinInput oldStdin newStdin

-- | Get a line of input from the IPython frontend.
getInputLine :: String -> IO String
getInputLine dir = do
  StdinChannel req rep <- readMVar stdinInterface

  -- Send a request for input.
  uuid <- UUID.random
  parentHeader <- fromJust . readMay <$> readFile (dir ++ "/.last-req-header")
  let header = MessageHeader
        { username = username parentHeader
        , identifiers = identifiers parentHeader
        , parentHeader = Just parentHeader
        , messageId = uuid
        , sessionId = sessionId parentHeader
        , metadata = Map.fromList []
        , msgType = InputRequestMessage
        }
  let msg = RequestInput header ""
  writeChan req msg

  -- Get the reply.
  InputReply _ value <- readChan rep
  return value

recordParentHeader :: String -> MessageHeader -> IO ()
recordParentHeader dir header =
  writeFile (dir ++ "/.last-req-header") $ show header

recordKernelProfile :: String -> Profile -> IO ()
recordKernelProfile dir profile =
  writeFile (dir ++ "/.kernel-profile") $ show profile
