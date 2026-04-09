{-# LANGUAGE ForeignFunctionInterface #-}

-- | Functionality for redirecting handles on Windows, corresponding to
-- Posix functionality in System.Posix.IO.
--
-- The core problem this solves is that GHC API's dynamic session has its own
-- copy of stdout (from the dynamically loaded base), which is a different
-- 'Handle' object than anything in the static code (even if we try to redirect
-- statically).
--
-- We need to redirect at the OS file descriptor level on Windows (like in the Posix
-- implementation), but 'hDuplicateTo' won't work because the pipe handle and
-- stdout have incompatible types, causing errors like:
--
-- > hDuplicateTo: illegal operation (handles are incompatible)
--
-- To solve this we use Windows CRT file descriptors, which provide appropriate
-- functionality to replicate the Posix setup.
module IHaskell.Windows.IO
  ( redirectHandle
  , suppressStdin
  ) where

-- base
import Control.Monad
  ( void, when )
import Data.Typeable
  ( cast )
import Foreign.C.Types
  ( CInt(..) )
import GHC.IO.FD
  ( FD(..) )
import GHC.IO.Handle.Types
  ( Handle(..), Handle__(..) )
import System.IO
  ( openFile, hClose, IOMode(..), stdin )

-- stm
import Control.Concurrent.MVar
  ( readMVar )

--------------------------------------------------------------------------------

-- Windows-specific FFI imports for CRT file descriptors
foreign import ccall unsafe "_dup"   c_dup   :: CInt -> IO CInt
foreign import ccall unsafe "_dup2"  c_dup2  :: CInt -> CInt -> IO CInt
foreign import ccall unsafe "_close" c_close :: CInt -> IO CInt

-- | Redirect @dst@ to write through @src@ at the OS file-descriptor level
-- using Windows @_dup@\/@_dup2@ operations.
--
-- Returns an action that restores @dst@.
--
-- This operates on CRT file descriptors (not Haskell Handle state), to
-- ensure that both Haskell and C IO operations get properly rerouted.
redirectHandle :: Handle -> Handle -> IO (IO ())
redirectHandle src dst = do
  srcFd <- handleFd src
  dstFd <- handleFd dst
  savedFd <- c_dup dstFd
  when (savedFd == -1) $
    ioError (userError "redirectHandle: _dup failed")
  r <- c_dup2 srcFd dstFd
  when (r == -1) $ do
    void $ c_close savedFd
    ioError (userError "redirectHandle: _dup2 failed")
  return $ do
    r2 <- c_dup2 savedFd dstFd
    void $ c_close savedFd
    when (r2 == -1) $
      ioError (userError "redirectHandle: restore _dup2 failed")

-- | Redirect 'stdin' to @NUL@ (write-only), causing reads to fail with an
-- @InvalidArgument@ error.
--
-- Returns an action that restores 'stdin'.
suppressStdin :: IO (IO ())
suppressStdin = do
  nullHandle <- openFile "NUL" WriteMode
  restore <- redirectHandle nullHandle stdin
  hClose nullHandle
  return restore

handleFd :: Handle -> IO CInt
handleFd h = do
  Handle__ { haDevice = dev } <- getHandleState h
  case cast dev of
    Just fd -> return (fdFD fd)
    Nothing -> ioError (userError "handleFd: handle has no CRT file descriptor")

getHandleState :: Handle -> IO Handle__
getHandleState (FileHandle _fp handleVar) = readMVar handleVar
getHandleState (DuplexHandle _fp _readSideVar writeSideVar) = readMVar writeSideVar
