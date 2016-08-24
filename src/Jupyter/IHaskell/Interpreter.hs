{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Jupyter.IHaskell.Interpreter (
    Interpreter,
    runInterpreter,
    resumeInterpreter,
    InterpreterState,
    KernelState(..),
    ghc,
    ) where

-- Imports from 'transformers'
import           Control.Monad.IO.Class (MonadIO(..))

-- Imports from 'mtl'
import           Control.Monad.State.Lazy (StateT, MonadState(..), runStateT)
import           Control.Monad.Trans (lift)

-- Imports from 'ghc'
import           GHC (runGhc, Ghc, getSessionDynFlags, setSessionDynFlags, HscTarget(HscInterpreted),
                      GhcLink(LinkInMemory), DynFlags(..))
import           GhcMonad (Session, reifyGhc, reflectGhc)
import qualified GhcMonad as GHC

newtype Interpreter a = Interpreter { unInterpreter :: StateT KernelState Ghc a }
  deriving (Functor, Applicative, Monad, MonadState KernelState)

data KernelState = KernelState

defaultKernelState :: KernelState
defaultKernelState = KernelState

data InterpreterState =
       InterpreterState
         { interpreterSession :: Session
         , interpreterState :: KernelState
         }

instance MonadIO Interpreter where
  liftIO = Interpreter . lift . GHC.liftIO

runInterpreter :: Maybe FilePath -> Interpreter a -> IO (a, InterpreterState)
runInterpreter libdirM interpreter =
  runGhc libdirM $ do
    flags <- getSessionDynFlags
    setSessionDynFlags $ flags { hscTarget = HscInterpreted, ghcLink = LinkInMemory }
    (result, state) <- runStateT (unInterpreter interpreter) defaultKernelState
    session <- reifyGhc return
    return (result, InterpreterState session state)

resumeInterpreter :: InterpreterState -> Interpreter a -> IO (a, InterpreterState)
resumeInterpreter (InterpreterState session state) (Interpreter interpreter) = do
  (result, state') <- reflectGhc (runStateT interpreter state) session
  return (result, InterpreterState session state')

ghc :: Ghc a -> Interpreter a
ghc = Interpreter . lift
