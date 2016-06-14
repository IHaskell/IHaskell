{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Description : All message type definitions.
module IHaskell.Types (
    Message(..),
    MessageHeader(..),
    MessageType(..),
    dupHeader,
    Username,
    Metadata(..),
    replyType,
    ExecutionState(..),
    StreamType(..),
    MimeType(..),
    DisplayData(..),
    EvaluationResult(..),
    ExecuteReplyStatus(..),
    KernelState(..),
    LintStatus(..),
    Width,
    Height,
    Display(..),
    defaultKernelState,
    extractPlain,
    kernelOpts,
    KernelOpt(..),
    IHaskellDisplay(..),
    IHaskellWidget(..),
    Widget(..),
    WidgetMsg(..),
    WidgetMethod(..),
    KernelSpec(..),
    ) where

import           IHaskellPrelude

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as CBS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

import           Data.Aeson (Value, (.=), object)
import           Data.Aeson.Types (emptyObject)
import qualified Data.ByteString.Char8 as Char
import           Data.Function (on)
import           Data.Serialize
import           GHC.Generics

import           IHaskell.IPython.Kernel

-- | A class for displayable Haskell types.
--
-- IHaskell's displaying of results behaves as if these two overlapping/undecidable instances also
-- existed:
--
-- > instance (Show a) => IHaskellDisplay a
-- > instance Show a where shows _ = id
class IHaskellDisplay a where
  display :: a -> IO Display

-- | Display as an interactive widget.
class IHaskellDisplay a => IHaskellWidget a where
  -- | Target name for this widget. The actual input parameter should be ignored. By default evaluate
  -- to "ipython.widget", which is used by IPython for its backbone widgets.
  targetName :: a -> String
  targetName _ = "ipython.widget"

  -- | Target module for this widget. Evaluates to an empty string by default.
  targetModule :: a -> String
  targetModule _ = ""

  -- | Get the uuid for comm associated with this widget. The widget is responsible for storing the
  -- UUID during initialization.
  getCommUUID :: a -> UUID

  -- | Called when the comm is opened. Allows additional messages to be sent after comm open.
  open :: a                -- ^ Widget to open a comm port with.
       -> (Value -> IO ()) -- ^ A function for sending messages.
       -> IO ()
  open _ _ = return ()

  -- | Respond to a comm data message. Called when a message is recieved on the comm associated with
  -- the widget.
  comm :: a                -- ^ Widget which is being communicated with.
       -> Value            -- ^ Data recieved from the frontend.
       -> (Value -> IO ()) -- ^ Way to respond to the message.
       -> IO ()
  comm _ _ _ = return ()

  -- | Called when a comm_close is recieved from the frontend.
  close :: a               -- ^ Widget to close comm port with.
        -> Value           -- ^ Data recieved from the frontend.
        -> IO ()
  close _ _ = return ()

data Widget = forall a. IHaskellWidget a => Widget a
  deriving Typeable

instance IHaskellDisplay Widget where
  display (Widget widget) = display widget

instance IHaskellWidget Widget where
  targetName (Widget widget) = targetName widget
  targetModule (Widget widget) = targetModule widget
  getCommUUID (Widget widget) = getCommUUID widget
  open (Widget widget) = open widget
  comm (Widget widget) = comm widget
  close (Widget widget) = close widget

instance Show Widget where
  show _ = "<Widget>"

instance Eq Widget where
  (==) = (==) `on` getCommUUID

-- | Wrapper for ipython-kernel's DisplayData which allows sending multiple results from the same
-- expression.
data Display = Display [DisplayData]
             | ManyDisplay [Display]
  deriving (Show, Typeable, Generic)

instance Serialize Display

instance Monoid Display where
  mempty = Display []
  ManyDisplay a `mappend` ManyDisplay b = ManyDisplay (a ++ b)
  ManyDisplay a `mappend` b = ManyDisplay (a ++ [b])
  a `mappend` ManyDisplay b = ManyDisplay (a : b)
  a `mappend` b = ManyDisplay [a, b]

-- | All state stored in the kernel between executions.
data KernelState =
       KernelState
         { getExecutionCounter :: Int
         , getLintStatus :: LintStatus   -- Whether to use hlint, and what arguments to pass it.
         , useSvg :: Bool
         , useShowErrors :: Bool
         , useShowTypes :: Bool
         , usePager :: Bool
         , openComms :: Map UUID Widget
         , kernelDebug :: Bool
         , supportLibrariesAvailable :: Bool
         }
  deriving Show

defaultKernelState :: KernelState
defaultKernelState = KernelState
  { getExecutionCounter = 1
  , getLintStatus = LintOn
  , useSvg = False
  , useShowErrors = False
  , useShowTypes = False
  , usePager = True
  , openComms = mempty
  , kernelDebug = False
  , supportLibrariesAvailable = True
  }

-- | Kernel options to be set via `:set` and `:option`.
data KernelOpt =
       KernelOpt
         { getOptionName :: [String] -- ^ Ways to set this option via `:option`
         , getSetName :: [String] -- ^ Ways to set this option via `:set`
         , getUpdateKernelState :: KernelState -> KernelState -- ^ Function to update the kernel
                                                              -- state.
         }

kernelOpts :: [KernelOpt]
kernelOpts =
  [ KernelOpt ["lint"] [] $ \state -> state { getLintStatus = LintOn }
  , KernelOpt ["no-lint"] [] $ \state -> state { getLintStatus = LintOff }
  , KernelOpt ["svg"] [] $ \state -> state { useSvg = True }
  , KernelOpt ["no-svg"] [] $ \state -> state { useSvg = False }
  , KernelOpt ["show-types"] ["+t"] $ \state -> state { useShowTypes = True }
  , KernelOpt ["no-show-types"] ["-t"] $ \state -> state { useShowTypes = False }
  , KernelOpt ["show-errors"] [] $ \state -> state { useShowErrors = True }
  , KernelOpt ["no-show-errors"] [] $ \state -> state { useShowErrors = False }
  , KernelOpt ["pager"] [] $ \state -> state { usePager = True }
  , KernelOpt ["no-pager"] [] $ \state -> state { usePager = False }
  ]

-- | Current HLint status.
data LintStatus = LintOn
                | LintOff
  deriving (Eq, Show)

-- | Send JSON objects with specific formats
data WidgetMsg = Open Widget Value
               |
               -- ^ Cause the interpreter to open a new comm, and register the associated widget in
               -- the kernelState. Also sends an initial state Value with comm_open.
                Update Widget Value
               |
               -- ^ Cause the interpreter to send a comm_msg containing a state update for the
               -- widget. Can be used to send fragments of state for update. Also updates the value
               -- of widget stored in the kernelState
                View Widget
               |
               -- ^ Cause the interpreter to send a comm_msg containing a display command for the
               -- frontend.
                Close Widget Value
               |
               -- ^ Cause the interpreter to close the comm associated with the widget. Also sends
               -- data with comm_close.
                Custom Widget Value
               |
               -- ^ A [method .= custom, content = value] message
                JSONValue Widget Value
               |
               -- ^ A json object that is sent to the widget without modifications.
                DispMsg Widget Display
               |
               -- ^ A 'display_data' message, sent as a [method .= custom] comm_msg
                ClrOutput Widget Bool
  -- ^ A 'clear_output' message, sent as a [method .= custom] comm_msg
  deriving (Show, Typeable)

data WidgetMethod = UpdateState Value
                  | CustomContent Value
                  | DisplayWidget

instance ToJSON WidgetMethod where
  toJSON DisplayWidget = object ["method" .= "display"]
  toJSON (UpdateState v) = object ["method" .= "update", "state" .= v]
  toJSON (CustomContent v) = object ["method" .= "custom", "content" .= v]

-- | Output of evaluation.
data EvaluationResult =
                      -- | An intermediate result which communicates what has been printed thus
                      -- far.
                        IntermediateResult
                          { outputs :: Display -- ^ Display outputs.
                          }
                      |
                        FinalResult
                          { outputs :: Display       -- ^ Display outputs.
                          , pagerOut :: [DisplayData] -- ^ Mimebundles to display in the IPython
                                                      -- pager.
                          , commMsgs :: [WidgetMsg]  -- ^ Comm operations
                          }
  deriving Show

-- | Duplicate a message header, giving it a new UUID and message type.
dupHeader :: MessageHeader -> MessageType -> IO MessageHeader
dupHeader header messageType = do
  uuid <- liftIO random

  return header { messageId = uuid, msgType = messageType }
