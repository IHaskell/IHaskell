{-# language NoImplicitPrelude, DoAndIfThenElse, OverloadedStrings, ExtendedDefaultRules #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

-- | Description : All message type definitions.
module IHaskell.Types (
    Message(..),
    MessageHeader(..),
    MessageType(..),
    dupHeader,
    setVersion,
    Username,
    Metadata,
    BufferPath,
    replyType,
    ExecutionState(..),
    StreamType(..),
    MimeType(..),
    DisplayData(..),
    ErrorOccurred(..),
    EvaluationResult(..),
    evaluationOutputs,
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

import           Data.Aeson (ToJSON (..), Value, (.=), object, Value(String))
import           Data.Function (on)
import           Data.Text (pack)
import           Data.Binary
import           GHC.Generics

import           IHaskell.IPython.Kernel

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap as KeyMap
#else
import qualified Data.HashMap.Strict as HashMap
#endif

-- | A class for displayable Haskell types.
--
-- IHaskell's displaying of results behaves as if these two overlapping/undecidable instances also
-- existed:
--
-- > instance (Show a) => IHaskellDisplay a
-- > instance Show a where shows _ = id
class IHaskellDisplay a where
  display :: a -> IO Display

type BufferPath = [Text]

emptyBPs :: [BufferPath]
emptyBPs = []

-- | Display as an interactive widget.
class IHaskellDisplay a => IHaskellWidget a where
  -- | Target name for this widget. The actual input parameter should be ignored. By default evaluate
  -- to "jupyter.widget", which is used by IPython for its backbone widgets.
  targetName :: a -> String
  targetName _ = "jupyter.widget"

  -- | Target module for this widget. Evaluates to an empty string by default.
  targetModule :: a -> String
  targetModule _ = ""

  -- | Buffer paths for this widget. Evaluates to an empty array by default.
  getBufferPaths :: a -> [BufferPath]
  getBufferPaths _ = emptyBPs

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

-- | these instances cause the image, html etc. which look like:
--
-- > Display
-- > [Display]
-- > IO [Display]
-- > IO (IO Display)
--
-- be run the IO and get rendered (if the frontend allows it) in the pretty form.
instance IHaskellDisplay a => IHaskellDisplay (IO a) where
  display = (display =<<)

instance IHaskellDisplay Display where
  display = return

instance IHaskellDisplay DisplayData where
  display disp = return $ Display [disp]

instance IHaskellDisplay a => IHaskellDisplay [a] where
  display disps = do
    displays <- mapM display disps
    return $ ManyDisplay displays

data Widget = forall a. IHaskellWidget a => Widget a
  deriving Typeable

instance IHaskellDisplay Widget where
  display (Widget widget) = display widget

instance IHaskellWidget Widget where
  targetName (Widget widget) = targetName widget
  targetModule (Widget widget) = targetModule widget
  getBufferPaths (Widget widget) = getBufferPaths widget
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
  deriving (Show, Eq, Typeable, Generic)

instance ToJSON Display where
  toJSON (Display d) = object (map displayDataToJson d)
  toJSON (ManyDisplay d) = toJSON d

instance Binary Display

instance Semigroup Display where
  ManyDisplay a <> ManyDisplay b = ManyDisplay (a ++ b)
  ManyDisplay a <> b = ManyDisplay (a ++ [b])
  a <> ManyDisplay b = ManyDisplay (a : b)
  a <> b = ManyDisplay [a, b]

instance Monoid Display where
  mempty = Display []
  mappend = (<>)

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
         , htmlCodeWrapperClass :: Maybe String -- ^ HTML output: class name for wrapper div
         , htmlCodeTokenPrefix :: String        -- ^ HTML output: class name prefix for token spans
         }
  deriving Show

defaultKernelState :: KernelState
defaultKernelState = KernelState
  { getExecutionCounter = 1
  , getLintStatus = LintOn
  , useSvg = True
  , useShowErrors = False
  , useShowTypes = False
  , usePager = True
  , openComms = mempty
  , kernelDebug = False
  , supportLibrariesAvailable = True
  , htmlCodeWrapperClass = Just "CodeMirror cm-s-jupyter cm-s-ipython"
  , htmlCodeTokenPrefix = "cm-"
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
                ClrOutput Bool
  -- ^ A 'clear_output' message, sent as a clear_output message
  deriving (Show, Typeable)

data WidgetMethod = UpdateState Value [BufferPath]
                  | CustomContent Value
                  | DisplayWidget

instance ToJSON WidgetMethod where
  toJSON DisplayWidget = object ["method" .= ("display" :: Text)]
  toJSON (UpdateState v bp) = object ["method" .= ("update" :: Text), "state" .= v, "buffer_paths" .= bp]
  toJSON (CustomContent v) = object ["method" .= ("custom" :: Text), "content" .= v]

-- | Output of evaluation.
--
-- A result can either be intermediate or final.
-- Final result has Mimebundles ('DisplayData') and Comm operations
-- ('WidgetMsg') on top of Display outputs.
data EvaluationResult
  -- | An intermediate result which communicates what has been printed thus far.
  = IntermediateResult
        !Display
  | FinalResult
        !Display
        ![DisplayData]
        ![WidgetMsg]
  deriving Show


evaluationOutputs :: EvaluationResult -> Display
evaluationOutputs er =
  case er of
    IntermediateResult outputs -> outputs
    FinalResult outputs _ _ -> outputs

-- | Duplicate a message header, giving it a new UUID and message type.
dupHeader :: MessageHeader -> MessageType -> IO MessageHeader
dupHeader hdr messageType = do
  uuid <- liftIO random
  return hdr { mhMessageId = uuid, mhMsgType = messageType }

-- | Modifies a header and appends the version of the Widget Messaging Protocol as metadata
setVersion :: MessageHeader  -- ^ The header to modify
           -> String         -- ^ The version to set
           -> MessageHeader  -- ^ The modified header

-- We use the 'fromList' function from "Data.HashMap.Strict" (or
-- "Data.Aeson.KeyMap") instead of the 'object' function from "Data.Aeson"
-- because 'object' returns a 'Value', but metadata needs an 'Object'.
#if MIN_VERSION_aeson(2,0,0)
setVersion hdr v = hdr { mhMetadata = Metadata (KeyMap.fromList [("version", String $ pack v)]) }
#else
setVersion hdr v = hdr { mhMetadata = Metadata (HashMap.fromList [("version", String $ pack v)]) }
#endif

-- | Whether or not an error occurred.
data ErrorOccurred = Success
                   | Failure
  deriving (Show, Eq)
