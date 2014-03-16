{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, DeriveDataTypeable, DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
-- | Description : All message type definitions.
module IHaskell.Types (
  Message (..),
  MessageHeader (..),
  MessageType(..),
  Username,
  Metadata(..),
  replyType,
  ExecutionState (..),
  StreamType(..),
  MimeType(..),
  DisplayData(..),
  EvaluationResult(..),
  ExecuteReplyStatus(..),
  InitInfo(..),
  KernelState(..),
  LintStatus(..),
  Width, Height,
  FrontendType(..),
  ViewFormat(..),
  Display(..),
  defaultKernelState,
  extractPlain,
  kernelOpts,
  KernelOpt(..),
  IHaskellDisplay(..),
  IHaskellWidget(..),
  Widget(..),
  ) where

import            ClassyPrelude
import qualified  Data.ByteString.Char8           as Char
import            Data.Serialize
import            GHC.Generics
import            Data.Map                        (Map, empty)
import            Data.Aeson                      (Value)

import            Text.Read                       as Read hiding (pfail, String)
import            Text.ParserCombinators.ReadP

import IPython.Kernel

data ViewFormat
     = Pdf
     | Html
     | Ipynb
     | Markdown
     | Latex
     deriving Eq

instance Show ViewFormat where
  show Pdf         = "pdf"
  show Html        = "html"
  show Ipynb       = "ipynb"
  show Markdown    = "markdown"
  show Latex       = "latex"

instance Read ViewFormat where
  readPrec = Read.lift $ do
    str <- munch (const True)
    case str of
      "pdf" -> return Pdf
      "html" -> return Html
      "ipynb" -> return Ipynb
      "notebook" -> return Ipynb
      "latex" -> return Latex
      "markdown" -> return Markdown
      "md" -> return Markdown
      _ -> pfail

-- | A class for displayable Haskell types.
--
-- IHaskell's displaying of results behaves as if these two
-- overlapping/undecidable instances also existed:
-- 
-- > instance (Show a) => IHaskellDisplay a
-- > instance Show a where shows _ = id
class IHaskellDisplay a where
  display :: a -> IO Display

-- | Display as an interactive widget.
class IHaskellDisplay a => IHaskellWidget a where
  open :: a               -- ^ Widget to open a comm port with.
       -> Value           -- ^ Comm open metadata.
       -> (Value -> IO ()) -- ^ Way to respond to the message.
       -> IO ()

  comm :: a               -- ^ Widget which is being communicated with.
       -> Value           -- ^ Sent data.
       -> (Value -> IO ()) -- ^ Way to respond to the message.
       -> IO ()

  close :: a               -- ^ Widget to close comm port with.
        -> Value           -- ^ Sent data.
        -> IO ()

data Widget = forall a. IHaskellWidget a => Widget a

instance Show Widget where
  show _ = "<Widget>"


-- | Wrapper for ipython-kernel's DisplayData which allows sending multiple
-- results from the same expression.
data Display = Display [DisplayData]
             | ManyDisplay [Display]
             deriving (Show, Typeable, Generic)
instance Serialize Display

instance Monoid Display where
    mempty = Display []
    ManyDisplay a `mappend` ManyDisplay b = ManyDisplay (a ++ b)
    ManyDisplay a `mappend` b             = ManyDisplay (a ++ [b])
    a             `mappend` ManyDisplay b = ManyDisplay (a : b)
    a             `mappend` b             = ManyDisplay [a,b]

instance Semigroup Display where
  a <> b = a `mappend` b

-- | All state stored in the kernel between executions.
data KernelState = KernelState
  { getExecutionCounter :: Int,
    getLintStatus :: LintStatus,  -- Whether to use hlint, and what arguments to pass it. 
    getFrontend :: FrontendType,
    useSvg :: Bool,
    useShowErrors :: Bool,
    useShowTypes :: Bool,
    openComms :: Map UUID Widget
  }
  deriving Show

defaultKernelState :: KernelState
defaultKernelState = KernelState
  { getExecutionCounter = 1,
    getLintStatus = LintOn,
    getFrontend = IPythonConsole,
    useSvg = True,
    useShowErrors = False,
    useShowTypes = False,
    openComms = empty
  }

data FrontendType
     = IPythonConsole
     | IPythonNotebook
     deriving (Show, Eq, Read)

-- | Kernel options to be set via `:set` and `:option`.
data KernelOpt = KernelOpt {
    getOptionName :: [String],                          -- ^ Ways to set this option via `:option`
    getSetName :: [String],                             -- ^ Ways to set this option via `:set`
    getUpdateKernelState :: KernelState -> KernelState   -- ^ Function to update the kernel state.
  }

kernelOpts :: [KernelOpt]
kernelOpts =
  [ KernelOpt ["lint"]           []     $ \state -> state { getLintStatus = LintOn }
  , KernelOpt ["no-lint"]        []     $ \state -> state { getLintStatus = LintOff }
  , KernelOpt ["svg"]            []     $ \state -> state { useSvg        = True }
  , KernelOpt ["no-svg"]         []     $ \state -> state { useSvg        = False }
  , KernelOpt ["show-types"]     ["+t"] $ \state -> state { useShowTypes  = True }
  , KernelOpt ["no-show-types"]  ["-t"] $ \state -> state { useShowTypes  = False }
  , KernelOpt ["show-errors"]    []     $ \state -> state { useShowErrors = True }
  , KernelOpt ["no-show-errors"] []     $ \state -> state { useShowErrors = False }
  ]

-- | Initialization information for the kernel.
data InitInfo = InitInfo {
  extensions :: [String],   -- ^ Extensions to enable at start.
  initCells :: [String],    -- ^ Code blocks to run before start.
  initDir :: String,        -- ^ Which directory this kernel should pretend to operate in.
  frontend :: FrontendType  -- ^ What frontend this serves.
  }
  deriving (Show, Read)

-- | Current HLint status.
data LintStatus
     = LintOn
     | LintOff
     deriving (Eq, Show)


-- | Output of evaluation.
data EvaluationResult =
  -- | An intermediate result which communicates what has been printed thus
  -- far.
  IntermediateResult {
    outputs :: Display      -- ^ Display outputs.
  }
  | FinalResult {
    outputs :: Display,     -- ^ Display outputs.
    pagerOut :: String            -- ^ Text to display in the IPython pager.
  }
