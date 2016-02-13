{-# LANGUAGE NoImplicitPrelude, DeriveFunctor #-}

module IHaskell.Flags (
    IHaskellMode(..),
    Argument(..),
    Args(..),
    LhsStyle(..),
    NotebookFormat(..),
    lhsStyleBird,
    parseFlags,
    help,
    ) where

import           IHaskellPrelude
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as CBS

import           System.Console.CmdArgs.Explicit
import           System.Console.CmdArgs.Text
import           Data.List (findIndex)
import           IHaskell.Types

-- Command line arguments to IHaskell. A set of arguments is annotated with the mode being invoked.
data Args = Args IHaskellMode [Argument]
  deriving Show

data Argument = ConfFile String     -- ^ A file with commands to load at startup.
              | OverwriteFiles      -- ^ Present when output should overwrite existing files. 
              | GhcLibDir String    -- ^ Where to find the GHC libraries.
              | KernelDebug         -- ^ Spew debugging output from the kernel.
              | Help                -- ^ Display help text.
              | Version             -- ^ Display version text.
              | ConvertFrom String
              | ConvertTo String
              | ConvertFromFormat NotebookFormat
              | ConvertToFormat NotebookFormat
              | ConvertLhsStyle (LhsStyle String)
              | KernelspecInstallPrefix String
              | KernelspecUseStack
  deriving (Eq, Show)

data LhsStyle string =
       LhsStyle
         { lhsCodePrefix :: string  -- ^ @>@
         , lhsOutputPrefix :: string  -- ^ @<<@
         , lhsBeginCode :: string  -- ^ @\\begin{code}@
         , lhsEndCode :: string  -- ^ @\\end{code}@
         , lhsBeginOutput :: string  -- ^ @\\begin{verbatim}@
         , lhsEndOutput :: string  -- ^ @\\end{verbatim}@
         }
  deriving (Eq, Functor, Show)

data NotebookFormat = LhsMarkdown
                    | IpynbFile
  deriving (Eq, Show)

-- Which mode IHaskell is being invoked in.
data IHaskellMode = ShowDefault String
                  | InstallKernelSpec
                  | ConvertLhs
                  | Kernel (Maybe String)
  deriving (Eq, Show)

-- | Given a list of command-line arguments, return the IHaskell mode and arguments to process.
parseFlags :: [String] -> Either String Args
parseFlags flags =
  let modeIndex = findIndex (`elem` modeFlags) flags
  in case modeIndex of
    Nothing ->
      -- Treat no mode as 'console'.
      process ihaskellArgs flags
    Just 0 -> process ihaskellArgs flags

    Just idx ->
      -- If mode not first, move it to be first.
      let (start, first:end) = splitAt idx flags
      in process ihaskellArgs $ first : start ++ end
  where
    modeFlags = concatMap modeNames allModes

allModes :: [Mode Args]
allModes = [installKernelSpec, kernel, convert]

-- | Get help text for a given IHaskell ode.
help :: IHaskellMode -> String
help mode = showText (Wrap 100) $ helpText [] HelpFormatAll $ chooseMode mode
  where
    chooseMode InstallKernelSpec = installKernelSpec
    chooseMode (Kernel _) = kernel
    chooseMode ConvertLhs = convert

ghcLibFlag :: Flag Args
ghcLibFlag = flagReq ["ghclib", "l"] (store GhcLibDir) "<path>" "Library directory for GHC."

kernelDebugFlag :: Flag Args
kernelDebugFlag = flagNone ["debug"] addDebug "Print debugging output from the kernel."
  where
    addDebug (Args mode prev) = Args mode (KernelDebug : prev)

kernelStackFlag :: Flag Args
kernelStackFlag = flagNone ["stack"] addStack
                    "Inherit environment from `stack` when it is installed"
  where
    addStack (Args mode prev) = Args mode (KernelspecUseStack : prev)

confFlag :: Flag Args
confFlag = flagReq ["conf", "c"] (store ConfFile) "<rc.hs>"
             "File with commands to execute at start; replaces ~/.ihaskell/rc.hs."

installPrefixFlag :: Flag Args
installPrefixFlag = flagReq ["prefix"] (store KernelspecInstallPrefix) "<install-dir>"
                      "Installation prefix for kernelspec (see Jupyter's --prefix option)"

helpFlag = flagHelpSimple (add Help)

add flag (Args mode flags) = Args mode $ flag : flags

store :: (String -> Argument) -> String -> Args -> Either String Args
store constructor str (Args mode prev) = Right $ Args mode $ constructor str : prev

installKernelSpec :: Mode Args
installKernelSpec =
  mode "install" (Args InstallKernelSpec []) "Install the Jupyter kernelspec." noArgs
    [ghcLibFlag, kernelDebugFlag, confFlag, installPrefixFlag, helpFlag, kernelStackFlag]

kernel :: Mode Args
kernel = mode "kernel" (Args (Kernel Nothing) []) "Invoke the IHaskell kernel." kernelArg
           [ghcLibFlag, kernelDebugFlag, confFlag, kernelStackFlag]
  where
    kernelArg = flagArg update "<json-kernel-file>"
    update filename (Args _ flags) = Right $ Args (Kernel $ Just filename) flags

convert :: Mode Args
convert = mode "convert" (Args ConvertLhs []) description unnamedArg convertFlags
  where
    description = "Convert between Literate Haskell (*.lhs) and Ipython notebooks (*.ipynb)."
    convertFlags = [ flagReq ["input", "i"] (store ConvertFrom) "<file>" "File to read."
                   , flagReq ["output", "o"] (store ConvertTo) "<file>" "File to write."
                   , flagReq ["from", "f"] (storeFormat ConvertFromFormat) "lhs|ipynb"
                       "Format of the file to read."
                   , flagReq ["to", "t"] (storeFormat ConvertToFormat) "lhs|ipynb"
                       "Format of the file to write."
                   , flagNone ["force"] consForce "Overwrite existing files with output."
                   , flagReq ["style", "s"] storeLhs "bird|tex"
                       "Type of markup used for the literate haskell file"
                   , flagNone ["bird"] (consStyle lhsStyleBird) "Literate haskell uses >"
                   , flagNone ["tex"] (consStyle lhsStyleTex) "Literate haskell uses \\begin{code}"
                   , helpFlag
                   ]

    consForce (Args mode prev) = Args mode (OverwriteFiles : prev)
    unnamedArg = Arg (store ConvertFrom) "<file>" False
    consStyle style (Args mode prev) = Args mode (ConvertLhsStyle style : prev)

    storeFormat constructor str (Args mode prev) =
      case T.toLower (T.pack str) of
        "lhs"   -> Right $ Args mode $ constructor LhsMarkdown : prev
        "ipynb" -> Right $ Args mode $ constructor IpynbFile : prev
        _       -> Left $ "Unknown format requested: " ++ str

    storeLhs str previousArgs =
      case T.toLower (T.pack str) of
        "bird" -> success lhsStyleBird
        "tex"  -> success lhsStyleTex
        _      -> Left $ "Unknown lhs style: " ++ str
      where
        success lhsStyle = Right $ consStyle lhsStyle previousArgs

lhsStyleBird, lhsStyleTex :: LhsStyle String
lhsStyleBird = LhsStyle "> " "\n<< " "" "" "" ""

lhsStyleTex = LhsStyle "" "" "\\begin{code}" "\\end{code}" "\\begin{verbatim}" "\\end{verbatim}"

ihaskellArgs :: Mode Args
ihaskellArgs =
  let noMode = mode "IHaskell" defaultReport descr noArgs [helpFlag, versionFlag]
      defaultReport = Args (ShowDefault helpStr) []
      descr = "Haskell for Interactive Computing."
      helpFlag = flagHelpSimple (add Help)
      versionFlag = flagVersion (add Version)
      helpStr = showText (Wrap 100) $ helpText [] HelpFormatAll ihaskellArgs
  in noMode { modeGroupModes = toGroup allModes }
  where
    add flag (Args mode flags) = Args mode $ flag : flags

noArgs = flagArg unexpected ""
  where
    unexpected a = error $ "Unexpected argument: " ++ a
