{-# language NoImplicitPrelude, DoAndIfThenElse, OverloadedStrings, ExtendedDefaultRules #-}
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

import qualified Data.Text as T
import           IHaskellPrelude hiding (Arg(..))

import           System.Console.CmdArgs.Explicit
import           System.Console.CmdArgs.Text
import           Data.List (findIndex)

-- Command line arguments to IHaskell. A set of arguments is annotated with the mode being invoked.
data Args = Args IHaskellMode [Argument]
  deriving Show

data Argument = ConfFile String               -- ^ A file with commands to load at startup.
              | OverwriteFiles                -- ^ Present when output should overwrite existing files.
              | GhcLibDir String              -- ^ Where to find the GHC libraries.
              | RTSFlags [String]             -- ^ Options for the GHC runtime (e.g. heap-size limit
                                              --     or number of threads).
              | KernelDebug                   -- ^ Spew debugging output from the kernel.
              | KernelName String             -- ^ The IPython kernel directory name.
              | DisplayName String            -- ^ The IPython display name.
              | Help                          -- ^ Display help text.
              | Version                       -- ^ Display version text.
              | CodeMirror String             -- ^ change codemirror mode (default=ihaskell)
              | HtmlCodeWrapperClass String   -- ^ set the wrapper class for HTML output
              | HtmlCodeTokenPrefix String    -- ^ set a prefix on each token of HTML output
              | ConvertFrom String
              | ConvertTo String
              | ConvertFromFormat NotebookFormat
              | ConvertToFormat NotebookFormat
              | ConvertLhsStyle (LhsStyle String)
              | KernelspecInstallPrefix String
              | KernelspecUseStack
              | KernelspecStackFlag String
              | KernelspecEnvFile FilePath
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
  let modeIndex = findIndex (`elem` modeFlgs) flags
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
    modeFlgs = concatMap modeNames allModes

allModes :: [Mode Args]
allModes = [installKernelSpec, kernel, convert]

-- | Get help text for a given IHaskell ode.
help :: IHaskellMode -> String
help md = showText (Wrap 100) $ helpText [] HelpFormatAll $ chooseMode md
  where
    chooseMode InstallKernelSpec = installKernelSpec
    chooseMode (Kernel _) = kernel
    chooseMode ConvertLhs = convert
    chooseMode (ShowDefault _) = error "IHaskell.Flags.help: Should never happen."

ghcLibFlag :: Flag Args
ghcLibFlag = flagReq ["ghclib", "l"] (store GhcLibDir) "<path>" "Library directory for GHC."

ghcRTSFlag :: Flag Args
ghcRTSFlag = flagReq ["use-rtsopts"] storeRTS "\"<flags>\""
                  "Runtime options (multithreading etc.). See `ghc +RTS -?`."
 where storeRTS allRTSFlags (Args md prev)
          = fmap (Args md . (:prev) . RTSFlags)
              . parseRTS . words $ filter (/='"') allRTSFlags
       parseRTS ("+RTS":fs)  -- Ignore if this is included (we already wrap
           = parseRTS fs     -- the ihaskell-kernel call in +RTS <flags> -RTS anyway)
       parseRTS ["-RTS"] = Right []
       parseRTS ("-RTS":_)  -- Evil injection of extra arguments? Unlikely, but...
           = Left "Adding non-RTS options to --use-rtsopts not permitted."
       parseRTS (f:fs) = (f:) <$> parseRTS fs
       parseRTS [] = Right []

kernelDebugFlag :: Flag Args
kernelDebugFlag = flagNone ["debug"] addDebug "Print debugging output from the kernel."
  where
    addDebug (Args md prev) = Args md (KernelDebug : prev)

kernelNameFlag :: Flag Args
kernelNameFlag =
  flagReq
    ["kernel-name"]
    (store KernelName)
    "<name>"
    "The directory name of the kernel."

displayNameFlag :: Flag Args
displayNameFlag =
  flagReq
    ["display-name"]
    (store DisplayName)
    "<name>"
    "The display name of the kernel."

kernelCodeMirrorFlag :: Flag Args
kernelCodeMirrorFlag = flagReq ["codemirror"] (store CodeMirror) "<codemirror>"
        "Specify codemirror mode that is used for syntax highlighting (default: ihaskell)."

kernelHtmlCodeWrapperClassFlag :: Flag Args
kernelHtmlCodeWrapperClassFlag = flagReq ["html-code-wrapper-class"] (store HtmlCodeWrapperClass) "CodeMirror cm-s-jupyter cm-s-ipython"
        "Specify class name for wrapper div around HTML output (default: 'CodeMirror cm-s-jupyter cm-s-ipython')"

kernelHtmlCodeTokenPrefixFlag :: Flag Args
kernelHtmlCodeTokenPrefixFlag = flagReq ["html-code-token-prefix"] (store HtmlCodeTokenPrefix) "cm-"
        "Specify class name prefix for each token in HTML output (default: cm-)"

kernelStackFlag :: Flag Args
kernelStackFlag = flagNone ["stack"] addStack
                    "Inherit environment from `stack` when it is installed"
  where
    addStack (Args md prev) = Args md (KernelspecUseStack : prev)

kernelStackExtraFlags :: Flag Args
kernelStackExtraFlags = flagReq ["stack-flag"] (store KernelspecStackFlag) ""
        "Extra flag to pass to `stack` when --stack is used. Can be specified multiple times."

kernelEnvFileFlag :: Flag Args
kernelEnvFileFlag =
  flagReq
    ["env-file"]
    (store KernelspecEnvFile)
    "<file>"
    "Load environment from this file when kernel is installed"

confFlag :: Flag Args
confFlag = flagReq ["conf", "c"] (store ConfFile) "<rc.hs>"
             "File with commands to execute at start; replaces ~/.ihaskell/rc.hs."

installPrefixFlag :: Flag Args
installPrefixFlag = flagReq ["prefix"] (store KernelspecInstallPrefix) "<install-dir>"
                      "Installation prefix for kernelspec (see Jupyter's --prefix option)"

helpFlag :: Flag Args
helpFlag = flagHelpSimple (add Help)

add :: Argument -> Args -> Args
add flag (Args md flags) = Args md $ flag : flags

store :: (String -> Argument) -> String -> Args -> Either String Args
store constructor str (Args md prev) = Right $ Args md $ constructor str : prev

installKernelSpec :: Mode Args
installKernelSpec =
  mode "install" (Args InstallKernelSpec []) "Install the Jupyter kernelspec." noArgs
    [ghcLibFlag, ghcRTSFlag, kernelDebugFlag, kernelNameFlag, displayNameFlag, confFlag, installPrefixFlag, helpFlag, kernelStackFlag, kernelStackExtraFlags, kernelEnvFileFlag]

kernel :: Mode Args
kernel = mode "kernel" (Args (Kernel Nothing) []) "Invoke the IHaskell kernel." kernelArg
           [ghcLibFlag
           , kernelDebugFlag
           , confFlag
           , kernelStackFlag
           , kernelStackExtraFlags
           , kernelEnvFileFlag
           , kernelCodeMirrorFlag
           , kernelHtmlCodeWrapperClassFlag
           , kernelHtmlCodeTokenPrefixFlag
           ]
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

    consForce (Args md prev) = Args md (OverwriteFiles : prev)
    unnamedArg = Arg (store ConvertFrom) "<file>" False
    consStyle style (Args md prev) = Args md (ConvertLhsStyle style : prev)

    storeFormat constructor str (Args md prev) =
      case T.toLower (T.pack str) of
        "lhs"   -> Right $ Args md $ constructor LhsMarkdown : prev
        "ipynb" -> Right $ Args md $ constructor IpynbFile : prev
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
      versionFlag = flagVersion (add Version)
      helpStr = showText (Wrap 100) $ helpText [] HelpFormatAll ihaskellArgs
  in noMode { modeGroupModes = toGroup allModes }

noArgs :: Arg a
noArgs = flagArg unexpected ""
  where
    unexpected a = error $ "Unexpected argument: " ++ a
