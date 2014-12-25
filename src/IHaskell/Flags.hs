{-# LANGUAGE NoImplicitPrelude, DeriveFunctor #-}
module IHaskell.Flags (
    IHaskellMode(..),
    Argument(..),
    Args(..),
    LhsStyle(..),
    lhsStyleBird,
    NotebookFormat(..),
    parseFlags,
    help,
    ) where

import           ClassyPrelude
import           System.Console.CmdArgs.Explicit
import           System.Console.CmdArgs.Text
import           Data.List (findIndex)
import           IHaskell.Types

-- Command line arguments to IHaskell.  A set of aruments is annotated with
-- the mode being invoked.
data Args = Args IHaskellMode [Argument]
  deriving Show

data Argument = ServeFrom String    -- ^ Which directory to serve notebooks from.
              | Extension String    -- ^ An extension to load at startup.
              | ConfFile String     -- ^ A file with commands to load at startup.
              | IPythonFrom String  -- ^ Which executable to use for IPython.
              | OverwriteFiles      -- ^ Present when output should overwrite existing files. 
              | ConvertFrom String
              | ConvertTo String
              | ConvertFromFormat NotebookFormat
              | ConvertToFormat NotebookFormat
              | ConvertLhsStyle (LhsStyle String)
              | GhcLibDir String    -- ^ Where to find the GHC libraries.
              | Help                -- ^ Display help text.
  deriving (Eq, Show)

data LhsStyle string = LhsStyle { lhsCodePrefix :: string  -- ^ @>@
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
-- `None` means no mode was specified.
data IHaskellMode = ShowHelp String
                  | Notebook
                  | Console
                  | ConvertLhs
                  | Kernel (Maybe String)
                  | View (Maybe ViewFormat) (Maybe String)
  deriving (Eq, Show)

-- | Given a list of command-line arguments, return the IHaskell mode and
-- arguments to process.
parseFlags :: [String] -> Either String Args
parseFlags flags = 
  let modeIndex = findIndex (`elem` modeFlags) flags in
    case modeIndex of
      Nothing -> Left $ "No mode provided. Modes available are: " ++ show modeFlags ++ "\n" ++
                       pack (showText (Wrap 100) $ helpText [] HelpFormatAll ihaskellArgs)
      Just 0 -> process ihaskellArgs flags

      -- If mode not first, move it to be first.
      Just idx -> 
        let (start, first:end) = splitAt idx flags in
          process ihaskellArgs $ first:start ++ end
  where
    modeFlags = concatMap modeNames allModes

allModes :: [Mode Args]
allModes = [console, notebook, view, kernel, convert]

-- | Get help text for a given IHaskell ode.
help :: IHaskellMode -> String
help mode = showText (Wrap 100) $ helpText [] HelpFormatAll $ chooseMode mode
  where
    chooseMode Console = console
    chooseMode Notebook = notebook
    chooseMode (Kernel _) = kernel
    chooseMode ConvertLhs = convert

ipythonFlag :: Flag Args
ipythonFlag = flagReq ["ipython", "i"] (store IPythonFrom) "<path>" "Executable for IPython."

ghcLibFlag :: Flag Args
ghcLibFlag = flagReq ["ghclib", "l"] (store GhcLibDir) "<path>" "Library directory for GHC."

universalFlags :: [Flag Args]
universalFlags = [ flagReq ["extension", "e", "X"] (store Extension) "<ghc-extension>"
                     "Extension to enable at start."
                 , flagReq ["conf", "c"] (store ConfFile) "<rc.hs>"
                     "File with commands to execute at start; replaces ~/.ihaskell/rc.hs."
                 , flagHelpSimple (add Help)
                 ]
  where
    add flag (Args mode flags) = Args mode $ flag : flags

store :: (String -> Argument) -> String -> Args -> Either String Args
store constructor str (Args mode prev) = Right $ Args mode $ constructor str : prev

notebook :: Mode Args
notebook = mode "notebook" (Args Notebook []) "Browser-based notebook interface." noArgs $
  flagReq ["serve","s"] (store ServeFrom) "<dir>" "Directory to serve notebooks from.":
  ipythonFlag:
  universalFlags

console :: Mode Args
console = mode "console" (Args Console []) "Console-based interactive repl." noArgs $ ipythonFlag : universalFlags

kernel :: Mode Args
kernel = mode "kernel" (Args (Kernel Nothing) []) "Invoke the IHaskell kernel." kernelArg [ghcLibFlag]
  where
    kernelArg = flagArg update "<json-kernel-file>"
    update filename (Args _ flags) = Right $ Args (Kernel $ Just filename) flags

convert :: Mode Args
convert = mode "convert" (Args ConvertLhs []) description unnamedArg convertFlags
  where
    description = "Convert between Literate Haskell (*.lhs) and Ipython notebooks (*.ipynb)."
    convertFlags = universalFlags ++ [ flagReq ["input", "i"] (store ConvertFrom) "<file>"
                                         "File to read."
                                     , flagReq ["output", "o"] (store ConvertTo) "<file>"
                                         "File to write."
                                     , flagReq ["from", "f"] (storeFormat ConvertFromFormat)
                                         "lhs|ipynb" "Format of the file to read."
                                     , flagReq ["to", "t"] (storeFormat ConvertToFormat) "lhs|ipynb"
                                         "Format of the file to write."
                                     , flagNone ["force"] consForce
                                         "Overwrite existing files with output."
                                     , flagReq ["style", "s"] storeLhs "bird|tex"
                                         "Type of markup used for the literate haskell file"
                                     , flagNone ["bird"] (consStyle lhsStyleBird)
                                         "Literate haskell uses >"
                                     , flagNone ["tex"] (consStyle lhsStyleTex)
                                         "Literate haskell uses \\begin{code}"
                                     ]

    consForce (Args mode prev) = Args mode (OverwriteFiles : prev)
    unnamedArg = Arg (store ConvertFrom) "<file>" False
    consStyle style (Args mode prev) = Args mode (ConvertLhsStyle style : prev)

    storeFormat constructor str (Args mode prev) = case toLower str of
      "lhs" -> Right $ Args mode $ constructor LhsMarkdown : prev
      "ipynb" -> Right $ Args mode $ constructor IpynbFile : prev
      _ -> Left $ "Unknown format requested: " ++ str

    storeLhs str previousArgs = case toLower str of
      "bird" -> success lhsStyleBird
      "tex"  -> success lhsStyleTex
      _      -> Left $ "Unknown lhs style: " ++ str
      where
        success lhsStyle = Right $ consStyle lhsStyle previousArgs

lhsStyleBird, lhsStyleTex :: LhsStyle String
lhsStyleBird = LhsStyle "> " "\n<< " "" "" "" ""
lhsStyleTex  = LhsStyle "" "" "\\begin{code}" "\\end{code}" "\\begin{verbatim}" "\\end{verbatim}"


view :: Mode Args
view =
  let empty = mode "view" (Args (View Nothing Nothing) []) "View IHaskell notebook." noArgs flags in
    empty {
      modeNames = ["view"],
      modeCheck  =
        \a@(Args (View fmt file) _) ->
          if not (isJust fmt && isJust file)
          then Left "Syntax: IHaskell view <format> <name>[.ipynb]"
          else Right a,
      modeHelp = concat [
        "Convert an IHaskell notebook to another format.\n",
        "Notebooks are searched in the IHaskell directory and the current directory.\n",
        "Available formats are " ++ intercalate ", " (map show 
          ["pdf", "html", "ipynb", "markdown", "latex"]),
        "."
      ],
      modeArgs = ([formatArg, filenameArg] ++ fst (modeArgs empty),
                  snd $ modeArgs empty)
                                                    
  }
  where
    flags = [ipythonFlag, flagHelpSimple (add Help)]
    formatArg = flagArg updateFmt "<format>"
    filenameArg = flagArg updateFile "<name>[.ipynb]"
    updateFmt fmtStr (Args (View _ s) flags) = 
      case readMay fmtStr of
        Just fmt -> Right $ Args (View (Just fmt) s) flags
        Nothing -> Left $ "Invalid format '" ++ fmtStr ++ "'."
    updateFile name (Args (View f _) flags) = Right $ Args (View f (Just name)) flags
    add flag (Args mode flags) = Args mode $ flag : flags
  
ihaskellArgs :: Mode Args
ihaskellArgs =
  let descr = "Haskell for Interactive Computing." 
      helpStr = showText (Wrap 100) $ helpText [] HelpFormatAll ihaskellArgs
      onlyHelp = [flagHelpSimple (add Help)]
      noMode = mode "IHaskell" (Args (ShowHelp helpStr) []) descr noArgs onlyHelp in
    noMode { modeGroupModes = toGroup allModes }
  where 
    add flag (Args mode flags) = Args mode $ flag : flags

noArgs = flagArg unexpected ""
  where
    unexpected a = error $ "Unexpected argument: " ++ a
