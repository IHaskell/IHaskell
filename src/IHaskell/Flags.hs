{-# LANGUAGE NoImplicitPrelude #-}
module IHaskell.Flags ( 
  IHaskellMode(..),
  Argument(..),
  Args(..),
  parseFlags,
  help,
  ) where

import ClassyPrelude
import System.Console.CmdArgs.Explicit
import System.Console.CmdArgs.Text
import Data.List (findIndex)

import IHaskell.Types

-- Command line arguments to IHaskell.  A set of aruments is annotated with
-- the mode being invoked.
data Args = Args IHaskellMode [Argument]
          deriving Show

data Argument
  = ServeFrom String    -- ^ Which directory to serve notebooks from.
  | Extension String    -- ^ An extension to load at startup.
  | ConfFile String     -- ^ A file with commands to load at startup.
  | IPythonFrom String  -- ^ Which executable to use for IPython.
  | Help                -- ^ Display help text.
  deriving (Eq, Show)

-- Which mode IHaskell is being invoked in.
-- `None` means no mode was specified.
data IHaskellMode
  = ShowHelp String
  | Notebook
  | Console
  | Kernel (Maybe String)
  | View (Maybe ViewFormat) (Maybe String)
  deriving (Eq, Show)

-- | Given a list of command-line arguments, return the IHaskell mode and
-- arguments to process.
parseFlags :: [String] -> Either String Args
parseFlags flags = 
  let modeIndex = findIndex (`elem` modeFlags) flags in
    case modeIndex of
      Nothing -> Left $ "No mode provided. Modes available are: " ++ show modeFlags
      Just 0 -> process ihaskellArgs flags

      -- If mode not first, move it to be first.
      Just idx -> 
        let (start, first:end) = splitAt idx flags in
          process ihaskellArgs $ first:start ++ end
  where
    modeFlags = concatMap modeNames allModes

allModes :: [Mode Args]
allModes = [console, notebook, view, kernel]

-- | Get help text for a given IHaskell ode.
help :: IHaskellMode -> String
help mode = 
    showText (Wrap 100) $ helpText [] HelpFormatAll $ chooseMode mode
  where
    chooseMode Console = console
    chooseMode Notebook = notebook
    chooseMode (Kernel _) = kernel

ipythonFlag :: Flag Args
ipythonFlag = 
  flagReq ["ipython", "i"] (store IPythonFrom) "<path>" "Executable for IPython."

universalFlags :: [Flag Args]
universalFlags = [
  flagReq ["extension","e", "X"] (store Extension) "<ghc-extension>" "Extension to enable at start.",
  flagReq ["conf","c"] (store ConfFile) "<file.hs>" "File with commands to execute at start.",
  flagHelpSimple (add Help)
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
console = mode "console" (Args Console []) "Console-based interactive repl." noArgs $ ipythonFlag:universalFlags

kernel = mode "kernel" (Args (Kernel Nothing) []) "Invoke the IHaskell kernel." kernelArg []
  where
    kernelArg = flagArg update "<json-kernel-file>"
    update filename (Args _ flags) = Right $ Args (Kernel $ Just filename) flags

view :: Mode Args
view =
  let empty = mode "view" (Args (View Nothing Nothing) []) "View IHaskell notebook." noArgs [ipythonFlag] in
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
    formatArg = flagArg updateFmt "<format>"
    filenameArg = flagArg updateFile "<name>[.ipynb]"
    updateFmt fmtStr (Args (View _ s) flags) = 
      case readMay fmtStr of
        Just fmt -> Right $ Args (View (Just fmt) s) flags
        Nothing -> Left $ "Invalid format '" ++ fmtStr ++ "'."
    updateFile name (Args (View f _) flags) = Right $ Args (View f (Just name)) flags
  
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
