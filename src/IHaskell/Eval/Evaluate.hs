{-# LANGUAGE DoAndIfThenElse, NoOverloadedStrings, TypeSynonymInstances, CPP #-}

{- | Description : Wrapper around GHC API, exposing a single `evaluate` interface that runs
                   a statement, declaration, import, or directive.

This module exports all functions used for evaluation of IHaskell input.
-}
module IHaskell.Eval.Evaluate (
  interpret, evaluate, Interpreter, liftIO, typeCleaner, globalImports
  ) where

import ClassyPrelude hiding (init, last, liftIO, head, hGetContents, tail, try)
import Control.Concurrent (forkIO, threadDelay)
import Prelude (putChar, head, tail, last, init, (!!))
import Data.List.Utils
import Data.List (findIndex, and)
import Data.String.Utils
import Text.Printf
import Data.Char as Char
import Data.Dynamic
import Data.Typeable
import qualified Data.Serialize as Serialize
import System.Directory
import Filesystem.Path.CurrentOS (encodeString)
import System.Posix.IO
import System.IO (hGetChar, hFlush)
import System.Random (getStdGen, randomRs)
import Unsafe.Coerce
import Control.Monad (guard)
import System.Process
import System.Exit
import Data.Maybe (fromJust)
import qualified Control.Monad.IO.Class as MonadIO (MonadIO, liftIO)
import qualified MonadUtils (MonadIO, liftIO)
import System.Environment (getEnv)
import qualified Data.Map as Map

import NameSet
import Name
import PprTyThing
import InteractiveEval
import DynFlags
import Type
import Exception (gtry)
import HscTypes
import HscMain
import qualified Linker
import TcType
import Unify
import InstEnv
import GhcMonad (liftIO, withSession)
import GHC hiding (Stmt, TypeSig)
import Exception hiding (evaluate)
import Outputable hiding ((<>))
import Packages
import Module
import qualified Pretty
import FastString
import Bag
import ErrUtils (errMsgShortDoc, errMsgExtraInfo)

import qualified System.IO.Strict as StrictIO

import IHaskell.Types
import IHaskell.IPython
import IHaskell.Eval.Parser
import IHaskell.Eval.Lint
import IHaskell.Display
import qualified IHaskell.Eval.Hoogle as Hoogle
import IHaskell.Eval.Util
import IHaskell.BrokenPackages
import qualified  IHaskell.IPython.Message.UUID      as UUID

import Paths_ihaskell (version)
import Data.Version (versionBranch)

data ErrorOccurred = Success | Failure deriving (Show, Eq)

-- | Enable debugging output
debug :: Bool
debug = False

-- | Set GHC's verbosity for debugging
ghcVerbosity :: Maybe Int
ghcVerbosity = Nothing -- Just 5

ignoreTypePrefixes :: [String]
ignoreTypePrefixes = ["GHC.Types", "GHC.Base", "GHC.Show", "System.IO",
                      "GHC.Float", ":Interactive", "GHC.Num", "GHC.IO",
                      "GHC.Integer.Type"]

typeCleaner :: String -> String
typeCleaner = useStringType . foldl' (.) id (map (`replace` "") fullPrefixes)
  where
    fullPrefixes = map (++ ".") ignoreTypePrefixes
    useStringType = replace "[Char]" "String"

write :: GhcMonad m => String -> m ()
write x = when debug $ liftIO $ hPutStrLn stderr $ "DEBUG: " ++ x

type Interpreter = Ghc

#if MIN_VERSION_ghc(7, 8, 0)
   -- GHC 7.8 exports a MonadIO instance for Ghc
#else
instance MonadIO.MonadIO Interpreter where
    liftIO = MonadUtils.liftIO
#endif

globalImports :: [String]
globalImports =
  [ "import IHaskell.Display()"
  , "import qualified Prelude as IHaskellPrelude"
  , "import qualified System.Directory as IHaskellDirectory"
  , "import qualified IHaskell.Display"
  , "import qualified IHaskell.IPython.Stdin"
  , "import qualified System.Posix.IO as IHaskellIO"
  , "import qualified System.IO as IHaskellSysIO"
  ]

-- | Run an interpreting action. This is effectively runGhc with
-- initialization and importing. First argument indicates whether `stdin`
-- is handled specially, which cannot be done in a testing environment.
interpret :: String -> Bool -> Interpreter a -> IO a
interpret libdir allowedStdin action = runGhc (Just libdir) $ do
  -- If we're in a sandbox, add the relevant package database
  sandboxPackages <- liftIO getSandboxPackageConf
  initGhci sandboxPackages
  case ghcVerbosity of
    Just verb -> do dflags <- getSessionDynFlags
                    void $ setSessionDynFlags $ dflags { verbosity = verb }
    Nothing   -> return ()

  initializeImports

  -- Close stdin so it can't be used.
  -- Otherwise it'll block the kernel forever.
  dir <- liftIO getIHaskellDir
  let cmd = printf "IHaskell.IPython.Stdin.fixStdin \"%s\"" dir
  when allowedStdin $ void $
    runStmt cmd RunToCompletion

  initializeItVariable

  -- Run the rest of the interpreter
  action

-- | Initialize our GHC session with imports and a value for 'it'.
initializeImports :: Interpreter ()
initializeImports = do
  -- Load packages that start with ihaskell-*, aren't just IHaskell,
  -- and depend directly on the right version of the ihaskell library.
  -- Also verify that the packages we load are not broken.
  dflags <- getSessionDynFlags
  broken <- liftIO getBrokenPackages
  displayPackages <- liftIO $ do
    (dflags, _) <- initPackages dflags
    let Just db = pkgDatabase dflags
        packageNames = map (packageIdString . packageConfigId) db

        initStr = "ihaskell-"

        -- Name of the ihaskell package, e.g. "ihaskell-1.2.3.4"
        iHaskellPkgName = initStr ++ intercalate "." (map show (versionBranch version))

        dependsOnRight pkg = not $ null $ do
            pkg <- db
            depId <- depends pkg
            dep <- filter ((== depId) . installedPackageId) db
            guard (iHaskellPkgName `isPrefixOf` packageIdString (packageConfigId dep))

        -- ideally the Paths_ihaskell module could provide a way to get the
        -- hash too (ihaskell-0.2.0.5-f2bce922fa881611f72dfc4a854353b9),
        -- for now. Things will end badly if you also happen to have an
        -- ihaskell-0.2.0.5-ce34eadc18cf2b28c8d338d0f3755502 installed.
        iHaskellPkg = case filter (== iHaskellPkgName) packageNames of
                [x] -> x
                [] -> error ("cannot find required haskell library: " ++ iHaskellPkgName)
                _ -> error ("multiple haskell packages " ++ iHaskellPkgName ++ " found")

        displayPkgs = [ pkgName
                  | pkgName <- packageNames,
                    Just (x:_) <- [stripPrefix initStr pkgName],
                    pkgName `notElem` broken,
                    isAlpha x]

    return displayPkgs

  -- Generate import statements all Display modules.
  let capitalize :: String -> String
      capitalize (first:rest) = Char.toUpper first : rest

      importFmt = "import IHaskell.Display.%s"

      toImportStmt :: String -> String
      toImportStmt = printf importFmt . capitalize . (!! 1) . split "-"

      displayImports = map toImportStmt displayPackages

  -- Import implicit prelude.
  importDecl <- parseImportDecl "import Prelude"
  let implicitPrelude = importDecl { ideclImplicit = True }

  -- Import modules.
  mapM_ (write . ("Importing " ++ )) displayImports
  imports <- mapM parseImportDecl $ globalImports ++ displayImports
  setContext $ map IIDecl $ implicitPrelude : imports

-- | Give a value for the `it` variable.
initializeItVariable :: Interpreter ()
initializeItVariable = do
  -- This is required due to the way we handle `it` in the wrapper
  -- statements - if it doesn't exist, the first statement will fail.
  write "Setting `it` to unit."
  void $ runStmt "let it = ()" RunToCompletion

-- | Publisher for IHaskell outputs.  The first argument indicates whether
-- this output is final (true) or intermediate (false).
type Publisher = (EvaluationResult -> IO ())

-- | Output of a command evaluation.
data EvalOut = EvalOut {
    evalStatus :: ErrorOccurred,
    evalResult :: Display,
    evalState :: KernelState,
    evalPager :: String,
    evalComms :: [CommInfo]
  }

-- | Evaluate some IPython input code.
evaluate :: KernelState                  -- ^ The kernel state.
         -> String                       -- ^ Haskell code or other interpreter commands.
         -> (EvaluationResult -> IO ())   -- ^ Function used to publish data outputs.
         -> Interpreter KernelState
evaluate kernelState code output = do
  cmds <- parseString (strip code)
  let execCount = getExecutionCounter kernelState

  when (getLintStatus kernelState /= LintOff) $ liftIO $ do
    lintSuggestions <- lint cmds
    unless (noResults lintSuggestions) $
      output $ FinalResult lintSuggestions "" []

  updated <- runUntilFailure kernelState (map unloc cmds ++ [storeItCommand execCount])
  return updated {
    getExecutionCounter = execCount + 1
  }
  where
    noResults (Display res) = null res
    noResults (ManyDisplay res) = all noResults res

    runUntilFailure :: KernelState -> [CodeBlock] -> Interpreter KernelState
    runUntilFailure state [] = return state
    runUntilFailure state (cmd:rest) = do
      evalOut <- evalCommand output cmd state

      -- Get displayed channel outputs.
      -- Merge them with normal display outputs.
      dispsIO <- extractValue "IHaskell.Display.displayFromChan"
      dispsMay <- liftIO dispsIO
      let result =
            case dispsMay of
              Nothing -> evalResult evalOut
              Just disps -> evalResult evalOut <> disps
          helpStr = evalPager evalOut

      -- Output things only if they are non-empty.
      let empty = noResults result && null helpStr && null (evalComms evalOut)
      unless empty $
        liftIO $ output $ FinalResult result helpStr (evalComms evalOut)

      -- Make sure to clear all comms we've started.
      let newState = evalState evalOut { evalComms = [] }

      case evalStatus evalOut of
        Success -> runUntilFailure newState rest
        Failure -> return newState

    storeItCommand execCount = Statement $ printf "let it%d = it" execCount

    extractValue :: Typeable a => String -> Interpreter a
    extractValue expr = do
      compiled <- dynCompileExpr expr
      case fromDynamic compiled of
        Nothing -> error "Expecting value!"
        Just result -> return result

safely :: KernelState -> Interpreter EvalOut -> Interpreter EvalOut
safely state = ghandle handler . ghandle sourceErrorHandler
  where
    handler :: SomeException -> Interpreter EvalOut
    handler exception =
      return EvalOut {
        evalStatus = Failure,
        evalResult = displayError $ show exception,
        evalState = state,
        evalPager = "",
        evalComms = []
      }

    sourceErrorHandler :: SourceError -> Interpreter EvalOut
    sourceErrorHandler srcerr = do
      let msgs = bagToList $ srcErrorMessages srcerr
      errStrs <- forM msgs $ \msg -> do
        shortStr <- doc $ errMsgShortDoc msg
        contextStr <- doc $ errMsgExtraInfo msg
        return $ unlines [shortStr, contextStr]

      let fullErr = unlines errStrs

      return EvalOut {
        evalStatus = Failure,
        evalResult = displayError fullErr,
        evalState = state,
        evalPager = "",
        evalComms = []
      }

wrapExecution :: KernelState
              -> Interpreter Display
              -> Interpreter EvalOut
wrapExecution state exec = safely state $ exec >>= \res ->
    return EvalOut {
      evalStatus = Success,
      evalResult = res,
      evalState = state,
      evalPager = "",
      evalComms = []
    }

-- | Return the display data for this command, as well as whether it
-- resulted in an error.
evalCommand :: Publisher -> CodeBlock -> KernelState -> Interpreter EvalOut
evalCommand _ (Import importStr) state = wrapExecution state $ do
  write $ "Import: " ++ importStr
  evalImport importStr

  -- Warn about `it` variable.
  return $ if "Test.Hspec" `isInfixOf` importStr
           then displayError $ "Warning: Hspec is unusable in IHaskell until the resolution of GHC bug #8639." ++
                                "\nThe variable `it` is shadowed and cannot be accessed, even in qualified form."
           else mempty

evalCommand _ (Module contents) state = wrapExecution state $ do
  write $ "Module:\n" ++ contents

  -- Write the module contents to a temporary file in our work directory
  namePieces <- getModuleName contents
  liftIO (print namePieces)
  let directory = "./" ++ intercalate "/" (init namePieces) ++ "/"
      filename = last namePieces ++ ".hs"
  liftIO $ do
    createDirectoryIfMissing True directory
    writeFile (fpFromString $ directory ++ filename) contents

  -- Clear old modules of this name
  let modName = intercalate "." namePieces
  removeTarget $ TargetModule $ mkModuleName modName
  removeTarget $ TargetFile filename Nothing

  -- Remember which modules we've loaded before.
  importedModules <- getContext

  let -- Get the dot-delimited pieces of the module name.
      moduleNameOf :: InteractiveImport -> [String]
      moduleNameOf (IIDecl decl) = split "." . moduleNameString . unLoc . ideclName $ decl
      moduleNameOf (IIModule imp) = split "." . moduleNameString $ imp

      -- Return whether this module prevents the loading of the one we're
      -- trying to load. If a module B exist, we cannot load A.B. All
      -- modules must have unique last names (where A.B has last name B).
      -- However, we *can* just reload a module.
      preventsLoading mod =
        let pieces = moduleNameOf mod in
            last namePieces == last pieces && namePieces /= pieces

  -- If we've loaded anything with the same last name, we can't use this.
  -- Otherwise, GHC tries to load the original *.hs fails and then fails.
  case find preventsLoading importedModules of
    -- If something prevents loading this module, return an error.
    Just previous -> do
      let prevLoaded = intercalate "." (moduleNameOf previous)
      return $ displayError $
        printf "Can't load module %s because already loaded %s" modName prevLoaded

    -- Since nothing prevents loading the module, compile and load it.
    Nothing -> doLoadModule modName modName

-- | Directives set via `:set`.
evalCommand output (Directive SetDynFlag flags) state =
  case words flags of
    -- For a single flag.
    [flag] -> do
      write $ "DynFlags: " ++ flags

      -- Check if this is setting kernel options.
      case find (elem flag . getSetName) kernelOpts of
        -- If this is a kernel option, just set it.
        Just (KernelOpt _ _ updater) ->
            return EvalOut {
              evalStatus = Success,
              evalResult = mempty,
              evalState = updater state,
              evalPager = "",
              evalComms = []
            }

        -- If not a kernel option, must be a dyn flag.
        Nothing -> do
          errs <- setFlags [flag]
          let display = case errs of
                [] -> mempty
                _ -> displayError $ intercalate "\n" errs

          -- For -XNoImplicitPrelude, remove the Prelude import.
          -- For -XImplicitPrelude, add it back in.
          case flag of
            "-XNoImplicitPrelude" ->
              evalImport "import qualified Prelude as Prelude"
            "-XImplicitPrelude" -> do
              importDecl <- parseImportDecl "import Prelude"
              let implicitPrelude = importDecl { ideclImplicit = True }
              imports <- getContext
              setContext $ IIDecl implicitPrelude : imports
            _ -> return ()

          return EvalOut {
            evalStatus = Success,
            evalResult = display,
            evalState = state,
            evalPager = "",
            evalComms = []
          }

    -- Apply many flags.
    flag:manyFlags -> do
      firstEval <- evalCommand output (Directive SetDynFlag flag) state
      case evalStatus firstEval of
        Failure -> return firstEval
        Success -> do
          let newState = evalState firstEval
              results  = evalResult firstEval
          restEval <- evalCommand output (Directive SetDynFlag $ unwords manyFlags) newState
          return restEval {
            evalResult = results ++ evalResult restEval
          }

evalCommand output (Directive SetExtension opts) state = do
  write $ "Extension: " ++ opts
  let set = concatMap (" -X" ++) $ words opts
  evalCommand output (Directive SetDynFlag set) state

evalCommand a (Directive SetOption opts) state = do
  write $ "Option: " ++ opts
  let (existing, nonExisting) = partition optionExists $ words opts
  if not $ null nonExisting
  then
    let err = "No such options: " ++ intercalate ", " nonExisting in
    return EvalOut {
      evalStatus = Failure,
      evalResult = displayError err,
      evalState = state,
      evalPager = "",
      evalComms = []
    }
  else
    let options = mapMaybe findOption $ words opts
        updater = foldl' (.) id $ map getUpdateKernelState options in
      return EvalOut {
        evalStatus = Success,
        evalResult = mempty,
        evalState = updater state,
        evalPager = "",
        evalComms = []
      }
  where
    optionExists = isJust . findOption
    findOption opt =
      find (elem opt . getOptionName) kernelOpts

evalCommand _ (Directive GetType expr) state = wrapExecution state $ do
  write $ "Type: " ++ expr
  formatType <$> ((expr ++ " :: ") ++ ) <$> getType expr

evalCommand _ (Directive GetKind expr) state = wrapExecution state $ do
  write $ "Kind: " ++ expr
  (_, kind) <- GHC.typeKind False expr
  flags <- getSessionDynFlags
  let typeStr = showSDocUnqual flags $ ppr kind
  return $ formatType $ expr ++ " :: " ++ typeStr

evalCommand _ (Directive LoadFile name) state = wrapExecution state $ do
  write $ "Load: " ++ name

  let filename = if endswith ".hs" name
                 then name
                 else name ++ ".hs"
  contents <- readFile $ fpFromString filename
  modName <- intercalate "." <$> getModuleName contents
  doLoadModule filename modName

evalCommand publish (Directive ShellCmd ('!':cmd)) state = wrapExecution state $
  case words cmd of
    "cd":dirs -> do
      -- Get home so we can replace '~` with it.
      homeEither <- liftIO (try $ getEnv "HOME" :: IO (Either SomeException String))
      let home = case homeEither of
                  Left _ -> "~"
                  Right val -> val

      let directory = replace "~" home $ unwords dirs
      exists <- liftIO $ doesDirectoryExist directory
      if exists
      then do
        -- Set the directory in IHaskell native code, for future shell
        -- commands. This doesn't set it for user code, though.
        liftIO $ setCurrentDirectory directory

        -- Set the directory for user code.
        let cmd = printf "IHaskellDirectory.setCurrentDirectory \"%s\"" $
                  replace " " "\\ " $
                  replace "\"" "\\\"" directory
        runStmt cmd RunToCompletion
        return mempty
      else
        return $ displayError $ printf "No such directory: '%s'" directory
    cmd -> liftIO $ do
      (readEnd, writeEnd) <- createPipe
      handle <- fdToHandle writeEnd
      pipe <- fdToHandle readEnd
      let initProcSpec = shell $ unwords cmd
          procSpec = initProcSpec {
            std_in = Inherit,
            std_out = UseHandle handle,
            std_err = UseHandle handle
          }
      (_, _, _, process) <- createProcess procSpec

      -- Accumulate output from the process.
      outputAccum <- liftIO $ newMVar ""

      -- Start a loop to publish intermediate results.
      let
        -- Compute how long to wait between reading pieces of the output.
        -- `threadDelay` takes an argument of microseconds.
        ms = 1000
        delay = 100 * ms

        -- Maximum size of the output (after which we truncate).
        maxSize = 100 * 1000
        incSize = 200
        output str = publish $ IntermediateResult $ Display [plain str]

        loop = do
          -- Wait and then check if the computation is done.
          threadDelay delay

          -- Read next chunk and append to accumulator.
          nextChunk <- readChars pipe "\n" incSize
          modifyMVar_ outputAccum (return . (++ nextChunk))

          -- Check if we're done.
          exitCode <- getProcessExitCode process
          let computationDone = isJust exitCode

          when computationDone $ do
            nextChunk <- readChars pipe "" maxSize
            modifyMVar_ outputAccum (return . (++ nextChunk))

          if not computationDone
          then do
            -- Write to frontend and repeat.
            readMVar outputAccum >>= output
            loop
          else do
            out <- readMVar outputAccum
            case fromJust exitCode of
              ExitSuccess -> return $ Display [plain out]
              ExitFailure code -> do
                let errMsg = "Process exited with error code " ++ show code
                    htmlErr = printf "<span class='err-msg'>%s</span>" errMsg
                return $ Display [plain $ out ++ "\n" ++ errMsg,
                                  html $ printf "<span class='mono'>%s</span>" out ++ htmlErr]

      loop


-- This is taken largely from GHCi's info section in InteractiveUI.
evalCommand _ (Directive GetHelp _) state = do
  write "Help via :help or :?."
  return EvalOut {
    evalStatus = Success,
    evalResult = Display [out],
    evalState = state,
    evalPager = "",
    evalComms = []
  }
  where out = plain $ intercalate "\n"
          ["The following commands are available:"
          ,"    :extension <Extension>    -  Enable a GHC extension."
          ,"    :extension No<Extension>  -  Disable a GHC extension."
          ,"    :type <expression>        -  Print expression type."
          ,"    :info <name>              -  Print all info for a name."
          ,"    :hoogle <query>           -  Search for a query on Hoogle."
          ,"    :doc <ident>              -  Get documentation for an identifier via Hogole."
          ,"    :set -XFlag -Wall         -  Set an option (like ghci)."
          ,"    :option <opt>             -  Set an option."
          ,"    :option no-<opt>          -  Unset an option."
          ,"    :?, :help                 -  Show this help text."
          ,""
          ,"Any prefix of the commands will also suffice, e.g. use :ty for :type."
          ,""
          ,"Options:"
          ,"  lint          - enable or disable linting."
          ,"  svg           - use svg output (cannot be resized)."
          ,"  show-types    - show types of all bound names"
          ,"  show-errors   - display Show instance missing errors normally."
          ]

-- This is taken largely from GHCi's info section in InteractiveUI.
evalCommand _ (Directive GetInfo str) state = safely state $ do
  write $ "Info: " ++ str
  -- Get all the info for all the names we're given.
  strings <- getDescription str

  let output = case getFrontend state of
        IPythonConsole -> unlines strings
        IPythonNotebook -> unlines (map htmlify strings)
      htmlify str =
        printf "<div style='background: rgb(247, 247, 247);'><form><textarea id='code'>%s</textarea></form></div>" str
        ++ script
      script =
        "<script>CodeMirror.fromTextArea(document.getElementById('code'), {mode: 'haskell', readOnly: 'nocursor'});</script>"

  return EvalOut {
    evalStatus = Success,
    evalResult = mempty,
    evalState = state,
    evalPager = output,
    evalComms = []
  }

evalCommand _ (Directive SearchHoogle query) state = safely state $ do
  results <- liftIO $ Hoogle.search query
  return $ hoogleResults state results

evalCommand _ (Directive GetDoc query) state = safely state $ do
  results <- liftIO $ Hoogle.document query
  return $ hoogleResults state results

evalCommand output (Statement stmt) state = wrapExecution state $ do
  write $ "Statement:\n" ++ stmt
  let outputter str = output $ IntermediateResult $ Display [plain str]
  (printed, result) <- capturedStatement outputter stmt
  case result of
    RunOk names -> do
      dflags <- getSessionDynFlags

      let allNames = map (showPpr dflags) names
          isItName name =
            name == "it" ||
            name == "it" ++ show (getExecutionCounter state)
          nonItNames = filter (not . isItName) allNames
          output = [plain printed | not . null $ strip printed]

      write $ "Names: " ++ show allNames

      -- Display the types of all bound names if the option is on.
      -- This is similar to GHCi :set +t.
      if not $ useShowTypes state
      then return $ Display output
      else do
        -- Get all the type strings.
        types <- forM nonItNames $ \name -> do
          theType <- showSDocUnqual dflags . ppr <$> exprType name
          return $ name ++ " :: " ++ theType

        let joined = unlines types
            htmled = unlines $ map formatGetType types

        return $ case extractPlain output of
          "" -> Display [html htmled]

          -- Return plain and html versions.
          -- Previously there was only a plain version.
          text -> Display
            [plain $ joined ++ "\n" ++ text,
             html  $ htmled ++ mono text]

    RunException exception -> throw exception
    RunBreak{} -> error "Should not break."

evalCommand output (Expression expr) state = do
  write $ "Expression:\n" ++ expr

  -- Try to use `display` to convert our type into the output
  -- Dislay If typechecking fails and there is no appropriate
  -- typeclass instance, this will throw an exception and thus `attempt` will
  -- return False, and we just resort to plaintext.
  let displayExpr = printf "(IHaskell.Display.display (%s))" expr :: String
  canRunDisplay <- attempt $ exprType displayExpr

  -- Check if this is a widget.
  let widgetExpr = printf "(IHaskell.Display.Widget (%s))" expr :: String
  isWidget <- attempt $ exprType widgetExpr

  write $ "Can Display: " ++ show canRunDisplay
  write $ "  Is Widget: " ++ show canRunDisplay

  if canRunDisplay
  then do
    -- Use the display. As a result, `it` is set to the output.
    out <- useDisplay displayExpr

    -- Register the `it` object as a widget.
    if isWidget
    then registerWidget out
    else return out

  else do
    -- Evaluate this expression as though it's just a statement.
    -- The output is bound to 'it', so we can then use it.
    evalOut <- evalCommand output (Statement expr) state

    let out = evalResult evalOut
        showErr = isShowError out

    -- If evaluation failed, return the failure.  If it was successful, we
    -- may be able to use the IHaskellDisplay typeclass.
    return $ if not showErr || useShowErrors state
            then evalOut
            else postprocessShowError evalOut

  where
    -- Try to evaluate an action. Return True if it succeeds and False if
    -- it throws an exception. The result of the action is discarded.
    attempt :: Interpreter a -> Interpreter Bool
    attempt action = gcatch (action >> return True) failure
      where failure :: SomeException -> Interpreter Bool
            failure _ = return False

    -- Check if the error is due to trying to print something that doesn't
    -- implement the Show typeclass.
    isShowError (ManyDisplay _) = False
    isShowError (Display errs) =
        -- Note that we rely on this error message being 'type cleaned', so
        -- that `Show` is not displayed as GHC.Show.Show.
        startswith "No instance for (Show" msg &&
        isInfixOf " arising from a use of `print'" msg
      where msg = extractPlain errs

    isSvg (DisplayData mime _) = mime == MimeSvg

    removeSvg :: Display -> Display
    removeSvg (Display disps) = Display $ filter (not . isSvg) disps
    removeSvg (ManyDisplay disps) = ManyDisplay $ map removeSvg disps

    useDisplay displayExpr = do
      -- If there are instance matches, convert the object into
      -- a Display. We also serialize it into a bytestring. We get
      -- the bytestring IO action as a dynamic and then convert back to
      -- a bytestring, which we promptly unserialize. Note that
      -- attempting to do this without the serialization to binary and
      -- back gives very strange errors - all the types match but it
      -- refuses to decode back into a Display.
      -- Suppress output, so as not to mess up console.

      -- First, evaluate the expression in such a way that we have access to `it`.
      io <- isIO expr
      let stmtTemplate = if io
                         then "it <- (%s)"
                         else "let { it = %s }"
      evalOut <- evalCommand output (Statement $ printf stmtTemplate expr) state
      case evalStatus evalOut of
        Failure -> return evalOut
        Success -> wrapExecution state $ do
          -- Compile the display data into a bytestring.
          let compileExpr = "fmap IHaskell.Display.serializeDisplay (IHaskell.Display.display it)"
          displayedBytestring <- dynCompileExpr compileExpr

          -- Convert from the bytestring into a display.
          case fromDynamic displayedBytestring of
            Nothing -> error "Expecting lazy Bytestring"
            Just bytestringIO -> do
              bytestring <- liftIO bytestringIO
              case Serialize.decode bytestring of
                Left err -> error err
                Right display ->
                  return $
                    if useSvg state
                    then display :: Display
                    else removeSvg display

    registerWidget :: EvalOut -> Ghc EvalOut
    registerWidget evalOut =
      case evalStatus evalOut of
        Failure -> return evalOut
        Success -> do
          element <- dynCompileExpr "IHaskell.Display.Widget it"
          case fromDynamic element of
            Nothing -> error "Expecting widget"
            Just widget -> do
              -- Stick the widget in the kernel state.
              uuid <- liftIO UUID.random
              let state = evalState evalOut
                  newComms = Map.insert uuid widget $ openComms state
                  state' = state { openComms = newComms }

              -- Store the fact that we should start this comm.
              return evalOut {
                evalComms = CommInfo widget uuid (targetName widget) : evalComms evalOut,
                evalState = state'
              }

    isIO expr = attempt $ exprType $ printf "((\\x -> x) :: IO a -> IO a) (%s)" expr

    postprocessShowError :: EvalOut -> EvalOut
    postprocessShowError evalOut = evalOut { evalResult = Display $ map postprocess disps }
      where
        Display disps = evalResult evalOut
        text = extractPlain disps

        postprocess (DisplayData MimeHtml _) = html $ printf fmt unshowableType (formatErrorWithClass "err-msg collapse" text) script
          where
            fmt = "<div class='collapse-group'><span class='btn' href='#' id='unshowable'>Unshowable:<span class='show-type'>%s</span></span>%s</div><script>%s</script>"
            script = unlines [
                "$('#unshowable').on('click', function(e) {",
                "    e.preventDefault();",
                "    var $this = $(this);",
                "    var $collapse = $this.closest('.collapse-group').find('.err-msg');",
                "    $collapse.collapse('toggle');",
                "});"
              ]

        postprocess other = other

        unshowableType = fromMaybe "" $ do
          let pieces = words text
              before = takeWhile (/= "arising") pieces
              after = init $ unwords $ tail $ dropWhile (/= "(Show") before

          firstChar <- headMay after
          return $ if firstChar == '('
                   then init $ tail after
                   else after



evalCommand _ (Declaration decl) state = wrapExecution state $ do
  write $ "Declaration:\n" ++ decl
  boundNames <- evalDeclarations decl
  let nonDataNames = filter (not . isUpper . head) boundNames

  -- Display the types of all bound names if the option is on.
  -- This is similar to GHCi :set +t.
  if not $ useShowTypes state
  then return mempty
  else do
    -- Get all the type strings.
    dflags <- getSessionDynFlags
    types <- forM nonDataNames $ \name -> do
      theType <- showSDocUnqual dflags . ppr <$> exprType name
      return $ name ++ " :: " ++ theType

    return $ Display [html $ unlines $ map formatGetType types]

evalCommand _ (TypeSignature sig) state = wrapExecution state $
  -- We purposefully treat this as a "success" because that way execution
  -- continues. Empty type signatures are likely due to a parse error later
  -- on, and we want that to be displayed.
  return $ displayError $ "The type signature " ++ sig ++
                          "\nlacks an accompanying binding."

evalCommand _ (ParseError loc err) state = do
  write "Parse Error."
  return EvalOut {
    evalStatus = Failure,
    evalResult = displayError $ formatParseError loc err,
    evalState = state,
    evalPager = "",
    evalComms = []
  }

evalCommand _ (Pragma (PragmaUnsupported pragmaType) pragmas) state = wrapExecution state $
  return $ displayError $ "Pragmas of type " ++ pragmaType ++
                          "\nare not supported."

evalCommand output (Pragma PragmaLanguage pragmas) state = do
  write $ "Got LANGUAGE pragma " ++ show pragmas
  evalCommand output (Directive SetExtension $ unwords pragmas) state

hoogleResults :: KernelState -> [Hoogle.HoogleResult] -> EvalOut
hoogleResults state results = EvalOut {
    evalStatus = Success,
    evalResult = mempty,
    evalState = state,
    evalPager = output,
    evalComms = []
  }
  where
    fmt =
        case getFrontend state of
          IPythonNotebook -> Hoogle.HTML
          IPythonConsole -> Hoogle.Plain
    output = unlines $ map (Hoogle.render fmt) results

-- Read from a file handle until we hit a delimiter or until we've read
-- as many characters as requested
readChars :: Handle -> String -> Int -> IO String

-- If we're done reading, return nothing.
readChars handle delims 0 = return []

readChars handle delims nchars = do
  -- Try reading a single character. It will throw an exception if the
  -- handle is already closed.
  tryRead <- gtry $ hGetChar handle :: IO (Either SomeException Char)
  case tryRead of
    Right char ->
      -- If this is a delimiter, stop reading.
      if char `elem` delims
      then return [char]
      else do
        next <- readChars handle delims (nchars - 1)
        return $ char:next
    -- An error occurs at the end of the stream, so just stop reading.
    Left _ -> return []


doLoadModule :: String -> String -> Ghc Display
doLoadModule name modName = do
  -- Remember which modules we've loaded before.
  importedModules <- getContext

  flip gcatch (unload importedModules) $ do
    -- Compile loaded modules.
    flags <- getSessionDynFlags
#if MIN_VERSION_ghc(7,8,0)
    let objTarget = defaultObjectTarget platform
        platform = targetPlatform flags
#else
    let objTarget = defaultObjectTarget
#endif
    setSessionDynFlags flags{ hscTarget = objTarget }

    -- Clear old targets to be sure.
    setTargets []
    load LoadAllTargets

    -- Load the new target.
    target <- guessTarget name Nothing
    addTarget target
    result <- load LoadAllTargets

    -- Reset the context, since loading things screws it up.
    initializeItVariable

    -- Add imports
    importDecl <- parseImportDecl $ "import " ++ modName
    let implicitImport = importDecl { ideclImplicit = True }
    setContext $ IIDecl implicitImport : importedModules

    -- Switch back to interpreted mode.
    flags <- getSessionDynFlags
    setSessionDynFlags flags{ hscTarget = HscInterpreted }

    case result of
      Succeeded -> return mempty
      Failed -> return $ displayError $ "Failed to load module " ++ modName
  where
    unload :: [InteractiveImport] -> SomeException -> Ghc Display
    unload imported exception = do
      print $ show exception
      -- Explicitly clear targets
      setTargets []
      load LoadAllTargets

      -- Switch to interpreted mode!
      flags <- getSessionDynFlags
      setSessionDynFlags flags{ hscTarget = HscInterpreted }

      -- Return to old context, make sure we have `it`.
      setContext imported
      initializeItVariable

      return $ displayError $ "Failed to load module " ++ modName ++ ": " ++ show exception

keepingItVariable :: Interpreter a -> Interpreter a
keepingItVariable act = do
  -- Generate the it variable temp name
  gen <- liftIO getStdGen
  let rand = take 20 $ randomRs ('0', '9') gen
      var name = name ++ rand
      goStmt s = runStmt s RunToCompletion
      itVariable = var "it_var_temp_"

  goStmt $ printf "let %s = it" itVariable
  val <- act
  goStmt $ printf "let it = %s" itVariable
  act

capturedStatement :: (String -> IO ())         -- ^ Function used to publish intermediate output.
                  -> String                            -- ^ Statement to evaluate.
                  -> Interpreter (String, RunResult)   -- ^ Return the output and result.
capturedStatement output stmt = do
  -- Generate random variable names to use so that we cannot accidentally
  -- override the variables by using the right names in the terminal.
  gen <- liftIO getStdGen
  let
    -- Variable names generation.
    rand = take 20 $ randomRs ('0', '9') gen
    var name = name ++ rand

    -- Variables for the pipe input and outputs.
    readVariable = var "file_read_var_"
    writeVariable = var "file_write_var_"

    -- Variable where to store old stdout.
    oldVariable = var "old_var_"

    -- Variable used to store true `it` value.
    itVariable = var "it_var_"

    voidpf str = printf $ str ++ " IHaskellPrelude.>> IHaskellPrelude.return ()"

    -- Statements run before the thing we're evaluating.
    initStmts =
      [ printf "let %s = it" itVariable
      , printf "(%s, %s) <- IHaskellIO.createPipe" readVariable writeVariable
      , printf "%s <- IHaskellIO.dup IHaskellIO.stdOutput" oldVariable
      , voidpf "IHaskellIO.dupTo %s IHaskellIO.stdOutput" writeVariable
      , voidpf "IHaskellSysIO.hSetBuffering IHaskellSysIO.stdout IHaskellSysIO.NoBuffering"
      , printf "let it = %s" itVariable
      ]

    -- Statements run after evaluation.
    postStmts =
      [ printf "let %s = it" itVariable
      , voidpf "IHaskellSysIO.hFlush IHaskellSysIO.stdout"
      , voidpf "IHaskellIO.dupTo %s IHaskellIO.stdOutput" oldVariable
      , voidpf "IHaskellIO.closeFd %s" writeVariable
      , printf "let it = %s" itVariable
      ]
    pipeExpr = printf "let %s = %s" (var "pipe_var_") readVariable

    goStmt :: String -> Ghc RunResult
    goStmt s = runStmt s RunToCompletion

  -- Initialize evaluation context.
  void $ forM initStmts goStmt

  -- Get the pipe to read printed output from.
  -- This is effectively the source code of dynCompileExpr from GHC API's
  -- InteractiveEval. However, instead of using a `Dynamic` as an
  -- intermediary, it just directly reads the value. This is incredibly
  -- unsafe! However, for some reason the `getContext` and `setContext`
  -- required by dynCompileExpr (to import and clear Data.Dynamic) cause
  -- issues with data declarations being updated (e.g. it drops newer
  -- versions of data declarations for older ones for unknown reasons).
  -- First, compile down to an HValue.
  Just (_, hValues, _) <- withSession $ liftIO . flip hscStmt pipeExpr
  -- Then convert the HValue into an executable bit, and read the value.
  pipe <- liftIO $ do
    fd <- head <$> unsafeCoerce hValues
    fdToHandle fd

  -- Read from a file handle until we hit a delimiter or until we've read
  -- as many characters as requested
  let
    readChars :: Handle -> String -> Int -> IO String

    -- If we're done reading, return nothing.
    readChars handle delims 0 = return []

    readChars handle delims nchars = do
      -- Try reading a single character. It will throw an exception if the
      -- handle is already closed.
      tryRead <- gtry $ hGetChar handle :: IO (Either SomeException Char)
      case tryRead of
        Right char ->
          -- If this is a delimiter, stop reading.
          if char `elem` delims
          then return [char]
          else do
            next <- readChars handle delims (nchars - 1)
            return $ char:next
        -- An error occurs at the end of the stream, so just stop reading.
        Left _ -> return []


  -- Keep track of whether execution has completed.
  completed <- liftIO $ newMVar False
  finishedReading <- liftIO newEmptyMVar
  outputAccum <- liftIO $ newMVar ""

  -- Start a loop to publish intermediate results.
  let
    -- Compute how long to wait between reading pieces of the output.
    -- `threadDelay` takes an argument of microseconds.
    ms = 1000
    delay = 100 * ms

    -- How much to read each time.
    chunkSize = 100

    -- Maximum size of the output (after which we truncate).
    maxSize = 100 * 1000

    loop = do
      -- Wait and then check if the computation is done.
      threadDelay delay
      computationDone <- readMVar completed

      if not computationDone
      then do
        -- Read next chunk and append to accumulator.
        nextChunk <- readChars pipe "\n" 100
        modifyMVar_ outputAccum (return . (++ nextChunk))

        -- Write to frontend and repeat.
        readMVar outputAccum >>= output
        loop
      else do
        -- Read remainder of output and accumulate it.
        nextChunk <- readChars pipe "" maxSize
        modifyMVar_ outputAccum (return . (++ nextChunk))

        -- We're done reading.
        putMVar finishedReading True

  liftIO $ forkIO loop

  result <- gfinally (goStmt stmt) $ do
    -- Execution is done.
    liftIO $ modifyMVar_ completed (const $ return True)

    -- Finalize evaluation context.
    void $ forM postStmts goStmt

    -- Once context is finalized, reading can finish.
    -- Wait for reading to finish to that the output accumulator is
    -- completely filled.
    liftIO $ takeMVar finishedReading

  printedOutput <- liftIO $ readMVar outputAccum
  return (printedOutput, result)

formatError :: ErrMsg -> String
formatError = formatErrorWithClass "err-msg"

formatErrorWithClass :: String -> ErrMsg -> String
formatErrorWithClass cls =
    printf "<span class='%s'>%s</span>" cls .
    replace "\n" "<br/>" .
    replace useDashV "" .
    replace "Ghci" "IHaskell" .
    fixDollarSigns .
    rstrip .
    typeCleaner
  where
    fixDollarSigns = replace "$" "<span>$</span>"
    useDashV = "\nUse -v to see a list of the files searched for."
    isShowError err =
      startswith "No instance for (Show" err &&
      isInfixOf " arising from a use of `print'" err


formatParseError :: StringLoc -> String -> ErrMsg
formatParseError (Loc line col) =
  printf "Parse error (line %d, column %d): %s" line col

formatGetType :: String -> String
formatGetType = printf "<span class='get-type'>%s</span>"

formatType :: String -> Display
formatType typeStr =  Display [plain typeStr, html $ formatGetType typeStr]

displayError :: ErrMsg -> Display
displayError msg = Display [plain . typeCleaner $ msg, html $ formatError msg]

mono :: String -> String
mono = printf "<span class='mono'>%s</span>"
