{-# LANGUAGE DoAndIfThenElse #-}
{- | Description : Wrapper around GHC API, exposing a single `evaluate` interface that runs
                   a statement, declaration, import, or directive.

This module exports all functions used for evaluation of IHaskell input.
-}
module IHaskell.Eval.Evaluate (
  interpret, evaluate, Interpreter, liftIO, typeCleaner, globalImports
  ) where

import ClassyPrelude hiding (liftIO, hGetContents, try)
import Control.Concurrent (forkIO, threadDelay)
import Prelude (putChar, head, tail, last, init, (!!))
import Data.List.Utils
import Data.List(findIndex)
import Data.String.Utils
import Text.Printf
import Data.Char as Char
import Data.Dynamic
import Data.Typeable
import qualified Data.Serialize as Serialize
import System.Directory
import System.Posix.IO
import System.IO (hGetChar, hFlush)
import System.Random (getStdGen, randomRs)
import Unsafe.Coerce

import NameSet
import Name
import PprTyThing
import InteractiveEval
import DynFlags
import Type
import Exception (gtry)
import HscTypes
import HscMain
import TcType
import Unify
import InstEnv
import GhcMonad (liftIO, withSession)
import GHC hiding (Stmt, TypeSig)
import GHC.Paths
import Exception hiding (evaluate)
import Outputable
import Packages
import Module

import qualified System.IO.Strict as StrictIO

import IHaskell.Types
import IHaskell.Eval.Parser
import IHaskell.Eval.Lint
import IHaskell.Display

data ErrorOccurred = Success | Failure deriving Show

debug :: Bool
debug = False

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

globalImports :: [String]
globalImports = 
  [ "import IHaskell.Display"
  , "import Control.Applicative ((<$>))"
  , "import GHC.IO.Handle (hDuplicateTo, hDuplicate)"
  , "import System.Posix.IO"
  , "import System.Posix.Files"
  , "import System.IO"
  ]

-- | Run an interpreting action. This is effectively runGhc with
-- initialization and importing.
interpret :: Interpreter a -> IO a
interpret action = runGhc (Just libdir) $ do
  -- Set the dynamic session flags
  originalFlags <- getSessionDynFlags
  let dflags = xopt_set originalFlags Opt_ExtendedDefaultRules
  void $ setSessionDynFlags $ dflags { hscTarget = HscInterpreted, ghcLink = LinkInMemory }

  initializeImports
  initializeItVariable

  -- Run the rest of the interpreter
  action

-- | Initialize our GHC session with imports and a value for 'it'.
initializeImports :: Interpreter ()
initializeImports = do
  -- Load packages that start with ihaskell-* and aren't just IHaskell.
  dflags <- getSessionDynFlags
  displayPackages <- liftIO $ do
    (dflags, _) <- initPackages dflags
    let Just db = pkgDatabase dflags
        packageNames = map (packageIdString . packageConfigId) db
        initStr = "ihaskell-"
        ihaskellPkgs = filter (startswith initStr) packageNames
        displayPkgs = filter (isAlpha . (!! (length initStr + 1))) ihaskellPkgs
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
initializeItVariable =
  -- This is required due to the way we handle `it` in the wrapper
  -- statements - if it doesn't exist, the first statement will fail.
  void $ runStmt "let it = ()" RunToCompletion

-- | Publisher for IHaskell outputs.  The first argument indicates whether
-- this output is final (true) or intermediate (false).
type Publisher = (Bool -> [DisplayData] -> IO ())

-- | Output of a command evaluation.
data EvalOut = EvalOut {
    evalStatus :: ErrorOccurred,
    evalResult :: [DisplayData],
    evalState :: KernelState
  }

-- | Evaluate some IPython input code.
evaluate :: KernelState      -- ^ The kernel state.
         -> String           -- ^ Haskell code or other interpreter commands.
         -> Publisher        -- ^ Function used to publish data outputs.
         -> Interpreter KernelState
evaluate kernelState code output = do
  cmds <- parseString (strip code)
  let execCount = getExecutionCounter kernelState

  when (getLintStatus kernelState /= LintOff) $ liftIO $ do
    lintSuggestions <- lint cmds
    unless (null lintSuggestions) $
      output True lintSuggestions

  updated <- runUntilFailure kernelState (map unloc cmds ++ [storeItCommand execCount])
  return updated {
    getExecutionCounter = execCount + 1
  }
  where
    runUntilFailure :: KernelState -> [CodeBlock] -> Interpreter KernelState
    runUntilFailure state [] = return state
    runUntilFailure state (cmd:rest) = do 
      evalOut <- evalCommand output cmd state

      -- Output things only if they are non-empty.
      let result = evalResult evalOut
      unless (null result) $
        liftIO $ output True result

      let newState = evalState evalOut
      case evalStatus evalOut of
        Success -> runUntilFailure newState rest
        Failure -> return newState

    storeItCommand execCount = Statement $ printf "let it%d = it" execCount

wrapExecution :: KernelState
              -> Interpreter [DisplayData]
              -> Interpreter EvalOut
wrapExecution state exec = ghandle handler $ exec >>= \res ->
    return EvalOut {
      evalStatus = Success,
      evalResult = res,
      evalState = state
    }
  where 
    handler :: SomeException -> Interpreter EvalOut
    handler exception =
      return EvalOut {
        evalStatus = Failure,
        evalResult = displayError $ show exception,
        evalState = state
      }

-- | Return the display data for this command, as well as whether it
-- resulted in an error.
evalCommand :: Publisher -> CodeBlock -> KernelState -> Interpreter EvalOut
evalCommand _ (Import importStr) state = wrapExecution state $ do
  write $ "Import: " ++ importStr
  importDecl <- parseImportDecl importStr
  context <- getContext

  -- If we've imported this implicitly, remove the old import.
  let noImplicit = filter (not . implicitImportOf importDecl) context
  setContext $ IIDecl importDecl : noImplicit

  flags <- getSessionDynFlags
  return $ if "Test.Hspec" `isInfixOf` importStr
           then displayError $ "Warning: Hspec is unusable in IHaskell until the resolution of GHC bug #8639." ++
                                "\nThe variable `it` is shadowed and cannot be accessed, even in qualified form."
           else []
  where
    implicitImportOf :: ImportDecl RdrName -> InteractiveImport -> Bool
    implicitImportOf _ (IIModule _) = False
    implicitImportOf imp (IIDecl decl) = ideclImplicit decl && ((==) `on` (unLoc . ideclName)) decl imp

evalCommand _ (Module contents) state = wrapExecution state $ do
  write $ "Module:\n" ++ contents

  -- Write the module contents to a temporary file in our work directory
  namePieces <- getModuleName contents
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

evalCommand _ (Directive SetExtension exts) state = wrapExecution state $ do
  write $ "Extension: " ++ exts
  results <- mapM setExtension (words exts)
  case catMaybes results of
    [] -> return []
    errors -> return $ displayError $ intercalate "\n" errors
  where
    -- Set an extension and update flags.
    -- Return Nothing on success. On failure, return an error message.
    setExtension :: String -> Interpreter (Maybe ErrMsg)
    setExtension ext = do
      flags <- getSessionDynFlags
      -- First, try to check if this flag matches any extension name.
      let newFlags =
            case find (flagMatches ext) xFlags of
              Just (_, flag, _) -> Just $ xopt_set flags flag
              -- If it doesn't match an extension name, try matching against
              -- disabling an extension.
              Nothing -> 
                case find (flagMatchesNo ext) xFlags of
                  Just (_, flag, _) -> Just $ xopt_unset flags flag
                  Nothing -> Nothing

      -- Set the flag if we need to.
      case newFlags of
        Just flags -> setSessionDynFlags flags >> return Nothing
        Nothing -> return $ Just $ "Could not parse extension name: " ++ ext

    -- Check if a FlagSpec matches an extension name.
    flagMatches ext (name, _, _) = ext == name

    -- Check if a FlagSpec matches "No<ExtensionName>".
    -- In that case, we disable the extension.
    flagMatchesNo ext (name, _, _) = ext == "No"  ++ name

evalCommand _ (Directive SetLint status) state = do
  let isOn = "on" == strip status
  let isOff = "off" == strip status
  return $ if isOn
           then EvalOut Success [] (state { getLintStatus = LintOn })
           else if isOff
             then EvalOut Success [] (state { getLintStatus = LintOff })
             else EvalOut Failure err state
  where
    err = displayError $ "Unknown hlint command: " ++ status

evalCommand _ (Directive GetType expr) state = wrapExecution state $ do
  write $ "Type: " ++ expr
  result <- exprType expr
  flags <- getSessionDynFlags
  let typeStr = showSDocUnqual flags $ ppr result
  return [plain typeStr, html $ formatGetType typeStr]

evalCommand _ (Directive LoadFile name) state = wrapExecution state $ do
  write $ "Load: " ++ name

  let filename = if endswith ".hs" name
                 then name
                 else name ++ ".hs"
  let modName =  replace "/" "."  $
                   if endswith ".hs" name
                   then replace ".hs" "" name
                   else name

  doLoadModule filename modName



-- This is taken largely from GHCi's info section in InteractiveUI.
evalCommand _ (Directive HelpForSet _) state = do
  write "Help for :set."
  return EvalOut {
    evalStatus = Success,
    evalResult = [out],
    evalState = state
  }
  where out = plain $ intercalate "\n"
          [":set is not implemented in IHaskell."
          ,"  Use :extension <Extension> to enable a GHC extension."
          ,"  Use :extension No<Extension> to disable a GHC extension."
          ]

-- This is taken largely from GHCi's info section in InteractiveUI.
evalCommand _ (Directive GetHelp _) state = do
  write "Help via :help or :?."
  return EvalOut {
    evalStatus = Success,
    evalResult = [out],
    evalState = state
  }
  where out = plain $ intercalate "\n"
          ["The following commands are available:"
          ,"    :extension <Extension>    -  enable a GHC extension."
          ,"    :extension No<Extension>  -  disable a GHC extension."
          ,"    :type <expression>        -  Print expression type."
          ,"    :info <name>              -  Print all info for a name."
          ,"    :?, :help                 -  Show this help text."
          ,""
          ,"Any prefix of the commands will also suffice, e.g. use :ty for :type."
          ]

-- This is taken largely from GHCi's info section in InteractiveUI.
evalCommand _ (Directive GetInfo str) state = wrapExecution state $ do
  write $ "Info: " ++ str
  -- Get all the info for all the names we're given.
  names     <- parseName str
  maybeInfos <- mapM getInfo names

  -- Filter out types that have parents in the same set.
  -- GHCi also does this.
  let getType (theType, _, _) = theType
      infos = catMaybes maybeInfos
      allNames = mkNameSet $ map (getName . getType) infos
      hasParent info = case tyThingParent_maybe (getType info) of
        Just parent -> getName parent `elemNameSet` allNames
        Nothing -> False
      filteredOutput = filter (not . hasParent) infos

  -- Convert to textual data.
  let printInfo (thing, fixity, classInstances) = 
        pprTyThingInContextLoc False thing $$ showFixity fixity $$ vcat (map GHC.pprInstance classInstances)
        where
          showFixity fixity =
            if fixity == GHC.defaultFixity
            then empty
            else ppr fixity <+> pprInfixName (getName thing)
      outs = map printInfo filteredOutput

  -- Print nicely.
  unqual <- getPrintUnqual
  flags <- getSessionDynFlags
  let strings = map (showSDocForUser flags unqual) outs
  return [plain $ intercalate "\n" strings]

evalCommand output (Statement stmt) state = wrapExecution state $ do
  write $ "Statement:\n" ++ stmt
  let outputter str = output False [plain str]
  (printed, result) <- capturedStatement outputter stmt
  case result of
    RunOk names -> do
      dflags <- getSessionDynFlags
      write $ "Names: " ++ show (map (showPpr dflags) names)  
      let output = [plain printed | not . null $ strip printed]
      return output
    RunException exception -> throw exception
    RunBreak{} -> error "Should not break."

evalCommand output (Expression expr) state = do
  write $ "Expression:\n" ++ expr
  -- Evaluate this expression as though it's just a statement.
  -- The output is bound to 'it', so we can then use it.
  evalOut <- evalCommand output (Statement expr) state

    -- Try to use `display` to convert our type into the output
  -- DisplayData. If typechecking fails and there is no appropriate
  -- typeclass, this will throw an exception and thus `attempt` will
  -- return False, and we just resort to plaintext.
  let displayExpr = printf "(IHaskell.Display.display (%s))" expr
  canRunDisplay <- attempt $ exprType displayExpr
  let out = evalResult evalOut
  write $ printf "%s: Attempting %s" (if canRunDisplay then "Success" else "Failure") displayExpr
  write $ "Show Error: " ++ show (isShowError out)
  write $ show out

  -- If evaluation failed, return the failure.  If it was successful, we
  -- may be able to use the IHaskellDisplay typeclass.
  if not canRunDisplay
  then return evalOut
  else case evalStatus evalOut of
    Success -> useDisplay displayExpr
    Failure -> if isShowError out
              then useDisplay displayExpr
              else return evalOut

  where
    -- Try to evaluate an action. Return True if it succeeds and False if
    -- it throws an exception. The result of the action is discarded.
    attempt :: Interpreter a -> Interpreter Bool
    attempt action = gcatch (action >> return True) failure
      where failure :: SomeException -> Interpreter Bool
            failure _ = return False

    -- Check if the error is due to trying to print something that doesn't
    -- implement the Show typeclass.
    isShowError errs = case find isPlain errs of
      Just (Display PlainText msg) -> 
        -- Note that we rely on this error message being 'type cleaned', so
        -- that `Show` is not displayed as GHC.Show.Show.
        startswith "No instance for (Show" msg &&
        isInfixOf " arising from a use of `print'" msg
      Nothing -> False
      where isPlain (Display mime _) = mime == PlainText

    useDisplay displayExpr = wrapExecution state $ do
      -- If there are instance matches, convert the object into
      -- a [DisplayData]. We also serialize it into a bytestring. We get
      -- the bytestring as a dynamic and then convert back to
      -- a bytestring, which we promptly unserialize. Note that
      -- attempting to do this without the serialization to binary and
      -- back gives very strange errors - all the types match but it
      -- refuses to decode back into a [DisplayData].
      -- Suppress output, so as not to mess up console.
      capturedStatement (const $ return ()) displayExpr

      displayedBytestring <- dynCompileExpr "IHaskell.Display.serializeDisplay it"
      case fromDynamic displayedBytestring of
        Nothing -> error "Expecting lazy Bytestring"
        Just bytestring ->
          case Serialize.decode bytestring of
            Left err -> error err
            Right displayData -> do
              write $ show displayData
              return displayData


evalCommand _ (Declaration decl) state = wrapExecution state $ do
  write $ "Declaration:\n" ++ decl
  runDecls decl

  -- Do not display any output
  return []

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
    evalState = state
  }

doLoadModule :: String -> String -> Ghc [DisplayData]
doLoadModule name modName = flip gcatch unload $ do
  -- Compile loaded modules. 
  flags <- getSessionDynFlags
  let objTarget = defaultObjectTarget
  setSessionDynFlags flags{ hscTarget = objTarget }

  -- Remember which modules we've loaded before.
  importedModules <- getContext

  -- Create a new target
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
    Succeeded -> return []
    Failed -> return $ displayError $ "Failed to load module " ++ modName
  where
    unload :: SomeException -> Ghc [DisplayData]
    unload exception = do
      -- Explicitly clear targets
      setTargets []
      load LoadAllTargets

      initializeItVariable
      return $ displayError $ "Failed to load module " ++ modName ++ ": " ++ show exception

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

    voidpf str = printf $ str ++ " >> return ()"
    
    -- Statements run before the thing we're evaluating.
    initStmts = 
      [ printf "let %s = it" itVariable
      , printf "(%s, %s) <- createPipe" readVariable writeVariable
      , printf "%s <- dup stdOutput" oldVariable
      , voidpf "dupTo %s stdOutput" writeVariable
      , voidpf "hSetBuffering stdout NoBuffering"
      , printf "let it = %s" itVariable
      ]
    
    -- Statements run after evaluation.
    postStmts = 
      [ printf "let %s = it" itVariable
      , voidpf "hFlush stdout"
      , voidpf "dupTo %s stdOutput" oldVariable
      , voidpf "closeFd %s" writeVariable
      , printf "let it = %s" itVariable
      ]
    pipeExpr = printf "let %s = %s" (var "pipe_var_") readVariable

    goStmt s = runStmt s RunToCompletion

  -- Initialize evaluation context.
  forM_ initStmts goStmt

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
    forM_ postStmts goStmt

    -- Once context is finalized, reading can finish.
    -- Wait for reading to finish to that the output accumulator is
    -- completely filled.
    liftIO $ takeMVar finishedReading
    
  printedOutput <- liftIO $ readMVar outputAccum
  return (printedOutput, result)

formatError :: ErrMsg -> String
formatError = printf "<span style='color: red; font-style: italic;'>%s</span>" .
            replace "\n" "<br/>" . 
            replace useDashV "" .
            rstrip . 
            typeCleaner
  where 
        useDashV = "\nUse -v to see a list of the files searched for."

formatParseError :: StringLoc -> String -> ErrMsg
formatParseError (Loc line col) = 
  printf "Parse error (line %d, column %d): %s" line col

formatGetType :: String -> String
formatGetType = printf "<span style='font-weight: bold; color: green;'>%s</span>"

displayError :: ErrMsg -> [DisplayData]
displayError msg = [plain . typeCleaner $ msg, html $ formatError msg] 
