{-# LANGUAGE DoAndIfThenElse #-}
{- | Description : Wrapper around GHC API, exposing a single `evaluate` interface that runs
                   a statement, declaration, import, or directive.

This module exports all functions used for evaluation of IHaskell input.
-}
module IHaskell.Eval.Evaluate (
  interpret, evaluate, Interpreter, liftIO, typeCleaner, globalImports
  ) where

import ClassyPrelude hiding (liftIO, hGetContents)
import Prelude (putChar, head, tail, last, init, (!!))
import Data.List.Utils
import Data.List(findIndex)
import Data.String.Utils
import Text.Printf
import Data.Char as Char
import Data.Dynamic
import Data.Typeable
import qualified Data.Serialize as Serialize
import System.Directory (removeFile, createDirectoryIfMissing, removeDirectoryRecursive)

import NameSet
import DynFlags (defaultObjectTarget)
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

makeWrapperStmts :: (String, [String], [String])
makeWrapperStmts = (fileName, initStmts, postStmts)
  where
    randStr = "1345964344725219474" :: String
    fileVariable = "file_var_" ++ randStr
    oldVariable = fileVariable ++ "_old"
    itVariable = "it_var_" ++ randStr
    fileName = ".ihaskell_capture"

    initStmts :: [String]
    initStmts = [
      printf "let %s = it" itVariable,
      printf "%s <- openFile \"%s\" WriteMode" fileVariable fileName,
      printf "%s <- hDuplicate stdout" oldVariable,
      printf "hDuplicateTo %s stdout" fileVariable,
      printf "let it = %s" itVariable]

    postStmts :: [String]
    postStmts = [
      printf "let %s = it" itVariable,
      "hFlush stdout",
      printf "hDuplicateTo %s stdout" oldVariable,
      printf "hClose %s" fileVariable,
      printf "let it = %s" itVariable]

write :: GhcMonad m => String -> m ()
write x = when debug $ liftIO $ hPutStrLn stderr x

type Interpreter = Ghc

globalImports :: [String]
globalImports = 
  [ "import IHaskell.Display"
  , "import Control.Applicative ((<$>))"
  , "import GHC.IO.Handle (hDuplicateTo, hDuplicate)"
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
  imports <- mapM parseImportDecl $ globalImports ++ displayImports
  setContext $ map IIDecl $ implicitPrelude : imports

-- | Give a value for the `it` variable.
initializeItVariable :: Interpreter ()
initializeItVariable =
  -- This is required due to the way we handle `it` in the wrapper
  -- statements - if it doesn't exist, the first statement will fail.
  void $ runStmt "let it = ()" RunToCompletion

-- | Evaluate some IPython input code.
evaluate :: Int                                -- ^ The execution counter of this evaluation.
         -> String                             -- ^ Haskell code or other interpreter commands.
         -> ([DisplayData] -> Interpreter ())   -- ^ Function used to publish data outputs.
         -> Interpreter ()
evaluate execCount code output = do
  cmds <- parseString (strip code)
  runUntilFailure (cmds ++ [storeItCommand execCount])
  where
    runUntilFailure :: [CodeBlock] -> Interpreter ()
    runUntilFailure [] = return ()
    runUntilFailure (cmd:rest) = do 
      (success, result) <- evalCommand cmd
      unless (null result) $ output result
      case success of
        Success -> runUntilFailure rest
        Failure -> return ()

    storeItCommand execCount = Statement $ printf "let it%d = it" execCount

wrapExecution :: Interpreter [DisplayData] -> Interpreter (ErrorOccurred, [DisplayData])
wrapExecution exec = ghandle handler $ exec >>= \res ->
    return (Success, res)
  where 
    handler :: SomeException -> Interpreter (ErrorOccurred, [DisplayData])
    handler exception = return (Failure, displayError $ show exception)

-- | Return the display data for this command, as well as whether it
-- resulted in an error.
evalCommand :: CodeBlock -> Interpreter (ErrorOccurred, [DisplayData])
evalCommand (Import importStr) = wrapExecution $ do
  write $ "Import: " ++ importStr
  importDecl <- parseImportDecl importStr
  context <- getContext

  -- If we've imported this implicitly, remove the old import.
  let noImplicit = filter (not . implicitImportOf importDecl) context
  setContext $ IIDecl importDecl : noImplicit

  flags <- getSessionDynFlags
  return []
  where
    implicitImportOf :: ImportDecl RdrName -> InteractiveImport -> Bool
    implicitImportOf _ (IIModule _) = False
    implicitImportOf imp (IIDecl decl) = ideclImplicit decl && ((==) `on` (unLoc . ideclName)) decl imp

evalCommand (Module contents) = wrapExecution $ do
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

  -- Set to use object code for fast running times, as that is the only
  -- reason you would want to use modules in IHaskell.
  flags <- getSessionDynFlags
  let objTarget = defaultObjectTarget
  setSessionDynFlags flags{ hscTarget = objTarget }

  -- Remember which modules we've loaded before.
  importedModules <- getContext

  let -- Get the dot-delimited pieces of hte module name.
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
    Just previous -> 
      let prevLoaded = intercalate "." (moduleNameOf previous) in
        return $ displayError $
          printf "Can't load module %s because already loaded %s" modName prevLoaded

    -- Since nothing prevents loading the module, compile and load it.
    Nothing -> do
      -- Create a new target
      target <- guessTarget modName Nothing
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

evalCommand (Directive SetExtension exts) = wrapExecution $ do
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

evalCommand (Directive GetType expr) = wrapExecution $ do
  result <- exprType expr
  flags <- getSessionDynFlags
  let typeStr = showSDocUnqual flags $ ppr result
  return [plain typeStr, html $ formatGetType typeStr]

-- This is taken largely from GHCi's info section in InteractiveUI.
evalCommand (Directive HelpForSet _) = return (Success, [out])
  where out = plain $ intercalate "\n"
          [":set is not implemented in IHaskell."
          ,"  Use :extension <Extension> to enable a GHC extension."
          ,"  Use :extension No<Extension> to disable a GHC extension."
          ]

-- This is taken largely from GHCi's info section in InteractiveUI.
evalCommand (Directive GetHelp _) = return (Success, [out])
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
evalCommand (Directive GetInfo str) = wrapExecution $ do
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

evalCommand (Statement stmt) = do
  write $ "Statement: " ++ stmt
  ghandle handler $ do
    (printed, result) <- capturedStatement stmt
    case result of
      RunOk names -> do
        dflags <- getSessionDynFlags
        write $ "Names: " ++ show (map (showPpr dflags) names)  
        let output = [plain printed | not . null $ strip printed]
        return (Success, output)
      RunException exception -> do
        write $ "RunException: " ++ show exception
        return (Failure, displayError $ show exception)
      RunBreak{} ->
        error "Should not break."
  where 
    handler :: SomeException -> Interpreter (ErrorOccurred, [DisplayData])
    handler exception = do
      write $ concat ["BreakCom: ", show exception, "\nfrom statement:\n", stmt]

      -- Close the file handle we opened for writing stdout and other cleanup.
      let (_, _, postStmts) = makeWrapperStmts
      forM_ postStmts $ \s -> runStmt s RunToCompletion

      return (Failure, displayError $ show exception)

evalCommand (Expression expr) = do
  -- Evaluate this expression as though it's just a statement.
  -- The output is bound to 'it', so we can then use it.
  (success, out) <- evalCommand (Statement expr)

    -- Try to use `display` to convert our type into the output
  -- DisplayData. If typechecking fails and there is no appropriate
  -- typeclass, this will throw an exception and thus `attempt` will
  -- return False, and we just resort to plaintext.
  let displayExpr = printf "(IHaskell.Display.display (%s))" expr
  canRunDisplay <- attempt $ exprType displayExpr
  write displayExpr

  -- If evaluation failed, return the failure.  If it was successful, we
  -- may be able to use the IHaskellDisplay typeclass.
  if not canRunDisplay
  then return (success, out)
  else case success of
    Success -> useDisplay displayExpr
    Failure -> if isShowError out
              then useDisplay displayExpr
              else return (success, out)

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
        startswith "No instance for (GHC.Show.Show " msg &&
        isInfixOf " arising from a use of `System.IO.print'" msg
      Nothing -> False
      where isPlain (Display mime _) = (mime == PlainText)

    useDisplay displayExpr = wrapExecution $ do
      -- If there are instance matches, convert the object into
      -- a [DisplayData]. We also serialize it into a bytestring. We get
      -- the bytestring as a dynamic and then convert back to
      -- a bytestring, which we promptly unserialize. Note that
      -- attempting to do this without the serialization to binary and
      -- back gives very strange errors - all the types match but it
      -- refuses to decode back into a [DisplayData].
      runStmt displayExpr RunToCompletion
      displayedBytestring <- dynCompileExpr "IHaskell.Display.serializeDisplay it"
      case fromDynamic displayedBytestring of
        Nothing -> error "Expecting lazy Bytestring"
        Just bytestring ->
          case Serialize.decode bytestring of
            Left err -> error err
            Right displayData -> do
              write $ show displayData
              return displayData


evalCommand (Declaration decl) = wrapExecution $ runDecls decl >> return []

evalCommand (ParseError loc err) = wrapExecution $
  return $ displayError $ formatParseError loc err

capturedStatement :: String -> Interpreter (String, RunResult)
capturedStatement stmt = do
  -- Generate random variable names to use so that we cannot accidentally
  -- override the variables by using the right names in the terminal.
  let (fileName, initStmts, postStmts) = makeWrapperStmts
      goStmt s = runStmt s RunToCompletion

  forM_ initStmts goStmt
  result <- goStmt stmt
  forM_ postStmts goStmt

  -- We must use strict IO, because we write to that file again if we
  -- execute more statements. If we read lazily, we may cause errors when
  -- trying to open the file for writing later.
  printedOutput <- liftIO $ StrictIO.readFile fileName

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
displayError msg = [plain msg, html $ formatError msg] 
