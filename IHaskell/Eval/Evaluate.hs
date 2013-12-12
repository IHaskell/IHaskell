{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{- | Description : Wrapper around GHC API, exposing a single `evaluate` interface that runs
                   a statement, declaration, import, or directive.

This module exports all functions used for evaluation of IHaskell input.
-}
module IHaskell.Eval.Evaluate (
  interpret, evaluate, Interpreter, liftIO,
  typeCleaner
  ) where

import ClassyPrelude hiding (liftIO, hGetContents)
import Prelude(putChar, tail, init, (!!))
import Data.List.Utils
import Data.List(findIndex)
import Data.String.Utils
import Text.Printf
import Data.Char as Char

import Language.Haskell.Exts.Parser hiding (parseType)
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.Syntax hiding (Name)

import InteractiveEval
import HscTypes
import GhcMonad (liftIO)
import GHC hiding (Stmt, TypeSig)
import GHC.Paths
import Exception hiding (evaluate)
import Outputable
import Packages
import Module

import qualified System.IO.Strict as StrictIO

import IHaskell.Types
import IHaskell.Eval.Parser

data ErrorOccurred = Success | Failure

debug :: Bool
debug = True

ignoreTypePrefixes :: [String]
ignoreTypePrefixes = ["GHC.Types", "GHC.Base", "GHC.Show", "System.IO",
                      "GHC.Float", ":Interactive", "GHC.Num", "GHC.IO"]

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
  [ "import Prelude"
  , "import Control.Applicative ((<$>))"
  , "import GHC.IO.Handle (hDuplicateTo, hDuplicate)"
  , "import System.IO"
  ]

directiveChar :: Char
directiveChar = ':'

-- | Run an interpreting action. This is effectively runGhc with
-- initialization and importing.
interpret :: Interpreter a -> IO a
interpret action = runGhc (Just libdir) $ do
  -- Set the dynamic session flags
  dflags <- getSessionDynFlags
  void $ setSessionDynFlags $ dflags { hscTarget = HscInterpreted, ghcLink = LinkInMemory }

  -- Load packages that start with ihaskell-* and aren't just IHaskell.
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

  -- Import modules.
  imports <- mapM parseImportDecl $ globalImports ++ displayImports
  setContext $ map IIDecl imports

  -- Give a value for `it`. This is required due to the way we handle `it`
  -- in the wrapper statements - if it doesn't exist, the first statement
  -- will fail.
  runStmt "putStrLn \"\"" RunToCompletion

  -- Run the rest of the interpreter
  action

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
      output result
      case success of
        Success -> runUntilFailure rest
        Failure -> return ()

    storeItCommand execCount = Statement $ printf "let it%d = it" execCount

wrapExecution :: Interpreter [DisplayData] -> Interpreter (ErrorOccurred, [DisplayData])
wrapExecution exec = ghandle handler $ exec >>= \res ->
    return (Success, res)
  where 
    handler :: SomeException -> Interpreter (ErrorOccurred, [DisplayData])
    handler exception = return (Failure, [Display MimeHtml $ formatError $ show exception])

-- | Return the display data for this command, as well as whether it
-- resulted in an error.
evalCommand :: CodeBlock -> Interpreter (ErrorOccurred, [DisplayData])
evalCommand (Import importStr) = wrapExecution $ do
  write $ "Import: " ++ importStr
  importDecl <- parseImportDecl importStr
  context <- getContext
  setContext $ IIDecl importDecl : context
  return []

evalCommand (Directive GetType expr) = wrapExecution $ do
  result <- exprType expr
  flags <- getSessionDynFlags
  let typeStr = formatGetType $ showSDocUnqual flags $ ppr result
  return [Display MimeHtml typeStr]

evalCommand (Statement stmt) = do
  write $ "Statement: " ++ stmt
  ghandle handler $ do
    (printed, result) <- capturedStatement stmt
    case result of
      RunOk names -> do
        dflags <- getSessionDynFlags
        write $ "Names: " ++ show (map (showPpr dflags) names)  
        let output = [Display PlainText printed | not . null $ strip printed]
        return (Success, output)
      RunException exception -> do
        write $ "RunException: " ++ show exception
        return (Failure, [Display MimeHtml $ formatError $ show exception])
      RunBreak{} ->
        error "Should not break."
  where 
    handler :: SomeException -> Interpreter (ErrorOccurred, [DisplayData])
    handler exception = do
      write $ concat ["BreakCom: ", show exception, "\nfrom statement:\n", stmt]

      -- Close the file handle we opened for writing stdout and other cleanup.
      let (_, _, postStmts) = makeWrapperStmts
      forM_ postStmts $ \s -> runStmt s RunToCompletion

      return (Failure, [Display MimeHtml $ formatError $ show exception])

evalCommand (Expression expr) = evalCommand (Statement expr)

evalCommand (Declaration decl) = wrapExecution $ runDecls decl >> return []

evalCommand (ParseError loc err) = wrapExecution $
  return [Display MimeHtml $ formatParseError loc err]

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

parseStmts :: String -> Either (LineNumber, ColumnNumber, String) [Stmt]
parseStmts code = 
  case parseResult of
    ParseOk (Do stmts) -> Right stmts
    ParseOk _ -> Right []
    ParseFailed srcLoc errMsg -> Left (srcLine srcLoc, srcColumn srcLoc, errMsg) 
  where
    parseResult = parseExp doBlock
    doBlock = unlines $ ("do" : map indent (lines code)) ++ [indent returnStmt]
    indent = ("  " ++) 
    returnStmt = "return ()"

formatError :: ErrMsg -> String
formatError = printf "<span style='color: red; font-style: italic;'>%s</span>" .
            replace "\n" "<br/>" . 
            replace useDashV "" .
            typeCleaner
  where 
        useDashV = "\nUse -v to see a list of the files searched for."

formatParseError :: StringLoc -> String -> ErrMsg
formatParseError (Loc line col) msg = 
  formatError $ printf "Parse error (line %d, column %d): %s" line col msg

formatGetType :: String -> String
formatGetType = printf "<span style='font-weight: bold; color: green;'>%s</span>"
