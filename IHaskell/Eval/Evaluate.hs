-- | This module exports all functions used for evaluation of IHaskell input.
module IHaskell.Eval.Evaluate (
  interpret, evaluate, Interpreter, liftIO
  ) where

import ClassyPrelude hiding (liftIO, hGetContents)
import Prelude(putChar, tail, init)
import Data.List.Utils
import Data.String.Utils
import Text.Printf

import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.Syntax hiding (Name)

import InteractiveEval
import HscTypes
import Name
import GhcMonad (liftIO)
import GHC hiding (Stmt)
import GHC.Paths
import Exception hiding (evaluate)

import IHaskell.Types

write :: GhcMonad m => String -> m ()
write x = liftIO $ hPutStrLn stderr x

type LineNumber = Int
type ColumnNumber = Int
type Interpreter = Ghc
data Command
  = Directive String
  | Import String
  | Statement String
  | ParseError LineNumber ColumnNumber String
  deriving Show

globalImports :: [String]
globalImports = 
  [ "import Prelude"
  , "import Control.Applicative ((<$>))"
  , "import GHC.IO.Handle (hDuplicateTo, hDuplicate)"
  , "import System.IO"
  ]

directiveChar :: Char
directiveChar = ':'

interpret :: Interpreter a -> IO a
interpret action = runGhc (Just libdir) $ do
  -- Set the dynamic session flags
  dflags <- getSessionDynFlags
  setSessionDynFlags $ dflags { hscTarget = HscInterpreted, ghcLink = LinkInMemory }

  -- Import modules.
  imports <- mapM parseImportDecl globalImports
  setContext $ map IIDecl imports

  -- Run the rest of the interpreter
  action

-- | Evaluate some IPython input code.
evaluate :: String                    -- ^ Haskell code or other interpreter commands.
         -> Interpreter [DisplayData] -- ^ All of the output.
evaluate code
  | strip code == "" = return [] 
  | otherwise = joinDisplays <$> mapM evalCommand (parseCommands $ strip code)

joinDisplays :: [[DisplayData]] -> [DisplayData]
joinDisplays displays = 
  let isPlain (Display mime _) = (mime == PlainText)
      allDisplays = concat displays
      plains = filter isPlain allDisplays
      other = filter (not . isPlain) allDisplays 
      getText (Display PlainText text) = text
      joinedPlains = Display PlainText $ concat $ map getText plains in
    joinedPlains : other

parseCommands :: String     -- ^ Code containing commands.
              -> [Command]  -- ^ Commands contained in code string.
parseCommands code = concatMap makeCommands pieces
  where
    pieces = groupBy ((==) `on` isDirective) $ lines code
    makeCommands lines
      | any isDirective lines = map createDirective lines
      | otherwise = 
          case parseStmts $ unlines lines of
            Left (srcLine, srcColumn, errMsg) -> [ParseError srcLine srcColumn errMsg]
            Right stmts -> map (Statement . prettyPrint) $ init stmts
    isDirective line = startswith [directiveChar] stripped || startswith "import" stripped
      where stripped = strip line
    createDirective line =
      case strip line of
        ':':_ -> Directive $ strip line
        _ -> Import $ strip line

evalCommand :: Command -> Interpreter [DisplayData]
evalCommand (Import importStr) = do
  write $ "Import: " ++ importStr
  importDecl <- parseImportDecl importStr
  context <- getContext
  setContext $ IIDecl importDecl : context
  return []

evalCommand (Directive directive) = do
  write $ "Directive: " ++ directive
  return [Display MimeHtml $ printf "<span style='font-weight: bold; color: green;'>%s</span>" directive]

evalCommand (Statement stmt) = do
  write $ "Statement: " ++ stmt
  ghandle handler $ do

    (printed, result) <- capturedStatement stmt
    case result of
      RunOk names -> --concat <$> mapM showName names
        return [Display PlainText printed]
      RunException exception -> do
        write $ "RunException: " ++ show exception
        return [Display MimeHtml $ makeError $ show exception]
      RunBreak{} ->
        error "Should not break."
  where 
    handler :: SomeException -> Interpreter [DisplayData]
    handler exception = do
      write $ concat ["Break: ", show exception, "\nfrom statement:\n", stmt]
      return [Display MimeHtml $ makeError $ show exception]

evalCommand (ParseError line col err) =
  return [Display MimeHtml $ makeError $ printf "error Error (line %d, column %d): %s" line col err]

capturedStatement :: String -> Interpreter (String, RunResult)
capturedStatement stmt = 
  let fileVariable = "ridiculous" :: String
      fileName = ".capture" :: String
      oldVariable = fileVariable ++ "'" :: String
      initStmts :: [String]
      initStmts = [
        printf "%s <- openFile \"%s\" WriteMode" fileVariable fileName,
        printf "%s <- hDuplicate stdout" oldVariable,
        printf "hDuplicateTo %s stdout" fileVariable]
      postStmts :: [String]
      postStmts = [
        "hFlush stdout",
        printf "hDuplicateTo %s stdout" oldVariable,
        printf "hClose %s" fileVariable]
      goStmt s = runStmt s RunToCompletion in do

    forM_ initStmts goStmt
    result <- goStmt stmt
    forM_ postStmts goStmt

    printedOutput <- liftIO $ readFile $ fpFromString fileName
    liftIO $ print printedOutput

    return (printedOutput, result)

showName :: Name -> Interpreter [DisplayData]
showName _ =
  return [Display PlainText "Hello!"]

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

makeError :: String -> String
makeError = printf "<span style='color: red; font-style: italic;'>%s</span>"
