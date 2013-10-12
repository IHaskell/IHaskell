-- | This module exports all functions used for evaluation of IHaskell input.
module IHaskell.Eval.Evaluate (
  evaluate, Interpreter, makeInterpreter
  ) where

import ClassyPrelude
import Prelude(putChar, tail, init)
import System.Process
import System.IO (hSetBuffering, BufferMode(..), hPutStr, hGetChar)
import Data.List.Utils
import Data.String.Utils
import Text.Printf

import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.Syntax

import IHaskell.Types

type LineNumber = Int
type ColumnNumber = Int

promptString :: String
promptString = "+++GHCI_IHASKELL+++>"

data Interpreter = Interpreter {
  inStream :: Handle,
  outStream :: Handle,
  errStream :: Handle,
  ghciHandle :: ProcessHandle
  }

makeInterpreter :: IO Interpreter
makeInterpreter = do
    let processSpec = (proc "ghci.sh" ["-ghci-script", "ihaskell.ghci"]) {
      std_in = CreatePipe,
      std_out = CreatePipe,
      std_err = CreatePipe
    }
    (Just input, Just output, Just errs, processHandle) <- createProcess processSpec

    hSetBuffering input  NoBuffering
    hSetBuffering output NoBuffering
    hSetBuffering errs   NoBuffering

    let interpreter = Interpreter {
      inStream = input,
      outStream = output,
      errStream = errs,
      ghciHandle = processHandle 
    }

    initializeInterpreter interpreter
    return interpreter

ghciSend :: Interpreter -> String -> IO ()
ghciSend interpreter = hPutStr (inStream interpreter)

ghciConsumePrompt :: Interpreter -> IO [String]
ghciConsumePrompt interpreter = readChars []
  where readChars prev =
          if startswith (reverse promptString) prev
          then case lines prev of
            _ : rest -> return $ reverse $ map reverse rest
            [] -> error "No prompt present."
          else do
            nextChar <- hGetChar (outStream interpreter)
            when (nextChar == '\n') $ print $ reverse prev
            readChars (nextChar : prev)

initializeInterpreter :: Interpreter -> IO ()
initializeInterpreter = void . ghciConsumePrompt

-- | Evaluate some IPython input code.
evaluate :: Interpreter      -- ^ Handle to the interpreter context.
         -> String           -- ^ Haskell code or other interpreter commands.
         -> IO [DisplayData] -- ^ All of the output.
evaluate interpreter code =
  case strip code of
    "" -> return []
    strippedCode ->
      let codePieces = map makeCodePiece $ groupBy sameCodePiece $ lines strippedCode in
        concat <$> mapM (evalCodePiece interpreter) codePieces

data CodePiece = GhciDirectives String | HaskellStmts String

makeCodePiece :: [String] -> CodePiece
makeCodePiece lines =
  if any isDirective lines
  then GhciDirectives $ unlines lines
  else HaskellStmts $ unlines lines

isDirective :: String -> Bool
isDirective line = 
  let stripped = strip line in
    startswith ":" stripped || startswith "import" stripped

sameCodePiece :: String -> String -> Bool
sameCodePiece = (==) `on` isDirective

evalCodePiece :: Interpreter -> CodePiece -> IO [DisplayData]
evalCodePiece interpreter (HaskellStmts code) =
  case parseStmts code of
    Left (errLine, errCol, errMsg) -> return [Display MimeHtml $ makeError $ printf "error Error (line %d, column %d): %s" errLine errCol errMsg]
    Right statements -> do
      mapM_ (putStrLn . pack) $ map prettyPrint statements
      concat <$> mapM (getResponse interpreter . prettyPrint) (init statements)

evalCodePiece interpreter (GhciDirectives directives) = do
    mapM_ (getResponse interpreter) $ lines directives
    return []

stripDoBlock :: String -> String
stripDoBlock =
  unlines . map dedent . init . lines
  where
    dedent (' ':' ':rest) = rest

getResponse :: Interpreter -> String -> IO [DisplayData]
getResponse interpreter code = do
  mapM_ (ghciSend interpreter . (++ "\n")) $ lines code
  inlines <- ghciConsumePrompt interpreter
  case inlines of
    [] -> return []
    _ -> return $ parseOutput $ unlines inlines 

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


parseOutput :: String -> [DisplayData]
parseOutput output
  | startswith "<interactive>" output = [Display MimeHtml $ makeError output]
  | otherwise = [Display PlainText output]

makeError :: String -> String
makeError output =
  let _ : rest = words output in
    printf "<span style='color: red; font-style: italic;'>%s</span>" $ unwords rest
