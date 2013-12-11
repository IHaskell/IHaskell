{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module IHaskell.Eval.Parser (
    parseCell,
    CodeBlock(..),
    DirectiveType(..),
    LineNumber,
    ColumnNumber,
    splitAtLoc
    ) where

import ClassyPrelude hiding (liftIO)

import Data.String.Utils (startswith, strip)
import Prelude (init, last, head)

import FastString
import StringBuffer
import ErrUtils
import SrcLoc
import GHC
import Bag
import Outputable hiding ((<>))
import Lexer
import OrdList
import Data.List (findIndex, maximumBy, maximum)

import IHaskell.GHC.HaskellParser

type LineNumber = Int
type ColumnNumber = Int

data CodeBlock
  = Expression String
  | Declaration String
  | Statement String
  | Import String
  | TypeSignature String
  | Directive DirectiveType String
  | ParseError LineNumber ColumnNumber String
  deriving Show

data DirectiveType
  = GetType
  | GetInfo
  deriving Show

-- $setup
-- >>> import GHC
-- >>> import GHC.Paths
-- >>> import IHaskell.Eval.Parser
-- >>> let ghc = runGhc (Just libdir)
-- >>> let test = ghc . parseCell

-- $extendedParserTests
--
-- >>> test "3\nlet x = expr"
-- [Expression "3",Statement "let x = expr"]
--
-- >>> test "let x = 3 in x + 3"
-- [Expression "let x = 3 in x + 3"]
--
-- >>> test "data X = Y Int"
-- [Declaration "data X = Y Int"]
--
-- >>> test "3\n:t expr"
-- [Expression "3",Directive GetType "expr"]
--
-- >>> test "y <- print 'no'"
-- [Statement "y <- print 'no'"]
--
-- >>> test "y <- do print 'no'\nlet x = expr"
-- [Statement "y <- do { print 'no' }",Statement "let x = expr"]
--
-- >>> test "y <- do print 'no'\nlet x = expr\nexpression"
-- [Statement "y <- do { print 'no' }",Statement "let x = expr",Expression "expression"]
--
-- >>> test "print yes\nprint no"
-- [Expression "print yes",Expression "print no"]
--
-- >>> test "fun [] = 10"
-- [Declaration "fun [] = 10"]
--
-- >>> test "fun [] = 10\nprint 'h'"
-- [Declaration "fun [] = 10",Expression "print 'h'"]
--
-- >>> test "fun (x:xs) = 100"
-- [Declaration "fun (x : xs) = 100"]
--
-- >>> test "fun [] = 10\nfun (x:xs) = 100"
-- [Declaration "fun [] = 10\nfun (x : xs) = 100"]
--
-- >>> test "fun :: [a] -> Int\nfun [] = 10\nfun (x:xs) = 100"
-- [Declaration "fun :: [a] -> Int\nfun [] = 10\nfun (x : xs) = 100"]
--
-- >>> test "let x = 3 in"
-- [ParseError 2 1 "parse error (possibly incorrect indentation or mismatched brackets)\n"]
--
-- >>> test "data X = 3"
-- [ParseError 1 10 "Illegal literal in type (use -XDataKinds to enable): 3\n"]


-- | Parse a single cell into code blocks.
--
-- >>> test "let x = 3"
-- [Statement "let x = 3"]
--
-- >>> test ":type hello\n:in goodbye"
-- [Directive GetType "hello",Directive GetInfo "goodbye"]
--
-- >>> test "import Data.Monoid"
-- [Import "import Data.Monoid"]
--
-- >>> test "3 + 5"
-- [Expression "3 + 5"]
parseCell :: GhcMonad m => String -> m [CodeBlock]
parseCell codeString = concat <$> processChunks 1 [] chunks
  where
    chunks = splitOnDirectives [] $ lines codeString
    parseChunk chunk line =
      if isDirective chunk
      then return [parseDirective chunk line]
      else parseCell' chunk line

    isDirective = startswith ":" . strip

    processChunks _ results [] = return $ reverse results
    processChunks line accum (chunk:remaining) = do
      block <- parseChunk chunk line
      processChunks (line + nlines chunk) (block : accum) remaining

    splitOnDirectives results [] = reverse results
    splitOnDirectives chunks (line:lines) =
      if startswith ":" $ strip line
      then splitOnDirectives (line : chunks) lines
      else
        let goodLines = takeWhile (not . startswith ":" . strip) (line:lines)
            remaining = drop (length goodLines) (line:lines) in
          splitOnDirectives (unlines goodLines : chunks) remaining

    nlines = length . lines

joinFunctions :: [CodeBlock] -> [CodeBlock]
joinFunctions (Declaration decl : rest) =
    let (decls, other) = havingSameName rest in
      Declaration (joinLines $ map undecl decls) : joinFunctions other
  where
    undecl (Declaration decl) = decl
    undecl _ = error "Expected declaration!"

    havingSameName :: [CodeBlock] -> ([CodeBlock], [CodeBlock]) 
    havingSameName blocks =
      let name = head $ words decl
          sameName = takeWhile (isNamedDecl name) rest 
          others = drop (length sameName) rest in
        (Declaration decl : sameName, others)

    isNamedDecl :: String -> CodeBlock -> Bool
    isNamedDecl name (Declaration dec) = head (words dec) == name
    isNamedDecl _ _ = False
joinFunctions (TypeSignature sig : Declaration decl : rest) = (Declaration $ sig ++ "\n" ++ joinedDecl):remaining
  where Declaration joinedDecl:remaining = joinFunctions $ Declaration decl : rest 
        
joinFunctions (x:xs) = x : joinFunctions xs
joinFunctions [] = []

parseCell' :: GhcMonad m => String -> Int  -> m [CodeBlock]
parseCell' code startLine = do
    flags <- getSessionDynFlags
    let chunks = separateByIndent code
    return $ joinFunctions $ concatMap (parseBlock flags) chunks
    --let parseResults = map (stmtToExprs flags . tryParser) (parsers flags)
    --case rights parseResults of
    --  [] -> 
    --    let errors = lefts parseResults
    --        longestParseError = maximumBy (compare `on` snd) errors in
    --      return [fst longestParseError]
    --  (result, used, remaining):_ -> do
    --    remainResult <- parseCell' remaining $ startLine + length (lines used)
    --    return $ result ++ if null (strip remaining)
    --                      then []
    --                      else remainResult

  where
    parseBlock :: DynFlags -> String -> [CodeBlock]
    parseBlock flags string = 
      let results = map (stmtToExprs flags . tryParser string) (parsers flags) in
      case rights results of
        [] -> [bestError $ lefts results]
        (result, used, remaining):_ -> 
          if not . null . strip $ remaining
          then error $ "Failed to fully parse " ++ string
          else result

    bestError :: [(CodeBlock, Int)] -> CodeBlock
    bestError errors =
      let longestParseLength = maximum $ map snd errors in
        fst $ if all ((== longestParseLength) . snd) errors
              then maximumBy (locCompare `on` fst) errors
              else maximumBy (compare `on` snd) errors
      where
        locCompare (ParseError line1 col1 _) (ParseError line2 col2 _) = compare line1 line2 <> compare col1 col2
        locCompare _ _ = EQ

    -- Attempt to convert a statement to an expression
    stmtToExprs :: DynFlags -> Either (CodeBlock, Int) (CodeBlock, String, String) -> Either (CodeBlock, Int) ([CodeBlock], String, String)
    stmtToExprs flags (Right (Statement string, used, remaining)) = Right (blocks, used, remaining)
      where blocks = if isExpr flags string
                     then parseExpressions used
                     else [Statement string]
    stmtToExprs _ (Left err) = Left err
    stmtToExprs _ (Right (block, used, remaining)) = Right ([block], used, remaining)

    -- Check whether a string is a valid expression.
    isExpr :: DynFlags -> String -> Bool
    isExpr flags str = case runParser flags fullExpression str of
      Left _ -> False
      Right _ -> True

    parseExpressions :: String -> [CodeBlock]
    parseExpressions string = map Expression $ filter (not . null) $ map strip $ separateByIndent string

    separateByIndent string = 
      let (first, rest) = splitByIndent (lines string) in
        first : if null rest
                then []
                else separateByIndent (unlines rest)

    splitByIndent :: [String] -> (String, [String])
    splitByIndent (first:rest) = (unlines $ first:take endOfBlock rest, drop endOfBlock rest)
      where
        endOfBlock = fromMaybe (length rest) $ findIndex (\x -> indentLevel x <= indentLevel first) rest
    splitByIndent [] = ("", [])

    indentLevel (' ':str) = 1 + indentLevel str
    indentLevel _ = 0 :: Int

    tryParser :: String -> (String -> CodeBlock, String -> (Either (String, LineNumber, ColumnNumber) String, String, String)) -> Either (CodeBlock, Int) (CodeBlock, String, String)
    tryParser string (blockType, parser) = case parser string of
      (Left (err, line, col), used, _) -> Left (ParseError line col err, length used)
      (Right res, used, remaining) -> Right (blockType res, used, remaining)
    parsers flags =
      [ (Import,      strParser flags partialImport)
      , (TypeSignature, strParser flags partialTypeSignature)
      , (Declaration, lstParser flags partialDeclaration)
      , (Statement,   strParser flags partialStatement)
      ]

    lstParser :: Outputable a => DynFlags -> P (OrdList a) -> String -> (Either (String, LineNumber, ColumnNumber) String, String, String)
    lstParser flags parser code =
      case runParser flags parser code of
        Left err -> (Left err, code, "")
        Right (out, used, remainingCode) -> (Right . strip . unlines $ map (showSDoc flags . ppr) (fromOL out), used, remainingCode)

    strParser :: Outputable a => DynFlags -> P a -> String -> (Either (String, LineNumber, ColumnNumber) String, String, String)
    strParser flags parser code =
      case runParser flags parser code of
        Left err -> (Left err, code, "")
        Right (out, used, remainingCode) -> (Right . showSDoc flags . ppr $ out, used, remainingCode)

-- | Parse a directive of the form :directiveName.
parseDirective :: String       -- ^ Directive string.
               -> Int          -- ^ Line number at which the directive appears.
               -> CodeBlock    -- ^ Directive code block or a parse error.
parseDirective (':':directive) line = case find rightDirective directives of
  Just (directiveType, _) -> Directive directiveType arg
    where arg = unwords restLine
          _:restLine = words directive
  Nothing -> ParseError line 0 $ "Unknown command: '" ++ directive ++ "'."
  where
    rightDirective (_, strings) = case words directive of
      [] -> False
      dir:_ -> dir `elem` strings
    directives =
      [(GetType, ["t", "ty", "typ", "type"])
      ,(GetInfo, ["i", "in", "inf", "info"])
      ]
parseDirective _ _ = error "Directive must start with colon!"

-- | Run a GHC parser on a string.
runParser :: DynFlags -> P a -> String -> Either (String, LineNumber, ColumnNumber) (a, String, String)
runParser dflags parser str = toEither (unP parser (mkPState dflags buffer location))
    where
       filename = "<interactive>"
       location = mkRealSrcLoc (mkFastString filename) 1 1
       buffer = stringToStringBuffer str

       toEither (PFailed span@(RealSrcSpan realSpan) err) = 
         let errMsg = printErrorBag $ unitBag $ mkPlainErrMsg dflags span err
             line = srcLocLine $ realSrcSpanStart realSpan
             col = srcLocCol $ realSrcSpanStart realSpan
           in Left (errMsg, line, col)

       toEither (PFailed span err) = 
         let errMsg = printErrorBag $ unitBag $ mkPlainErrMsg dflags span err
           in Left (errMsg, 0, 0)

       toEither (POk parseState result) = 
         let parseEnd = realSrcSpanStart $ last_loc parseState
             endLine = srcLocLine parseEnd
             endCol = srcLocCol parseEnd
             (before, after) = splitAtLoc endLine endCol str in
           Right (result, before, after)

       -- Convert the bag of errors into an error string.
       printErrorBag bag = unlines . map show $ bagToList bag

-- | Split a string at a given line and column. The column is included in
-- the second part of the split.
--
-- >>> splitAtLoc 2 3 "abc\ndefghi\nxyz\n123"
-- ("abc\nde","fghi\nxyz\n123")
--
-- >>> splitAtLoc 2 1 "abc"
-- ("abc","")
--
-- >>> splitAtLoc 2 1 "abc\nhello"
-- ("abc\n","hello")
splitAtLoc :: LineNumber -> ColumnNumber -> String -> (String, String)
splitAtLoc line col string = 
  if line > length (lines string)
  then (string, "")
  else (before, after)
  where
    (beforeLines, afterLines) = splitAt line $ lines string
    theLine = last beforeLines
    (beforeChars, afterChars) = splitAt (col - 1) theLine

    before = joinLines (init beforeLines) ++ '\n' : beforeChars
    after = joinLines $ afterChars : afterLines

-- Not the same as 'unlines', due to trailing \n
joinLines = intercalate "\n"
