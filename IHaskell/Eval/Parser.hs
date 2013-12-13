{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module IHaskell.Eval.Parser (
    parseString,
    CodeBlock(..),
    StringLoc(..),
    DirectiveType(..),
    LineNumber,
    ColumnNumber,
    ErrMsg,
    splitAtLoc,
    layoutChunks,
    parseDirective
    ) where

-- Hide 'unlines' to use our own 'joinLines' instead.
import ClassyPrelude hiding (liftIO, unlines)

import Data.List (findIndex, maximumBy, maximum, inits)
import Data.String.Utils (startswith, strip)
import Data.List.Utils (subIndex)
import Prelude (init, last, head, tail)

import Bag
import ErrUtils hiding (ErrMsg)
import FastString
import GHC
import Lexer
import OrdList
import Outputable hiding ((<>))
import SrcLoc
import StringBuffer

import IHaskell.GHC.HaskellParser

-- | A line number in an input string.
type LineNumber   = Int

-- | A column number in an input string.
type ColumnNumber = Int

-- | An error message string.
type ErrMsg = String

-- | A location in an input string.
data StringLoc = Loc LineNumber ColumnNumber deriving Show

-- | A block of code to be evaluated.
-- Each block contains a single element - one declaration, statement,
-- expression, etc. If parsing of the block failed, the block is instead
-- a ParseError, which has the error location and error message.
data CodeBlock
  = Expression String                  -- ^ A Haskell expression.
  | Declaration String                 -- ^ A data type or function declaration.
  | Statement String                   -- ^ A Haskell statement (as if in a `do` block).
  | Import String                      -- ^ An import statement.
  | TypeSignature String               -- ^ A lonely type signature (not above a function declaration).
  | Directive DirectiveType String     -- ^ An IHaskell directive.
  | ParseError StringLoc ErrMsg        -- ^ An error indicating that parsing the code block failed.
  deriving Show

-- | Directive types. Each directive is associated with a string in the
-- directive code block.
data DirectiveType
  = GetType         -- ^ Get the type of an expression via ':type' (or unique prefixes)
  | GetInfo         -- ^ Get info about the identifier via ':info' (or unique prefixes)
  | SetExtension    -- ^ Enable or disable an extension via ':extension' (or prefixes)
  deriving Show

-- | Output from running a parser.
data ParseOutput a
    = Failure ErrMsg StringLoc    -- ^ Parser failed with given error message and location.
    | Success a (String, String)  -- ^ Parser succeeded with an output.
                                  -- Auxiliary strings say what part of the
                                  -- input string was used and what
                                  -- part is remaining.

-- $setup
-- >>> import GHC
-- >>> import GHC.Paths
-- >>> import IHaskell.Eval.Parser
-- >>> let ghc = runGhc (Just libdir)
-- >>> let test = ghc . parseString

-- $extendedParserTests
--
-- >>> test ""
-- []
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
-- [ParseError (Loc 1 13) "parse error (possibly incorrect indentation or mismatched brackets)"]
--
-- >>> test "data X = 3"
-- [ParseError (Loc 1 10) "Illegal literal in type (use -XDataKinds to enable): 3"]


-- | Parse a string into code blocks.
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
parseString :: GhcMonad m => String -> m [CodeBlock]
parseString codeString =
  -- Split input into chunks based on indentation.
  let chunks = layoutChunks $ dropComments codeString in
    joinFunctions <$> processChunks 1 [] chunks
  where
    parseChunk :: GhcMonad m => String -> LineNumber -> m CodeBlock
    parseChunk chunk line =
      if isDirective chunk
      then return $ parseDirective chunk line
      else parseCodeChunk chunk line

    processChunks :: GhcMonad m => LineNumber -> [CodeBlock] -> [String] -> m [CodeBlock]
    processChunks line accum remaining =
      case remaining of
        -- If we have no more remaining lines, return the accumulated results.
        [] -> return $ reverse accum

        -- If we have more remaining, parse the current chunk and recurse.
        chunk:remaining ->  do
          block <- parseChunk chunk line
          processChunks (line + nlines chunk) (block : accum) remaining

    -- Test wither a given chunk is a directive.
    isDirective :: String -> Bool
    isDirective = startswith ":" . strip

    -- Number of lines in this string.
    nlines :: String -> Int
    nlines = length . lines

-- | Parse a single chunk of code, as indicated by the layout of the code.
parseCodeChunk :: GhcMonad m => String -> LineNumber  -> m CodeBlock
parseCodeChunk code startLine = do
    flags <- getSessionDynFlags
    let
        -- Try each parser in turn.
        rawResults = map (tryParser code) (parsers flags)

        -- Convert statements into expressions where we can
        results = map (statementToExpression flags) rawResults in
      case successes results of
        -- If none of them succeeded, choose the best error message to
        -- display. Only one of the error messages is actually relevant.
        [] -> return $ bestError $ failures results

        -- If one of the parsers succeeded
        (result, used, remaining):_ -> 
          return $ if not . null . strip $ remaining
                   then ParseError (Loc 1 1) $ "Could not parse " ++ code
                   else result
  where
    successes :: [ParseOutput a] -> [(a, String, String)]
    successes [] = []
    successes (Success a (used, rem):rest) = (a, used, rem) : successes rest
    successes (_:rest) = successes rest

    failures :: [ParseOutput a] -> [(ErrMsg, LineNumber, ColumnNumber)]
    failures [] = []
    failures (Failure msg (Loc line col):rest) = (msg, line, col) : failures rest
    failures (_:rest) = failures rest

    bestError :: [(ErrMsg, LineNumber, ColumnNumber)] -> CodeBlock
    bestError errors = ParseError (Loc line col) msg
      where
        (msg, line, col) = maximumBy compareLoc errors
        compareLoc (_, line1, col1) (_, line2, col2) = compare line1 line2 <> compare col1 col2

    statementToExpression :: DynFlags -> ParseOutput CodeBlock -> ParseOutput CodeBlock
    statementToExpression flags (Success (Statement stmt) strs) = Success result strs
      where result = if isExpr flags stmt
                     then Expression stmt
                     else Statement stmt
    statementToExpression _ other = other

    -- Check whether a string is a valid expression.
    isExpr :: DynFlags -> String -> Bool
    isExpr flags str = case runParser flags fullExpression str of
      Failure {} -> False
      Success {} -> True

    tryParser :: String -> (String -> CodeBlock, String -> ParseOutput String) -> ParseOutput CodeBlock
    tryParser string (blockType, parser) = case parser string of
      Success res (used, remaining) -> Success (blockType res) (used, remaining)
      Failure err loc -> Failure err loc

    parsers :: DynFlags -> [(String -> CodeBlock, String -> ParseOutput String)]
    parsers flags =
      [ (Import,        unparser toCode   partialImport)
      , (TypeSignature, unparser toCode   partialTypeSignature)
      , (Declaration,   unparser listCode partialDeclaration)
      , (Statement,     unparser toCode   partialStatement)
      ]
      where
        toCode :: Outputable a => a -> String
        toCode = strip . showSDoc flags . ppr

        listCode :: Outputable a => OrdList a -> String
        listCode = joinLines . map toCode . fromOL

        unparser :: (a -> String) -> P a -> String -> ParseOutput String
        unparser postprocess parser code =
          case runParser flags parser code of
            Success out strs -> Success (postprocess out) strs
            Failure err loc -> Failure err loc

-- | Find consecutive declarations of the same function and join them into
-- a single declaration. These declarations may also include a type
-- signature, which is also joined with the subsequent declarations.
joinFunctions :: [CodeBlock] -> [CodeBlock]
joinFunctions (Declaration decl : rest) =
    -- Find all declarations having the same name as this one.
    let (decls, other) = havingSameName rest in
      -- Convert them into a single declaration.
      Declaration (joinLines $ map undecl decls) : joinFunctions other
  where
    undecl (Declaration decl) = decl
    undecl _ = error "Expected declaration!"

    -- Get all declarations with the same name as the first declaration.
    -- The name of a declaration is the first word, which we expect to be
    -- the name of the function.
    havingSameName :: [CodeBlock] -> ([CodeBlock], [CodeBlock]) 
    havingSameName blocks =
      let name = head $ words decl
          sameName = takeWhile (isNamedDecl name) rest 
          others = drop (length sameName) rest in
        (Declaration decl : sameName, others)

    isNamedDecl :: String -> CodeBlock -> Bool
    isNamedDecl name (Declaration dec) = head (words dec) == name
    isNamedDecl _ _ = False

-- Allow a type signature followed by declarations to be joined to the
-- declarations. Parse the declaration joining separately.
joinFunctions (TypeSignature sig : Declaration decl : rest) = (Declaration $ sig ++ "\n" ++ joinedDecl):remaining
  where Declaration joinedDecl:remaining = joinFunctions $ Declaration decl : rest 
        
joinFunctions (x:xs) = x : joinFunctions xs
joinFunctions [] = []


-- | Parse a directive of the form :directiveName.
--
-- >>> parseDirective ":ty hello" 0
-- Directive GetType "hello"
--
-- >>> parseDirective ":inf goodbye" 0
-- Directive GetInfo "goodbye"
--
-- >>> parseDirective ":nope goodbye" 11
-- ParseError (Loc 11 1) "Unknown directive: 'nope'."
parseDirective :: String       -- ^ Directive string.
               -> Int          -- ^ Line number at which the directive appears.
               -> CodeBlock    -- ^ Directive code block or a parse error.
parseDirective (':':directive) line = case find rightDirective directives of
  Just (directiveType, _) -> Directive directiveType arg
    where arg = unwords restLine
          _:restLine = words directive
  Nothing -> 
    let directiveStart = case words directive of
          [] -> ""
          first:_ -> first in
      ParseError (Loc line 1) $ "Unknown directive: '" ++ directiveStart ++ "'."
  where
    rightDirective (_, dirname) = case words directive of
      [] -> False
      dir:_ -> dir `elem` tail (inits dirname)
    directives =
      [(GetType, "type")
      ,(GetInfo, "info")
      ,(SetExtension, "extension")
      ]
parseDirective _ _ = error "Directive must start with colon!"

-- | Run a GHC parser on a string. Return success or failure with
-- associated information for both.
runParser :: DynFlags -> P a -> String -> ParseOutput a
runParser flags parser str =
  -- Create an initial parser state.
  let filename = "<interactive>"
      location = mkRealSrcLoc (mkFastString filename) 1 1
      buffer = stringToStringBuffer str
      parseState = mkPState flags buffer location in
    -- Convert a GHC parser output into our own.
    toParseOut $ unP parser parseState
  where
    toParseOut :: ParseResult a -> ParseOutput a
    toParseOut (PFailed span@(RealSrcSpan realSpan) err) = 
      let errMsg = printErrorBag $ unitBag $ mkPlainErrMsg flags span err
          line = srcLocLine $ realSrcSpanStart realSpan
          col = srcLocCol $ realSrcSpanStart realSpan
        in Failure errMsg $ Loc line col

    toParseOut (PFailed span err) = 
      let errMsg = printErrorBag $ unitBag $ mkPlainErrMsg flags span err
        in Failure errMsg $ Loc 0 0

    toParseOut (POk parseState result) = 
      let parseEnd = realSrcSpanStart $ last_loc parseState
          endLine = srcLocLine parseEnd
          endCol = srcLocCol parseEnd
          (before, after) = splitAtLoc endLine endCol str in
        Success result (before, after)

    -- Convert the bag of errors into an error string.
    printErrorBag bag = joinLines . map show $ bagToList bag

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

-- | Split an input string into chunks based on indentation.
-- A chunk is a line and all lines immediately following that are indented
-- beyond the indentation of the first line. This parses Haskell layout
-- rules properly, and allows using multiline expressions via indentation. 
--
-- >>> layoutChunks "a string"
-- ["a string"]
--
-- >>> layoutChunks "a\n string"
-- ["a\n string"]
--
-- >>> layoutChunks "a\n string\nextra"
-- ["a\n string","extra"]
layoutChunks :: String -> [String]
layoutChunks string = layoutLines (lines string)
  where
    layoutLines :: [String] -> [String]
    -- Empty string case.  If there's no input, output is empty.
    layoutLines [] = []

    -- Use the indent of the first line to find the end of the first block.
    layoutLines (firstLine:rest) = 
      let firstIndent = indentLevel firstLine
          blockEnded line = indentLevel line <= firstIndent in
        case findIndex blockEnded rest of
          -- If the first block doesn't end, return the whole string, since
          -- that just means the block takes up the entire string. 
          Nothing -> [string]

          -- We found the end of the block. Split this bit out and recurse.
          Just idx -> 
              joinLines (firstLine:take idx rest) : layoutChunks (joinLines $ drop idx rest)

    -- Compute indent level of a string as number of leading spaces.
    indentLevel :: String -> Int
    indentLevel (' ':str) = 1 + indentLevel str
    indentLevel _ = 0

-- Not the same as 'unlines', due to trailing \n
joinLines :: [String] -> String
joinLines = intercalate "\n"

-- | Drop comments from Haskell source.
dropComments :: String -> String
dropComments = removeOneLineComments . removeMultilineComments
  where
    removeOneLineComments ('-':'-':remaining) = removeOneLineComments (dropWhile (/= '\n') remaining)
    removeOneLineComments (x:xs) = x:removeOneLineComments xs
    removeOneLineComments x = x

    removeMultilineComments ('{':'-':remaining) = 
      case subIndex "-}" remaining of
        Nothing -> ""
        Just idx -> removeMultilineComments $ drop (2 + idx) remaining
    removeMultilineComments (x:xs) = x:removeMultilineComments xs
    removeMultilineComments x = x
