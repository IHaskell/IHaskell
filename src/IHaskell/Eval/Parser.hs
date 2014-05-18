{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module IHaskell.Eval.Parser (
    parseString,
    CodeBlock(..),
    StringLoc(..),
    DirectiveType(..),
    LineNumber,
    ColumnNumber,
    ErrMsg,
    layoutChunks,
    parseDirective,
    getModuleName,
    Located(..),
    ) where

-- Hide 'unlines' to use our own 'joinLines' instead.
import ClassyPrelude hiding (head, tail, liftIO, unlines, maximumBy)

import Data.List (findIndex, maximumBy, maximum, inits)
import Data.String.Utils (startswith, strip, split)
import Data.List.Utils (subIndex)
import Prelude (init, last, head, tail)

import Bag
import ErrUtils hiding (ErrMsg)
import FastString
import GHC hiding (Located)
import Lexer
import OrdList
import Outputable hiding ((<>))
import SrcLoc hiding (Located)
import StringBuffer

import Language.Haskell.GHC.Parser
import IHaskell.Eval.Util

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
  | Module String                      -- ^ A full Haskell module, to be compiled and loaded.
  | ParseError StringLoc ErrMsg        -- ^ An error indicating that parsing the code block failed.
  deriving (Show, Eq)

-- | Directive types. Each directive is associated with a string in the
-- directive code block.
data DirectiveType
  = GetType         -- ^ Get the type of an expression via ':type' (or unique prefixes)
  | GetInfo         -- ^ Get info about the identifier via ':info' (or unique prefixes)
  | SetDynFlag      -- ^ Enable or disable an extensions, packages etc. via `:set`. Emulates GHCi's `:set`
  | LoadFile        -- ^ Load a Haskell module.
  | SetOption       -- ^ Set IHaskell kernel option `:option`.
  | SetExtension    -- ^ `:extension Foo` is a shortcut for `:set -XFoo`
  | ShellCmd        -- ^ Execute a shell command.
  | GetHelp         -- ^ General help via ':?' or ':help'.
  | SearchHoogle    -- ^ Search for something via Hoogle.
  | GetDoc          -- ^ Get documentation for an identifier via Hoogle.
  | GetKind         -- ^ Get the kind of a type via ':kind'.
  deriving (Show, Eq)

-- | Parse a string into code blocks.
parseString :: GhcMonad m => String -> m [Located CodeBlock]
parseString codeString = do
  -- Try to parse this as a single module.
  flags <- getSessionDynFlags
  let output = runParser flags parserModule codeString
  case output of
    Parsed {} -> return [Located 1 $ Module codeString]
    Failure {} -> do
      -- Split input into chunks based on indentation.
      let chunks = layoutChunks $ removeComments codeString
      result <- joinFunctions <$> processChunks [] chunks

      -- Return to previous flags. When parsing, flags can be set to make
      -- sure parsing works properly. But we don't want those flags to be
      -- set during evaluation until the right time.
      setSessionDynFlags flags
      return result
  where
    parseChunk :: GhcMonad m => String -> LineNumber -> m (Located CodeBlock)
    parseChunk chunk line = Located line <$>
      if isDirective chunk
      then return $ parseDirective chunk line
      else parseCodeChunk chunk line

    processChunks :: GhcMonad m => [Located CodeBlock] -> [Located String] -> m [Located CodeBlock]
    processChunks accum remaining =
      case remaining of
        -- If we have no more remaining lines, return the accumulated results.
        [] -> return $ reverse accum

        -- If we have more remaining, parse the current chunk and recurse.
        Located line chunk:remaining ->  do
          block <- parseChunk chunk line
          activateParsingExtensions $ unloc block
          processChunks (block : accum) remaining

    -- Test wither a given chunk is a directive.
    isDirective :: String -> Bool
    isDirective = startswith ":" . strip

    -- Number of lines in this string.
    nlines :: String -> Int
    nlines = length . lines

activateParsingExtensions :: GhcMonad m => CodeBlock -> m ()
activateParsingExtensions (Directive SetExtension ext) = void $ setExtension ext
activateParsingExtensions (Directive SetDynFlag flags) =
  case stripPrefix "-X" flags of
    Just ext -> void $ setExtension ext
    Nothing -> return ()
activateParsingExtensions _ = return ()

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
        result:_ -> return result
  where
    successes :: [ParseOutput a] -> [a]
    successes [] = []
    successes (Parsed a:rest) = a : successes rest
    successes (_:rest) = successes rest

    failures :: [ParseOutput a] -> [(ErrMsg, LineNumber, ColumnNumber)]
    failures [] = []
    failures (Failure msg (Loc line col):rest) = (msg, line, col) : failures rest
    failures (_:rest) = failures rest

    bestError :: [(ErrMsg, LineNumber, ColumnNumber)] -> CodeBlock
    bestError errors = ParseError (Loc (line + startLine - 1) col) msg
      where
        (msg, line, col) = maximumBy compareLoc errors
        compareLoc (_, line1, col1) (_, line2, col2) = compare line1 line2 <> compare col1 col2

    statementToExpression :: DynFlags -> ParseOutput CodeBlock -> ParseOutput CodeBlock
    statementToExpression flags (Parsed (Statement stmt)) = Parsed result
      where result = if isExpr flags stmt
                     then Expression stmt
                     else Statement stmt
    statementToExpression _ other = other

    -- Check whether a string is a valid expression.
    isExpr :: DynFlags -> String -> Bool
    isExpr flags str = case runParser flags parserExpression str of
      Parsed {} -> True
      _ -> False

    tryParser :: String -> (String -> CodeBlock, String -> ParseOutput String) -> ParseOutput CodeBlock
    tryParser string (blockType, parser) = case parser string of
      Parsed res -> Parsed (blockType res)
      Failure err loc -> Failure err loc

    parsers :: DynFlags -> [(String -> CodeBlock, String -> ParseOutput String)]
    parsers flags =
      [ (Import,        unparser parserImport)
      , (TypeSignature, unparser parserTypeSignature)
      , (Declaration,   unparser parserDeclaration)
      , (Statement,     unparser parserStatement)
      ]
      where
        unparser :: Parser a -> String -> ParseOutput String
        unparser parser code =
          case runParser flags parser code of
            Parsed out -> Parsed code
            Partial out strs -> Partial code strs
            Failure err loc -> Failure err loc

-- | Find consecutive declarations of the same function and join them into
-- a single declaration. These declarations may also include a type
-- signature, which is also joined with the subsequent declarations.
joinFunctions :: [Located CodeBlock] -> [Located CodeBlock]
joinFunctions (Located line (Declaration decl) : rest) =
    -- Find all declarations having the same name as this one.
    let (decls, other) = havingSameName rest in
      -- Convert them into a single declaration.
      Located line (Declaration (joinLines $ map (undecl . unloc) decls)) : joinFunctions other
  where
    undecl (Declaration decl) = decl
    undecl _ = error "Expected declaration!"

    -- Get all declarations with the same name as the first declaration.
    -- The name of a declaration is the first word, which we expect to be
    -- the name of the function.
    havingSameName :: [Located CodeBlock] -> ([Located CodeBlock], [Located CodeBlock])
    havingSameName blocks =
      let name = head $ words decl
          sameName = takeWhile (isNamedDecl name) rest
          others = drop (length sameName) rest in
        (Located line (Declaration decl) : sameName, others)

    isNamedDecl :: String -> Located CodeBlock -> Bool
    isNamedDecl name (Located _ (Declaration dec)) = head (words dec) == name
    isNamedDecl _ _ = False

-- Allow a type signature followed by declarations to be joined to the
-- declarations. Parse the declaration joining separately.
joinFunctions (Located line (TypeSignature sig) : Located dl (Declaration decl) : rest) =
  Located line (Declaration $ sig ++ "\n" ++ joinedDecl):remaining
  where Located _ (Declaration joinedDecl):remaining = joinFunctions $ Located dl (Declaration decl) : rest

-- Also allow two type signatures. This is necessary for operator
-- declarations in which you have a fixity declaration.
joinFunctions (Located line (TypeSignature sig) :
               Located _ (TypeSignature sig')   :
               Located dl (Declaration decl)    : rest) =
  Located line (Declaration $ intercalate "\n" [sig, sig', joinedDecl]):remaining
  where Located _ (Declaration joinedDecl):remaining = joinFunctions $ Located dl (Declaration decl) : rest

joinFunctions (x:xs) = x : joinFunctions xs
joinFunctions [] = []


-- | Parse a directive of the form :directiveName.
parseDirective :: String       -- ^ Directive string.
               -> Int          -- ^ Line number at which the directive appears.
               -> CodeBlock    -- ^ Directive code block or a parse error.

parseDirective (':':'!':directive) line = Directive ShellCmd $ '!':directive
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
      [ (GetType,      "type")
      , (GetKind,      "kind")
      , (GetInfo,      "info")
      , (SearchHoogle, "hoogle")
      , (GetDoc,       "documentation")
      , (SetDynFlag,   "set")
      , (LoadFile,     "load")
      , (SetOption,    "option")
      , (SetExtension, "extension")
      , (GetHelp,      "?")
      , (GetHelp,      "help")
      ]
parseDirective _ _ = error "Directive must start with colon!"

-- | Parse a module and return the name declared in the 'module X where'
-- line. That line is required, and if it does not exist, this will error.
-- Names with periods in them are returned piece y piece.
getModuleName :: GhcMonad m => String -> m [String]
getModuleName moduleSrc = do
  flags <- getSessionDynFlags
  let output = runParser flags parserModule moduleSrc
  case output of
    Failure {} -> error "Module parsing failed."
    Parsed mod ->
      case unLoc <$> hsmodName (unLoc mod) of
        Nothing -> error "Module must have a name."
        Just name -> return $ split "." $ moduleNameString name

-- Not the same as 'unlines', due to trailing \n
joinLines :: [String] -> String
joinLines = intercalate "\n"
