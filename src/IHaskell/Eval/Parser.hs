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
    PragmaType(..),
    ) where

import           IHaskellPrelude
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as CBS

import           Data.List (maximumBy, inits)
import           Prelude (head, tail)
import           Control.Monad (msum)

import           GHC hiding (Located)

import           Language.Haskell.GHC.Parser
import           IHaskell.Eval.Util
import           StringUtils (strip, split)

-- | A block of code to be evaluated. Each block contains a single element - one declaration,
-- statement, expression, etc. If parsing of the block failed, the block is instead a ParseError,
-- which has the error location and error message.
data CodeBlock = Expression String              -- ^ A Haskell expression.
               | Declaration String             -- ^ A data type or function declaration.
               | Statement String               -- ^ A Haskell statement (as if in a `do` block).
               | Import String                  -- ^ An import statement.
               | TypeSignature String           -- ^ A lonely type signature (not above a function
                                                -- declaration).
               | Directive DirectiveType String -- ^ An IHaskell directive.
               | Module String                  -- ^ A full Haskell module, to be compiled and loaded.
               | ParseError StringLoc ErrMsg    -- ^ An error indicating that parsing the code block
                                                -- failed.
               | Pragma PragmaType [String]     -- ^ A list of GHC pragmas (from a {-# LANGUAGE ... #-}
                                                -- block)
  deriving (Show, Eq)

-- | Directive types. Each directive is associated with a string in the directive code block.
data DirectiveType = GetType      -- ^ Get the type of an expression via ':type' (or unique prefixes)
                   | GetInfo      -- ^ Get info about the identifier via ':info' (or unique prefixes)
                   | SetDynFlag   -- ^ Enable or disable an extensions, packages etc. via `:set`.
                                  -- Emulates GHCi's `:set`
                   | LoadFile     -- ^ Load a Haskell module.
                   | SetOption    -- ^ Set IHaskell kernel option `:option`.
                   | SetExtension -- ^ `:extension Foo` is a shortcut for `:set -XFoo`
                   | ShellCmd     -- ^ Execute a shell command.
                   | GetHelp      -- ^ General help via ':?' or ':help'.
                   | SearchHoogle -- ^ Search for something via Hoogle.
                   | GetDoc       -- ^ Get documentation for an identifier via Hoogle.
                   | GetKind      -- ^ Get the kind of a type via ':kind'.
                   | LoadModule   -- ^ Load and unload modules via ':module'.
  deriving (Show, Eq)

-- | Pragma types. Only LANGUAGE pragmas are currently supported. Other pragma types are kept around
-- as a string for error reporting.
data PragmaType = PragmaLanguage
                | PragmaUnsupported String
  deriving (Show, Eq)

-- | Parse a string into code blocks.
parseString :: String -> Ghc [Located CodeBlock]
parseString codeString = do
  -- Try to parse this as a single module.
  flags <- getSessionDynFlags
  let output = runParser flags parserModule codeString
  case output of
    Parsed mod
      | Just _ <- hsmodName (unLoc mod) -> return [Located 1 $ Module codeString]
    _ -> do
      -- Split input into chunks based on indentation.
      let chunks = layoutChunks $ removeComments codeString
      result <- joinFunctions <$> processChunks [] chunks

      -- Return to previous flags. When parsing, flags can be set to make sure parsing works properly. But
      -- we don't want those flags to be set during evaluation until the right time.
      _ <- setSessionDynFlags flags
      return result

  where
    parseChunk :: GhcMonad m => String -> LineNumber -> m (Located CodeBlock)
    parseChunk chunk line = Located line <$> handleChunk chunk line
      where
        handleChunk chunk line
          | isDirective chunk = return $ parseDirective chunk line
          | isPragma chunk = return $ parsePragma chunk line
          | otherwise = parseCodeChunk chunk line

    processChunks :: GhcMonad m => [Located CodeBlock] -> [Located String] -> m [Located CodeBlock]
    processChunks accum remaining =
      case remaining of
        -- If we have no more remaining lines, return the accumulated results.
        [] -> return $ reverse accum

        -- If we have more remaining, parse the current chunk and recurse.
        Located line chunk:remaining -> do
          block <- parseChunk chunk line
          activateExtensions $ unloc block
          processChunks (block : accum) remaining

    -- Test whether a given chunk is a directive.
    isDirective :: String -> Bool
    isDirective = isPrefixOf ":" . strip

    -- Test if a chunk is a pragma.
    isPragma :: String -> Bool
    isPragma = isPrefixOf "{-#" . strip

activateExtensions :: GhcMonad m => CodeBlock -> m ()
activateExtensions (Directive SetExtension ext) = void $ setExtension ext
activateExtensions (Directive SetDynFlag flags) =
  case stripPrefix "-X" flags of
    Just ext -> void $ setExtension ext
    Nothing  -> return ()
activateExtensions (Pragma PragmaLanguage extensions) = void $ setAll extensions
  where
    setAll :: GhcMonad m => [String] -> m (Maybe String)
    setAll exts = do
      errs <- mapM setExtension exts
      return $ msum errs
activateExtensions _ = return ()

-- | Parse a single chunk of code, as indicated by the layout of the code.
parseCodeChunk :: GhcMonad m => String -> LineNumber -> m CodeBlock
parseCodeChunk code startLine = do
  flags <- getSessionDynFlags
  let 
      -- Try each parser in turn.
      rawResults = map (tryParser code) (parsers flags)

      -- Convert statements into expressions where we can
      results = map (statementToExpression flags) rawResults
  case successes results of
    -- If none of them succeeded, choose the best error message to display. Only one of the error
    -- messages is actually relevant.
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
      where
        result = if isExpr flags stmt
                   then Expression stmt
                   else Statement stmt
    statementToExpression _ other = other

    -- Check whether a string is a valid expression.
    isExpr :: DynFlags -> String -> Bool
    isExpr flags str =
      case runParser flags parserExpression str of
        Parsed{} -> True
        _        -> False

    tryParser :: String -> (String -> CodeBlock, String -> ParseOutput String) -> ParseOutput CodeBlock
    tryParser string (blockType, parser) =
      case parser string of
        Parsed res      -> Parsed (blockType res)
        Failure err loc -> Failure err loc
        otherwise       -> error "tryParser failed, output was neither Parsed nor Failure"

    parsers :: DynFlags -> [(String -> CodeBlock, String -> ParseOutput String)]
    parsers flags =
      [ (Import, unparser parserImport)
      , (TypeSignature, unparser parserTypeSignature)
      , (Statement, unparser parserStatement)
      , (Declaration, unparser parserDeclaration)
      ]
      where
        unparser :: Parser a -> String -> ParseOutput String
        unparser parser code =
          case runParser flags parser code of
            Parsed out       -> Parsed code
            Partial out strs -> Partial code strs
            Failure err loc  -> Failure err loc

-- | Find consecutive declarations of the same function and join them into a single declaration.
-- These declarations may also include a type signature, which is also joined with the subsequent
-- declarations.
joinFunctions :: [Located CodeBlock] -> [Located CodeBlock]
joinFunctions [] = []
joinFunctions blocks =
  if signatureOrDecl $ unloc $ head blocks
    then Located lnum (conjoin $ map unloc decls) : joinFunctions rest
    else head blocks : joinFunctions (tail blocks)
  where
    decls = takeWhile (signatureOrDecl . unloc) blocks
    rest = drop (length decls) blocks
    lnum = line $ head decls

    signatureOrDecl (Declaration _) = True
    signatureOrDecl (TypeSignature _) = True
    signatureOrDecl _ = False

    str (Declaration s) = s
    str (TypeSignature s) = s
    str _ = error "Expected declaration or signature"

    conjoin :: [CodeBlock] -> CodeBlock
    conjoin = Declaration . intercalate "\n" . map str

-- | Parse a pragma of the form {-# LANGUAGE ... #-}
parsePragma :: String       -- ^ Pragma string.
            -> Int          -- ^ Line number at which the directive appears.
            -> CodeBlock    -- ^ Pragma code block or a parse error.
parsePragma ('{':'-':'#':pragma) line =
  let commaToSpace :: Char -> Char
      commaToSpace ',' = ' '
      commaToSpace x = x
      pragmas = words $ takeWhile (/= '#') $ map commaToSpace pragma
  in case pragmas of
    --empty string pragmas are unsupported
    [] -> Pragma (PragmaUnsupported "") []
    "LANGUAGE":xs -> Pragma PragmaLanguage xs
    x:xs -> Pragma (PragmaUnsupported x) xs

-- | Parse a directive of the form :directiveName.
parseDirective :: String       -- ^ Directive string.
               -> Int          -- ^ Line number at which the directive appears.
               -> CodeBlock    -- ^ Directive code block or a parse error.
parseDirective (':':'!':directive) line = Directive ShellCmd $ '!' : directive
parseDirective (':':directive) line =
  case find rightDirective directives of
    Just (directiveType, _) -> Directive directiveType arg
      where arg = unwords restLine
            _:restLine = words directive
    Nothing ->
      let directiveStart =
                            case words directive of
                              []      -> ""
                              first:_ -> first
      in ParseError (Loc line 1) $ "Unknown directive: '" ++ directiveStart ++ "'."
  where
    rightDirective (_, dirname) =
      case words directive of
        []    -> False
        dir:_ -> dir `elem` tail (inits dirname)
    directives =
      [ (LoadModule, "module")
      , (GetType, "type")
      , (GetKind, "kind")
      , (GetInfo, "info")
      , (SearchHoogle, "hoogle")
      , (GetDoc, "documentation")
      , (SetDynFlag, "set")
      , (LoadFile, "load")
      , (SetOption, "option")
      , (SetExtension, "extension")
      , (GetHelp, "?")
      , (GetHelp, "help")
      ]
parseDirective _ _ = error "Directive must start with colon!"

-- | Parse a module and return the name declared in the 'module X where' line. That line is
-- required, and if it does not exist, this will error. Names with periods in them are returned
-- piece y piece.
getModuleName :: GhcMonad m => String -> m [String]
getModuleName moduleSrc = do
  flags <- getSessionDynFlags
  let output = runParser flags parserModule moduleSrc
  case output of
    Failure{} -> error "Module parsing failed."
    Parsed mod ->
      case unLoc <$> hsmodName (unLoc mod) of
        Nothing   -> error "Module must have a name."
        Just name -> return $ split "." $ moduleNameString name
    otherwise -> error "getModuleName failed, output was neither Parsed nor Failure"
