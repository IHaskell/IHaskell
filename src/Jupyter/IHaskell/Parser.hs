{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module Jupyter.IHaskell.Parser (
    -- Parser handling
    parseCodeBlocks,
    ParseError(..),
    Loc(..),
    CodeBlock(..),
    DirectiveType(..),
    ) where

-- Imports from 'base'
import           Control.Applicative (some, many, (<|>))
import           Control.Monad (void)
import           Data.Bifunctor (first, second)
import           Data.Char (isAlphaNum)
import           Data.List (findIndex, maximumBy)
import           Data.Monoid ((<>))
import           Data.Ord (comparing)

-- Imports from 'text'
import           Data.Text (Text)
import qualified Data.Text as T

-- Imports from 'megaparsec'
import           Text.Megaparsec (string', parse, manyTill, eof, lookAhead)
import           Text.Megaparsec.Char (alphaNumChar, anyChar, space, char)
import           Text.Megaparsec.Text (Parser)

-- Imports from 'containers'
import           Data.Map (Map)
import qualified Data.Map as Map

-- Imports from 'ghc'
import           Bag (unitBag, bagToList)
import           ErrUtils (ErrMsg)
import           ErrUtils (mkPlainErrMsg)
import           FastString (mkFastString)
import           GHC (getSessionDynFlags, DynFlags)
import           Lexer (P, mkPState, ParseResult(..), unP)
import           Parser (parseTypeSignature, parseImport, parseStatement, parseDeclaration, parseExpression)
import           SrcLoc (mkRealSrcLoc, SrcSpan(..), srcLocLine, srcLocCol, realSrcSpanStart)
import           StringBuffer (stringToStringBuffer)

-- Imports from 'ihaskell'
import           Jupyter.IHaskell.Interpreter (Interpreter, ghc)

-- | A block of code to be evaluated. A single cell may contain many blocks, but the blocks can be
-- evaluated sequentially.
data CodeBlock = Expression Text   -- ^ A Haskell expression.
               | Declarations Text -- ^ A set of declarations.
               | Statement Text    -- ^ A Haskell statement (as if in a `do` block).
               | Import Text       -- ^ An import statement.
               | Directive DirectiveType Text -- ^ An IHaskell directive.
               | LanguagePragma [Text] -- ^ A LANGAUGE pragma enabling some extensions
  deriving (Show, Eq, Ord)

-- | Directive types. Each directive is associated with a string in the directive code block.
data DirectiveType = GetType      -- ^ Get the type of an expression via ':type' (or unique prefixes)
                   | GetInfo      -- ^ Get info about the identifier via ':info' (or unique prefixes)
                   | SetDynFlag   -- ^ Enable or disable an extensions, packages etc. via `:set`.
                                  -- Emulates GHCi's `:set`
                   | LoadFile     -- ^ Load a Haskell module.
                   | ShellCmd     -- ^ Execute a shell command.
                   | GetHelp      -- ^ General help via ':?' or ':help'.
                   | SearchHoogle -- ^ Search for something via Hoogle.
                   | GetDoc       -- ^ Get documentation for an identifier via Hoogle.
                   | GetKind      -- ^ Get the kind of a type via ':kind'.
                   | LoadModule   -- ^ Load and unload modules via ':module'.
  deriving (Show, Eq, Ord)

data ParseError =
       ParseError
         { parseErrorMessage :: String
         , parseErrorLine :: Int
         , parseErrorColumn :: Int
         }
  deriving (Eq, Ord, Show)

-- | Store locations along with a value.
data Loc a = Loc {
    locLine :: Int, -- Where this element is located.
    unLoc :: a          -- Loc element.
  } deriving (Eq, Show, Functor)


parseCodeBlocks :: Text -- ^ Code to parse into blocks
                -> Interpreter (Either ParseError [Loc CodeBlock])
parseCodeBlocks string =
  second mergeConsecutiveDecls <$> sequence <$> mapM parseChunkLocated (layoutChunks string)
  where
    parseChunkLocated (Loc line txt) = do
      result <- parseChunk txt
      return $
        case result of
          Left err    -> Left err { parseErrorLine = parseErrorLine err + line - 1 }
          Right block -> Right (Loc line block)

mergeConsecutiveDecls :: [Loc CodeBlock] -> [Loc CodeBlock]
mergeConsecutiveDecls = foldr go []
  where
    go :: Loc CodeBlock -> [Loc CodeBlock] -> [Loc CodeBlock]
    go blk rest =
      case rest of
        [] -> [blk]
        (x:xs) ->
          case (blk, x) of
            (Loc line (Declarations blkText), Loc line2 (Declarations xText)) ->
              Loc line (Declarations $ T.concat [blkText, T.replicate (line2 - line) "\n", xText]) : xs
            _ -> blk : x : xs

parseChunk :: Text -> Interpreter (Either ParseError CodeBlock)
parseChunk input
  | Right parsed <- parse parseDirective "" input
  = return $ mkDirectiveBlock parsed

  | Right parsed <- parse parsePragma "" input
  = return $ mkPragmaBlock parsed

  | otherwise
  = first selectBestError <$> firstM ($ input) parsers
  where
    parsers :: [Text -> Interpreter (Either ParseError CodeBlock)]
    parsers =
      -- Parsers to try on a block, in order. Expressions must come before statements, because all
      -- expressions are statements (but only some statements are expressions).
      [ parseChunkAs parseImport Import
      , parseChunkAs parseTypeSignature Declarations
      , parseChunkAs parseExpression Expression
      , parseChunkAs parseStatement Statement
      , parseChunkAs parseDeclaration Declarations
      ]

    parseChunkAs :: P a -> (Text -> CodeBlock) -> Text -> Interpreter (Either ParseError CodeBlock)
    parseChunkAs parser mkBlock text = do
      second (const $ mkBlock text) <$> runParser parser text

selectBestError :: [ParseError] -> ParseError
selectBestError = maximumBy farthestError
  where
    farthestError x y = comparing parseErrorLine x y <> comparing parseErrorColumn x y

firstM :: Monad m
       => (a -> m (Either b c))
       -> [a]
       -> m (Either [b] c)
firstM f = go id
  where
    go errs xs =
      case xs of
        [] -> return (Left $ errs [])
        x:xs' -> do
          result <- f x
          case result of
            Left err -> go ((err :) . errs) xs'
            Right v  -> return (Right v)


-- | Run a GHC parser on a string. Return an error message on parse failure or the parsed object.
runParser :: P a -> Text -> Interpreter (Either ParseError a)
runParser parser string = do
  -- Create an initial parser state.
  flags <- ghc getSessionDynFlags
  let filename = "<interactive>"
      location = mkRealSrcLoc (mkFastString filename) 1 1
      buffer = stringToStringBuffer $ T.unpack string
      parseState = mkPState flags buffer location

  -- Convert a GHC parser output into our own.
  return . output flags $ unP parser parseState

  where
    output :: DynFlags -> ParseResult a -> Either ParseError a
    output flags parseResult =
      case parseResult of
        PFailed srcSpan@(RealSrcSpan realSpan) err ->
          Left
            ParseError
              { parseErrorMessage = printErr $ mkPlainErrMsg flags srcSpan err
              , parseErrorLine = srcLocLine $ realSrcSpanStart realSpan
              , parseErrorColumn = srcLocCol $ realSrcSpanStart realSpan
              }

        PFailed srcSpan err ->
          Left
            ParseError
              { parseErrorMessage = printErr $ mkPlainErrMsg flags srcSpan err
              , parseErrorLine = 0
              , parseErrorColumn = 0
              }

        POk _ result -> Right result

    -- Convert an error message from GHC into a string.
    printErr :: ErrMsg -> String
    printErr = unlines . map show . bagToList . unitBag

parsePragma :: Parser (Text, Text)
parsePragma = do
  space
  void $ string' "{-#"
  space
  pragma <- some alphaNumChar
  space
  line <- manyTill anyChar (lookAhead $ string' "#-}")
  space
  void $ string' "#-}"
  return (T.pack pragma, T.strip $ T.pack line)

parseDirective :: Parser (Text, Text)
parseDirective = do
  space
  void $ char ':'
  directive <- some (alphaNumChar <|> char '!' <|> char '?')
  space
  line <- manyTill anyChar (void (char '\n') <|> eof)
  return (T.pack directive, T.pack line)

mkDirectiveBlock :: (Text, Text) -> Either ParseError CodeBlock
mkDirectiveBlock (directive, line) =
  case Map.lookup directive directiveMap of
    Just dirType -> Right $ Directive dirType line
    Nothing      -> Left $ ParseError ("Unknown directive: " ++ T.unpack directive) 1 1
  where
    -- List of directives. Directives that come later in the list are prioritized if they clash in
    -- shortened name with other directives. For example, if 'hoogle' is earlier in the list than
    -- 'help', then ':h' will refer to 'help' and not to 'hoogle'.
    directives :: [(Text, DirectiveType)]
    directives = [ ("type", GetType)
                 , ("info", GetInfo)
                 , ("set", SetDynFlag)
                 , ("load", LoadFile)
                 , ("!", ShellCmd)
                 , ("?", GetHelp)
                 , ("hoogle", SearchHoogle)
                 , ("help", GetHelp)
                 , ("document", GetDoc)
                 , ("kind", GetKind)
                 , ("module", LoadModule)
                 ]

    directiveMap :: Map Text DirectiveType
    directiveMap = Map.fromList $ do
      (word, dirType) <- directives
      map (,dirType) $ tail $ T.inits word

mkPragmaBlock :: (Text, Text) -> Either ParseError CodeBlock
mkPragmaBlock (pragma, line) =
  case T.toUpper pragma of
    "LANGUAGE" -> Right $ LanguagePragma $ T.splitOn "," $ T.replace " " "" line
    other      -> Left $ ParseError ("Unsupported pragma: " ++ T.unpack other) 1 1

-- | Split an input string into chunks based on indentation.
-- A chunk is a line and all lines immediately following that are indented
-- beyond the indentation of the first line. This parses Haskell layout
-- rules properly, and allows using multiline expressions via indentation.
--
-- Quasiquotes are allowed via a post-processing step.
layoutChunks :: Text -> [Loc Text]
layoutChunks = joinQuasiquotes . go 1 . removeComments
  where
    go :: Int -> Text -> [Loc Text]
    go line = filter (not . T.null . unLoc) . map (fmap T.strip) . layoutLines line . T.lines

    layoutLines :: Int -> [Text] -> [Loc Text]
    -- Empty string case.  If there's no input, output is empty.
    layoutLines _ [] = []

    -- Use the indent of the first line to find the end of the first block.
    layoutLines lineIdx (firstLine:rest) =
      let firstIndent = indentLevel firstLine
          blockEnded line = indentLevel line <= firstIndent in
        case findIndex blockEnded rest of
          -- If the first block doesn't end, return the whole string, since
          -- that just means the block takes up the entire string.
          Nothing -> [Loc lineIdx $ T.intercalate "\n" (firstLine:rest)]

          -- We found the end of the block. Split this bit out and recurse.
          Just idx ->
            let (before, after) = splitAt idx rest in
              Loc lineIdx (joinLines $ firstLine:before) : go (lineIdx + idx + 1) (joinLines after)

    -- Compute indent level of a string as number of leading spaces.
    indentLevel :: Text -> Int
    indentLevel str =
      case T.unpack str of
        (' ':str') -> 1 + indentLevel (T.pack str')
        -- Count a tab as two spaces.
        ('\t':str') -> 2 + indentLevel (T.pack str')
        -- Count empty lines as a large indent level, so they're always with the previous expression.
        "" -> 100000
        _ -> 0


-- | Drop comments from Haskell source.
-- Simply gets rid of them, does not replace them in any way.
removeComments :: Text -> Text
removeComments = T.pack . removeOneLineComments . removeMultilineComments 0 0 . T.unpack
  where
    removeOneLineComments str =
      case str of
        -- Don't remove comments after cmd directives
        ':':'!':remaining ->":!" ++ takeLine remaining ++ dropLine remaining

        -- Handle strings.
        '"':remaining ->
          let quoted = takeString remaining
              len = length quoted in
            '"':quoted ++ removeOneLineComments (drop len remaining)

        '-':'-':remaining -> dropLine remaining
        x:xs -> x:removeOneLineComments xs
        [] -> []
      where
        dropLine = removeOneLineComments . dropWhile (/= '\n')

    removeMultilineComments :: Int -> Int -> String -> String
    removeMultilineComments nesting pragmaNesting str =
      case str of
        -- Don't remove comments after cmd directives
        ':':'!':remaining ->":!" ++ takeLine remaining ++
          removeMultilineComments nesting pragmaNesting (dropWhile (/= '\n') remaining)

        -- Handle strings.
        '"':remaining ->
          if nesting == 0
          then
            let quoted = takeString remaining
                len = length quoted in
              '"':quoted ++ removeMultilineComments nesting pragmaNesting (drop len remaining)
          else
            removeMultilineComments nesting pragmaNesting remaining
        '{':'-':'#':remaining ->
          if nesting == 0
          then "{-#" ++ removeMultilineComments nesting (pragmaNesting + 1) remaining
          else removeMultilineComments nesting pragmaNesting remaining
        '#':'-':'}':remaining ->
          if nesting == 0
          then if pragmaNesting > 0
               then '#':'-':'}':removeMultilineComments nesting (pragmaNesting - 1) remaining
               else '#':'-':'}':removeMultilineComments nesting pragmaNesting remaining
          else removeMultilineComments nesting pragmaNesting remaining
        '{':'-':remaining -> removeMultilineComments (nesting + 1) pragmaNesting remaining
        '-':'}':remaining ->
          if nesting > 0
          then removeMultilineComments (nesting - 1) pragmaNesting remaining
          else '-':'}':removeMultilineComments nesting pragmaNesting remaining
        x:xs ->
          if nesting > 0
          then removeMultilineComments nesting pragmaNesting xs
          else x:removeMultilineComments nesting pragmaNesting xs
        [] -> []

    takeLine = takeWhile (/= '\n')

    -- Take a part of a string that ends in an unescaped quote.
    takeString str = case str of
      escaped@('\\':'"':_) -> escaped
      '"':_ -> "\""
      x:xs -> x:takeString xs
      [] -> []


-- | Post processing step to combine quasiquoted blocks into single blocks.
-- This is necessary because quasiquoted blocks don't follow normal indentation rules.
joinQuasiquotes :: [Loc Text] -> [Loc Text]
joinQuasiquotes = reverse . joinQuasiquotes' . reverse
  where
    -- This operates by finding |] and then joining blocks until a line
    -- that has some corresponding [...|. This is still a hack, but close to
    -- good enough.
    joinQuasiquotes' [] = []
    joinQuasiquotes' (block:blocks) =
      if "|]" `T.isInfixOf` unLoc block
      then
        let (pieces, rest) = break (hasQuasiquoteStart . unLoc) blocks
        in case rest of
          [] -> block : joinQuasiquotes' blocks
          startBlock:blocks' ->
            concatBlocks (block : pieces ++ [startBlock]) : joinQuasiquotes blocks'
      else block : joinQuasiquotes' blocks

    -- Combine a lit of reversed blocks into a single, non-reversed block.
    concatBlocks :: [Loc Text] -> Loc Text
    concatBlocks blocks = Loc (locLine $ last blocks) $ joinLines $ map unLoc $ reverse blocks

    -- Does this string have a [...| in it?
    hasQuasiquoteStart :: Text -> Bool
    hasQuasiquoteStart str =
      case T.break (== '[') str of
        (_, "") -> False
        (_, rest) ->
          case T.break (== '|') (T.tail rest) of
            (_, "") -> False
            (chars, _) -> T.all isIdentChar chars

    isIdentChar :: Char -> Bool
    isIdentChar c = isAlphaNum c || c == '_' || c == '\''

-- Not the same as 'unlines', due to trailing \n
joinLines :: [Text] -> Text
joinLines = T.intercalate "\n"
