{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeApplications #-}
module Language.Haskell.GHC.Parser (
  -- Parser handling
  runParser,
  LineNumber,
  ColumnNumber,
  ErrMsg,
  StringLoc(..),
  ParseOutput(..),
  Parser,
  Located(..),

  -- Different parsers
  parserStatement,
  parserImport,
  parserDeclaration,
  parserTypeSignature,
  parserModule,
  parserExpression,
  parsePragmasIntoDynFlags,

  -- Haskell string preprocessing.
  removeComments,
  layoutChunks,
  ) where

import Data.List (intercalate, findIndex, isInfixOf)
import Data.Char (isAlphaNum)

#if MIN_VERSION_ghc(9,8,0)
import GHC.Driver.Config.Parser (initParserOpts)
import GHC.Parser.Errors.Types (PsMessage(..))
import GHC.Types.Error (defaultDiagnosticOpts, getMessages, MsgEnvelope(..))
import GHC.Utils.Error (diagnosticMessage, formatBulleted)
import GHC.Utils.Outputable (defaultSDocContext, renderWithContext)
#elif MIN_VERSION_ghc(9,6,0)
import GHC.Driver.Config.Parser (initParserOpts)
import GHC.Parser.Errors.Types (PsMessage(..))
import GHC.Types.Error (getMessages, MsgEnvelope(..))
import GHC.Utils.Error (diagnosticMessage, defaultDiagnosticOpts, formatBulleted)
import GHC.Utils.Outputable (defaultSDocContext, renderWithContext)
#elif MIN_VERSION_ghc(9,4,0)
import GHC.Driver.Config.Parser (initParserOpts)
import GHC.Types.Error (diagnosticMessage, getMessages, MsgEnvelope(..))
import GHC.Utils.Error (formatBulleted)
import GHC.Utils.Outputable (defaultSDocContext)
#elif MIN_VERSION_ghc(9,2,0)
import GHC.Driver.Config (initParserOpts)
import GHC.Parser.Errors.Ppr (pprError)
#endif

#if MIN_VERSION_ghc(9,0,0)
import GHC.Data.Bag
import GHC.Driver.Session (parseDynamicFilePragma)
import GHC.Data.FastString
import GHC.Parser.Header (getOptions)
import GHC.Parser.Lexer hiding (buffer)
import GHC.Data.OrdList
import GHC.Utils.Panic (handleGhcException)
import qualified GHC.Types.SrcLoc as SrcLoc
import GHC.Data.StringBuffer hiding (len)
#else
import Bag
import DynFlags (parseDynamicFilePragma)
import FastString
import HeaderInfo (getOptions)
import Lexer hiding (buffer)
import OrdList
import Panic (handleGhcException)
import qualified SrcLoc as SrcLoc
import StringBuffer hiding (len)
#endif
#if MIN_VERSION_ghc(8,10,0)
#else
import ErrUtils hiding (ErrMsg)
#endif
#if MIN_VERSION_ghc(8,4,0)
import GHC hiding (Located, Parsed, parser)
#else
import GHC hiding (Located, parser)
#endif

import qualified Language.Haskell.GHC.HappyParser as Parse

-- | A line number in an input string.
type LineNumber   = Int

-- | A column number in an input string.
type ColumnNumber = Int

-- | An error message string.
type ErrMsg = String

-- | A location in an input string.
data StringLoc = Loc LineNumber ColumnNumber deriving (Show, Eq)

-- | Output from running a parser.
data ParseOutput a
    = Failure ErrMsg StringLoc    -- ^ Parser failed with given error message and location.
    | Parsed a                    -- ^ Parser succeeded with an output.
    | Partial a (String, String)  -- ^ Partial parser succeeded with an output.
    deriving (Eq, Show)                  -- Auxiliary strings say what part of the
                                  -- input string was used and what
                                  -- part is remaining.
                                  --
-- | Store locations along with a value.
data Located a = Located {
    line :: LineNumber, -- Where this element is located.
    unloc :: a          -- Located element.
  } deriving (Eq, Show, Functor)


data Parser a = Parser (P a)

-- Our parsers.
#if MIN_VERSION_ghc(8,4,0)
parserStatement :: Parser (Maybe (LStmt GhcPs (LHsExpr GhcPs)))
#else
parserStatement :: Parser (Maybe (LStmt RdrName (LHsExpr RdrName)))
#endif
parserStatement = Parser Parse.fullStatement

#if MIN_VERSION_ghc(8,4,0)
parserImport :: Parser (LImportDecl GhcPs)
#else
parserImport :: Parser (LImportDecl RdrName)
#endif
parserImport = Parser Parse.fullImport

#if MIN_VERSION_ghc(8,4,0)
parserDeclaration :: Parser (OrdList (LHsDecl GhcPs))
#else
parserDeclaration :: Parser (OrdList (LHsDecl RdrName))
#endif
parserDeclaration = Parser Parse.fullDeclaration

#if MIN_VERSION_ghc(8,4,0)
parserExpression :: Parser (LHsExpr GhcPs)
#else
parserExpression :: Parser (LHsExpr RdrName)
#endif
parserExpression = Parser Parse.fullExpression

#if MIN_VERSION_ghc(8,4,0)
parserTypeSignature :: Parser (SrcLoc.Located (OrdList (LHsDecl GhcPs)))
#else
parserTypeSignature :: Parser (SrcLoc.Located (OrdList (LHsDecl RdrName)))
#endif
parserTypeSignature = Parser Parse.fullTypeSignature

#if MIN_VERSION_ghc(9,6,0)
parserModule :: Parser (SrcLoc.Located (HsModule GhcPs))
#elif MIN_VERSION_ghc(9,0,0)
parserModule :: Parser (SrcLoc.Located HsModule)
#elif MIN_VERSION_ghc(8,4,0)
parserModule :: Parser (SrcLoc.Located (HsModule GhcPs))
#else
parserModule :: Parser (SrcLoc.Located (HsModule RdrName))
#endif
parserModule = Parser Parse.fullModule

-- | Run a GHC parser on a string. Return success or failure with
-- associated information for both.
runParser :: DynFlags -> Parser a -> String -> ParseOutput a
runParser flags (Parser parser) str =
  -- Create an initial parser state.
  let filename = "<interactive>"
      location = SrcLoc.mkRealSrcLoc (mkFastString filename) 1 1
      buffer = stringToStringBuffer str
#if MIN_VERSION_ghc(9,2,0)
      parseState = initParserState (initParserOpts flags) buffer location in
#else
      parseState = mkPState flags buffer location in
#endif
    -- Convert a GHC parser output into our own.
    toParseOut (unP parser parseState)
  where
    toParseOut :: ParseResult a -> ParseOutput a
#if MIN_VERSION_ghc(9,4,0)
    toParseOut (PFailed pstate) =
      let realSpan = SrcLoc.psRealSpan $ last_loc pstate
          errMsg = printErrorBag (getMessages $ errors pstate)
          ln = srcLocLine $ SrcLoc.realSrcSpanStart realSpan
          col = srcLocCol $ SrcLoc.realSrcSpanStart realSpan
        in Failure errMsg $ Loc ln col
#elif MIN_VERSION_ghc(9,2,0)
    toParseOut (PFailed pstate) =
      let realSpan = SrcLoc.psRealSpan $ last_loc pstate
          errMsg = printErrorBag (errors pstate)
          ln = srcLocLine $ SrcLoc.realSrcSpanStart realSpan
          col = srcLocCol $ SrcLoc.realSrcSpanStart realSpan
        in Failure errMsg $ Loc ln col
#elif MIN_VERSION_ghc(9,0,0)
    toParseOut (PFailed pstate) =
      let realSpan = SrcLoc.psRealSpan $ last_loc pstate
          errMsg = printErrorBag $ snd $ (messages pstate) flags
          ln = srcLocLine $ SrcLoc.realSrcSpanStart realSpan
          col = srcLocCol $ SrcLoc.realSrcSpanStart realSpan
        in Failure errMsg $ Loc ln col
#elif MIN_VERSION_ghc(8,10,0)
    toParseOut (PFailed pstate) =
      let realSpan = last_loc pstate
          errMsg = printErrorBag $ snd $ (messages pstate) flags
          ln = srcLocLine $ SrcLoc.realSrcSpanStart realSpan
          col = srcLocCol $ SrcLoc.realSrcSpanStart realSpan
        in Failure errMsg $ Loc ln col
#elif MIN_VERSION_ghc(8,4,0)
    toParseOut (PFailed _ spn@(RealSrcSpan realSpan) err) =
      let errMsg = printErrorBag $ unitBag $ mkPlainErrMsg flags spn err
          ln = srcLocLine $ SrcLoc.realSrcSpanStart realSpan
          col = srcLocCol $ SrcLoc.realSrcSpanStart realSpan
        in Failure errMsg $ Loc ln col
#else
    toParseOut (PFailed spn@(RealSrcSpan realSpan) err) =
      let errMsg = printErrorBag $ unitBag $ mkPlainErrMsg flags spn err
          ln = srcLocLine $ SrcLoc.realSrcSpanStart realSpan
          col = srcLocCol $ SrcLoc.realSrcSpanStart realSpan
        in Failure errMsg $ Loc ln col
#endif

#if MIN_VERSION_ghc(8,10,0)
#elif MIN_VERSION_ghc(8,4,0)
    toParseOut (PFailed _ spn err) =
      let errMsg = printErrorBag $ unitBag $ mkPlainErrMsg flags spn err
        in Failure errMsg $ Loc 0 0
#else
    toParseOut (PFailed spn err) =
      let errMsg = printErrorBag $ unitBag $ mkPlainErrMsg flags spn err
        in Failure errMsg $ Loc 0 0
#endif

    toParseOut (POk _parseState result) =
      Parsed result

    -- Convert the bag of errors into an error string.
#if MIN_VERSION_ghc(9,8,0)
    printErrorBag bag = joinLines . map (renderWithContext defaultSDocContext . formatBulleted . diagnosticMessage (defaultDiagnosticOpts @PsMessage) . errMsgDiagnostic) $ bagToList bag
#elif MIN_VERSION_ghc(9,6,0)
    printErrorBag bag = joinLines . map (renderWithContext defaultSDocContext . formatBulleted defaultSDocContext . diagnosticMessage (defaultDiagnosticOpts @PsMessage) . errMsgDiagnostic) $ bagToList bag
#elif MIN_VERSION_ghc(9,4,0)
    printErrorBag bag = joinLines . map (show . formatBulleted defaultSDocContext . diagnosticMessage . errMsgDiagnostic) $ bagToList bag
#elif MIN_VERSION_ghc(9,2,0)
    printErrorBag bag = joinLines . map (show . pprError) $ bagToList bag
#else
    printErrorBag bag = joinLines . map show $ bagToList bag
#endif

-- Taken from http://blog.shaynefletcher.org/2019/06/have-ghc-parsing-respect-dynamic-pragmas.html
parsePragmasIntoDynFlags :: DynFlags -> FilePath -> String -> IO (Maybe DynFlags)
parsePragmasIntoDynFlags flags filepath str =
  catchErrors $ do
#if MIN_VERSION_ghc(9,4,0)
    let opts = snd $ getOptions (initParserOpts flags) (stringToStringBuffer str) filepath
#else
    let opts = getOptions flags (stringToStringBuffer str) filepath
#endif
    (flags', _, _) <- parseDynamicFilePragma flags opts
    return $ Just flags'
  where
    catchErrors :: IO (Maybe DynFlags) -> IO (Maybe DynFlags)
    catchErrors act =
      handleGhcException reportErr (handleSourceError reportErr act)
    reportErr e = do
      putStrLn $ "error : " ++ show e
      return Nothing

-- Not the same as 'unlines', due to trailing \n
joinLines :: [String] -> String
joinLines = intercalate "\n"

-- | Split an input string into chunks based on indentation.
-- A chunk is a line and all lines immediately following that are indented
-- beyond the indentation of the first line. This parses Haskell layout
-- rules properly, and allows using multiline expressions via indentation.
--
-- Quasiquotes are allowed via a post-processing step.
layoutChunks :: String -> [Located String]
layoutChunks = joinQuasiquotes . go 1
  where
    go :: LineNumber -> String -> [Located String]
    go ln = filter (not . null . unloc) . map (fmap strip) . layoutLines ln . lines

    -- drop spaces on left and right
    strip = dropRight . dropLeft
      where
        dropLeft = dropWhile (`elem` whitespace)
        dropRight = reverse . dropWhile (`elem` whitespace) . reverse
        whitespace = " \t\n"

    layoutLines :: LineNumber -> [String] -> [Located String]
    -- Empty string case.  If there's no input, output is empty.
    layoutLines _ [] = []

    -- Use the indent of the first line to find the end of the first block.
    layoutLines lineIdx xs@(firstLine:rest) =
      let firstIndent = indentLevel firstLine
          blockEnded ln = indentLevel ln <= firstIndent in
        case findIndex blockEnded rest of
          -- If the first block doesn't end, return the whole string, since
          -- that just means the block takes up the entire string.
          Nothing -> [Located lineIdx $ intercalate "\n" xs]

          -- We found the end of the block. Split this bit out and recurse.
          Just idx ->
            let (before, after) = splitAt idx rest in
              Located lineIdx (joinLines $ firstLine:before) : go (lineIdx + idx + 1) (joinLines after)

    -- Compute indent level of a string as number of leading spaces.
    indentLevel :: String -> Int
    indentLevel (' ':str) = 1 + indentLevel str

    -- Count a tab as two spaces.
    indentLevel ('\t':str) = 2 + indentLevel str

    -- Count empty lines as a large indent level, so they're always with the previous expression.
    indentLevel "" = 100000

    indentLevel _ = 0


-- | Drop comments from Haskell source.
-- Simply gets rid of them, does not replace them in any way.
removeComments :: String -> String
removeComments = removeOneLineComments . removeMultilineComments 0 0
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
joinQuasiquotes :: [Located String] -> [Located String]
joinQuasiquotes = reverse . joinQuasiquotes' . reverse
  where
    -- This operates by finding |] and then joining blocks until a line
    -- that has some corresponding [...|. This is still a hack, but close to
    -- good enough.
    joinQuasiquotes' [] = []
    joinQuasiquotes' (block:blocks) =
      if "|]" `isInfixOf` unloc block
      then
        let (pieces, rest) = break (hasQuasiquoteStart . unloc) blocks
        in case rest of
          [] -> block : joinQuasiquotes' blocks
          startBlock:blocks' ->
            concatBlocks (block : pieces ++ [startBlock]) : joinQuasiquotes blocks'
      else block : joinQuasiquotes' blocks

    -- Combine a lit of reversed blocks into a single, non-reversed block.
    concatBlocks :: [Located String] -> Located String
    concatBlocks blocks = Located (line $ last blocks) $ joinLines $ map unloc $ reverse blocks

    -- Does this string have a [...| in it?
    hasQuasiquoteStart :: String -> Bool
    hasQuasiquoteStart str =
      case break (== '[') str of
        (_, "") -> False
        (_, _:rest) ->
          case break (== '|') rest of
            (_, "") -> False
            (chars, _) -> all isIdentChar chars

    isIdentChar :: Char -> Bool
    isIdentChar c = isAlphaNum c || c == '_' || c == '\''
