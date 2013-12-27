module Language.Haskell.GHC.Parser (
  -- Parser handling
  runParser,
  LineNumber,
  ColumnNumber,
  ErrMsg,
  StringLoc(..),
  ParseOutput(..),
  Parser,

  -- Different parsers
  parserStatement,
  parserImport,
  parserDeclaration,
  parserTypeSignature,
  parserModule,
  parserExpression,
  partialStatement,
  partialImport,
  partialDeclaration,
  partialTypeSignature,
  partialModule,
  partialExpression,
  ) where

import Data.List (intercalate)

import Bag
import ErrUtils hiding (ErrMsg)
import FastString
import GHC
import Lexer
import OrdList
import Outputable hiding ((<>))
import SrcLoc
import StringBuffer

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

data ParserType = FullParser | PartialParser
data Parser a = Parser ParserType (P a)

-- Our parsers.
parserStatement      = Parser FullParser    Parse.fullStatement
parserImport         = Parser FullParser    Parse.fullImport
parserDeclaration    = Parser FullParser    Parse.fullDeclaration
parserExpression     = Parser FullParser    Parse.fullExpression
parserTypeSignature  = Parser FullParser    Parse.fullTypeSignature
parserModule         = Parser FullParser    Parse.fullModule
partialStatement     = Parser PartialParser Parse.partialStatement
partialImport        = Parser PartialParser Parse.partialImport
partialDeclaration   = Parser PartialParser Parse.partialDeclaration
partialExpression    = Parser PartialParser Parse.partialExpression
partialTypeSignature = Parser PartialParser Parse.partialTypeSignature
partialModule        = Parser PartialParser Parse.partialModule

-- | Run a GHC parser on a string. Return success or failure with
-- associated information for both.
runParser :: DynFlags -> Parser a -> String -> ParseOutput a
runParser flags (Parser parserType parser) str =
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
        case parserType of
          PartialParser -> Partial result (before, after)
          FullParser -> Parsed result

    -- Convert the bag of errors into an error string.
    printErrorBag bag = joinLines . map show $ bagToList bag

-- | Split a string at a given line and column. The column is included in
-- the second part of the split.
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
joinLines :: [String] -> String
joinLines = intercalate "\n"
