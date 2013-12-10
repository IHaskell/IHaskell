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
import Prelude (init, last)

import FastString
import StringBuffer
import ErrUtils
import SrcLoc
import GHC
import GhcMonad (liftIO)
import Bag
import Outputable hiding ((<>))
import Lexer
import OrdList

import IHaskell.GHC.HaskellParser

import Debug.Trace

type LineNumber = Int
type ColumnNumber = Int

data CodeBlock
  = Expression String
  | Declaration String
  | Statement String
  | Import String
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
-- >>> test "let x = 3 in x + 3"
-- [Expression "let x = 3 in x + 3"]
--
-- >>> test "3\n:t expr"
-- [Expression "3",Directive GetType "expr"]
--
-- >>> test "3\nlet x = expr"
-- [Expression "3",Statement "let x = expr"]
--
-- >>> test "y <- do print 'no'\nlet x = expr"
-- [Statement "y <- do print 'no'",Statement "let x = expr"]
--
-- >>> test "y <- do print 'no'\nlet x = expr"
-- [Statement "y <- print 'no'",Statement "let x = expr"]
--
-- >>> test "print yes\nprint no"
-- [Expression "print yes",Statement "print no"]


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

parseCell' :: GhcMonad m => String -> Int  -> m [CodeBlock]
parseCell' code startLine = do
    flags <- getSessionDynFlags
    let parseResults = map tryParser (parsers flags)
    case rights parseResults of
      [] -> return [ParseError startLine 0 "Failed"]
      (result, used, remaining):_ -> do
        remainResult <- parseCell' remaining $ startLine + length (lines used)
        return $ result : if null (strip remaining)
                          then []
                          else remainResult

  where
    tryParser :: (String -> CodeBlock, String -> (Either String String, String, String)) -> Either String (CodeBlock, String, String)
    tryParser (blockType, parser) = case parser code of
      (Left err, _, _) -> Left err
      (Right res, used, remaining) -> Right (blockType res, used, remaining)
    parsers flags =
      [ (Import,      strParser flags partialImport)
      , (Expression,  strParser flags partialExpression)
      , (Statement,   strParser flags partialStatement)
      , (Declaration, lstParser flags partialDeclaration)
      ]

    lstParser :: Outputable a => DynFlags -> P (OrdList a) -> String -> (Either String String, String, String)
    lstParser flags parser code =
      case runParser flags parser code of
        Left err -> (Left err, code, "")
        Right (out, used, remainingCode) -> (Right . showSDoc flags . ppr . fromOL $ out, used, remainingCode)

    strParser :: Outputable a => DynFlags -> P a -> String -> (Either String String, String, String)
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

-- | Run a GHC parser on a string.
runParser :: DynFlags -> P a -> String -> Either String (a, String, String)
runParser dflags parser str = toEither (unP parser (mkPState dflags buffer location))
    where
       filename = "<interactive>"
       location = mkRealSrcLoc (mkFastString filename) 1 1
       buffer = stringToStringBuffer str

       toEither (PFailed span err) = Left $ printErrorBag $ unitBag $ mkPlainErrMsg dflags span err
       toEither (POk parseState result) = 
         let parseEnd = loc parseState
             endLine = srcLocLine parseEnd
             endCol = srcLocCol parseEnd
             (before, after) = splitAtLoc endLine endCol str in
           Right (result, before, after)

       -- Convert the bag of errors into an error string.
       printErrorBag bag = unlines . map show $ bagToList bag

-- | Split a string at a given line and column.
--
-- >>> splitAtLoc 2 3 "abc\ndefghi\nxyz\n123"
-- ("abc\ndef","ghi\nxyz\n123")
--
-- >>> splitAtLoc 2 1 "abc"
-- ("abc","")
splitAtLoc :: LineNumber -> ColumnNumber -> String -> (String, String)
splitAtLoc line col string = 
  if line > length (lines string)
  then (string, "")
  else (before, after)
  where
    (beforeLines, afterLines) = splitAt line $ lines string
    theLine = last beforeLines
    (beforeChars, afterChars) = splitAt col theLine

    -- Not the same as 'unlines', due to trailing \n
    joinLines = intercalate "\n"

    before = joinLines (init beforeLines) ++ '\n' : beforeChars
    after = afterChars ++ '\n' : joinLines afterLines
