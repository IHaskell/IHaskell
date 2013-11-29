{-# LANGUAGE NoImplicitPrelude #-}
module IHaskell.Eval.Parser (
     chunkCode,
     CodeChunk(..),
     ChunkType(..),
    ) where

import ClassyPrelude hiding (liftIO, hGetContents)

import FastString
import StringBuffer
import ErrUtils
import SrcLoc
import GHC
import Bag
import Lexer

import Data.String.Utils (strip, startswith)
import Data.List.Utils (grab)
import Control.Monad.State

-- | A chunk of code with with a source span and an associated chunk type.
data CodeChunk = Chunk RealSrcSpan ChunkType

-- | Possible types of code chunks.
data ChunkType = Directive
               | Expr
               | Stmt
               | Decl
               | Import
               deriving (Eq, Show, Ord)

-- | Simple tree data structure.
data Tree a = Branch [Tree a] | Leaf a deriving Show

-- | Delimiter categorization as an opening, closing, or neither opening
-- nor closing delimiter. Used to generate trees of tokens.
data DelimType = Opening | Closing | Neither

-- | Put the given statements into a `do` block.
wrapInDoBlock :: String -> String
wrapInDoBlock codeStr =
  if null stripped
  then []
  else unlines $ "do" : map indent (lines stripped)
  where
    stripped = strip codeStr
    indent = ("    " ++)

-- | Convert a string of code into raw, uncleaned code chunks.
classifyCode :: DynFlags -> String -> Either String [CodeChunk]
classifyCode flags codeStr = groupLikeChunks . treeToChunks . tokenTree <$> runLexer flags (wrapInDoBlock codeStr)

-- | Group code chunks that are alike into one code chunk.
groupLikeChunks :: [CodeChunk] -> [CodeChunk]
groupLikeChunks chunks = map joinChunks $ groupBy sameChunkType chunks 
  where
    sameChunkType (Chunk _ firstType) (Chunk _ secondType) = firstType == secondType

    joinChunks :: [CodeChunk] -> CodeChunk
    joinChunks [chunk] = chunk
    joinChunks (Chunk firstLoc chunkType:rest) = Chunk newSpan chunkType
      where newSpan = mkRealSrcSpan (realSrcSpanStart firstLoc) (realSrcSpanEnd restChunkLoc)
            Chunk restChunkLoc _ = joinChunks rest

-- | Convert a list of tokens into a tree of tokens.
tokenTree :: [Located Lexer.Token] -> Tree (Located Lexer.Token)
tokenTree = toTree $ \x -> case x of
            -- Opening delimiters are opening curly braces and opening parentheses. 
            L _ ITvocurly -> Opening
            L _ IToparen -> Opening

            -- Closing delimiters are closing curly braces and closing parentheses. 
            L _ ITvccurly -> Closing
            L _ ITcparen -> Closing

            -- Everthing else isn't a delimiter.
            _ -> Neither

-- | Convert a list into a tree given a function that can classify each
-- element of the list as a delimiter (opening or closing) or not
-- a delimiter.
toTree :: (a -> DelimType) -- ^ Function which classifies the delimiter type of a list element.
       -> [a]             -- ^ List of tokens.
       -> Tree a          -- ^ Tree generated from tokens where each set of delimiters encodes a new level.
toTree delimType tokens = 
  case toTree' delimType 0 [[]] tokens of
    x -> Branch $ reverse x

  where
    -- Helper function for tree conversion.
    toTree' :: (a -> DelimType)   -- Convert list element to a tree.
            -> Int               -- The level of the tree which is being parsed.
            -> [[Tree a]]        -- The currently parsed branches at every level.
                                -- The first element is a list of branches
                                -- at the level currently being parsed, the
                                -- second element is the branches at the
                                -- level above, and so on.
            -> [a]               -- Remaining tokens.
            -> [Tree a]          -- Branches of the output tree.
    toTree' delimType n accum (token:rest) =
      case delimType token of
        -- If we see an opening delimiter, go down one level.
        -- Reset the parsed things at the current level to nothing, since
        -- we haven't parsed any tokens. 
        Opening -> toTree' delimType (n+1) ([] : accum) rest

        -- If we see a closing parenthesis, go back up one level.
        -- The level below just becomes a single parsed token at this level.
        Closing -> case accum of
          sublevel : currentLevel : uplevels -> toTree' delimType (n-1) levels rest
            where first = Branch $ reverse sublevel
                  currentLevelNodes = first : currentLevel
                  levels = currentLevelNodes : uplevels

        -- If we see something that isn't a delimiter, simply add it to the
        -- current level of parsed nodes.
        Neither -> case accum of
          currentLevel : uplevels -> toTree' delimType n ((Leaf token : currentLevel) : uplevels) rest

    -- Once done parsing, return the branches. We're done paring because
    -- the remaining tokens are empty and because the level of the tree is
    -- just zero (the top level).
    toTree' _ 0 (a:_) [] = a

-- | Divide the code string into chunks. Each code chunk can be evaluated
-- separately.
chunkCode :: GhcMonad m => String                        -- ^ String containing code to parse and split.
          -> m (Either String [(String, ChunkType)])    -- ^ Either an error string or a list of code chunks.
chunkCode codeString = do
  flags <- getSessionDynFlags
  let chunks = classifyCode flags codeString
  return $ case chunks of
    Right chunks -> Right $ evalState (extractDirectives chunks) $ lines codeString
    Left str -> Left str

  where
    -- Get number of lines in a source span.
    nlines :: RealSrcSpan -> Int
    nlines span = 1 + srcLocLine (realSrcSpanEnd span) - srcLocLine (realSrcSpanStart span)

    -- Extract all directives in this chunk. Convert a chunk into a list of
    -- strings and their chunk types.
    extractDirectives :: [CodeChunk] -> State [String] [(String, ChunkType)]
    extractDirectives (Chunk span chunkType:rest) = do
      spanLines <- grab $ nlines span
      next <- extractDirectives rest
      return $ catchDirectives spanLines chunkType ++ next
      where
        catchDirectives :: [String] -> ChunkType -> [(String, ChunkType)]
        catchDirectives codeLines chunkType = case break isDirective codeLines of
          -- If there are no directives...
          (allLines, []) -> [(unlines allLines, chunkType)]
          (preLines, directiveLine:postLines) -> [(unlines preLines, chunkType), (directiveLine, Directive)] ++ catchDirectives postLines chunkType

        isDirective line = startswith ":" $ strip line

    extractDirectives [] = return []

-- | only go down one level
treeToChunks :: Tree (Located Lexer.Token) -> [CodeChunk]
treeToChunks = convertToChunks (convertToChunks leafToChunk)
  where
    convertToChunks recursion (Branch subnodes) = concatMap recursion subnodes
    convertToChunks _ (Leaf value) = makeChunk value

    leafToChunk (Leaf value) = makeChunk value
    leafToChunk _ = []

    makeChunk (L (RealSrcSpan location) token) = [Chunk location $ classifyToken token]
    makeChunk _ = []

-- | Classifies a token based on what type of Haskell form it is likely to
-- be part of.  Certain tokens can mean you are in an import or in
-- a declaration.  However, you can have declarations inside expressions or
-- statements when between curly brackets. After lexing the input, lines
-- are classified based on tokens using `classifyToken` and then the
-- original input is split based on these classifications.
classifyToken :: Lexer.Token -> ChunkType
classifyToken tok = case tok of
  ITclass               -> Decl 
  ITdata                -> Decl 
  ITdefault             -> Decl 
  ITderiving            -> Decl 
  IThiding              -> Decl 
  ITimport              -> Import
  ITinfix               -> Decl 
  ITinfixl              -> Decl 
  ITinfixr              -> Decl 
  ITinstance            -> Decl 
  ITmodule              -> Decl 
  ITnewtype             -> Decl 
  ITqualified           -> Import
  ITtype                -> Decl 
  ITwhere               -> Decl 
  ITscc                 -> Decl 
  ITforeign             -> Decl 
  ITexport              -> Decl 
  ITlabel               -> Decl  -- ?
  ITdynamic             -> Decl 
  ITsafe                -> Decl 
  ITinterruptible       -> Decl 
  ITunsafe              -> Decl 
  ITstdcallconv         -> Decl 
  ITccallconv           -> Decl 
  ITcapiconv            -> Decl 
  ITprimcallconv        -> Decl 
  ITfamily              -> Decl 
  ITinline_prag {}      -> Decl 
  ITspec_prag {}        -> Decl 
  ITspec_inline_prag {} -> Decl 
  ITsource_prag {}      -> Decl 
  ITrules_prag    {}    -> Decl 
  ITwarning_prag   {}   -> Decl 
  ITdeprecated_prag  {} -> Decl 
  ITline_prag {}        -> Decl 
  ITscc_prag            -> Decl 
  ITgenerated_prag      -> Decl 
  ITcore_prag           -> Decl 
  ITunpack_prag         -> Decl 
  ITnounpack_prag       -> Decl 
  ITann_prag            -> Decl 
  ITclose_prag          -> Decl 
  IToptions_prag {}     -> Decl 
  ITinclude_prag {}     -> Decl 
  ITlanguage_prag       -> Decl 
  ITvect_prag           -> Decl  -- ?
  ITvect_scalar_prag    -> Decl 
  ITnovect_prag         -> Decl 
  ITctype               -> Decl 
  ITdcolon              -> Decl 
  ITequal               -> Decl 
  ITvbar                -> Decl  -- |
  ITdotdot              -> Expr  -- [1 .. ]
  ITcolon               -> Expr
  ITcase                -> Expr 
  ITdo                  -> Expr 
  ITelse                -> Expr 
  ITif                  -> Expr 
  ITin                  -> Expr 
  ITlet                 -> Expr 
  ITof                  -> Expr 
  ITthen                -> Expr 
  ITforall              -> Expr 
  ITmdo                 -> Expr 
  ITgroup               -> Expr  -- SQL comprehensions.
  ITby                  -> Expr 
  ITusing               -> Expr 
  ITlam                 -> Expr 
  ITlcase               -> Expr 
  ITlarrow              -> Expr 
  ITrarrow              -> Expr 
  ITat                  -> Expr 
  ITtilde               -> Expr 
  ITtildehsh            -> Expr
  ITdarrow              -> Expr 
  ITminus               -> Expr 
  ITbang                -> Expr 
  ITstar                -> Expr 
  ITdot                 -> Expr 
  ITbiglam              -> Expr
  ITocurly              -> Expr 
  ITccurly              -> Expr 
  ITvocurly             -> Expr 
  ITvccurly             -> Expr 
  ITobrack              -> Expr 
  ITopabrack            -> Expr 
  ITcpabrack            -> Expr 
  ITcbrack              -> Expr 
  IToparen              -> Expr 
  ITcparen              -> Expr 
  IToubxparen           -> Expr 
  ITcubxparen           -> Expr 
  ITsemi                -> Expr 
  ITcomma               -> Expr 
  ITunderscore          -> Expr 
  ITbackquote           -> Expr 
  ITsimpleQuote         -> Expr 
  ITvarid {}            -> Expr 
  ITconid {}            -> Expr 
  ITvarsym {}           -> Expr 
  ITconsym {}           -> Expr 
  ITqvarid {}           -> Expr 
  ITqconid {}           -> Expr 
  ITqvarsym {}          -> Expr 
  ITqconsym {}          -> Expr 
  ITprefixqvarsym {}    -> Expr 
  ITprefixqconsym {}    -> Expr 
  ITdupipvarid {}       -> Expr
  ITchar {}             -> Expr 
  ITstring {}           -> Expr 
  ITinteger{}           -> Expr 
  ITrational{}          -> Expr 
  ITprimchar {}         -> Expr 
  ITprimstring{}        -> Expr 
  ITprimint  {}         -> Expr 
  ITprimword   {}       -> Expr 
  ITprimfloat    {}     -> Expr 
  ITprimdouble  {}      -> Expr 
  ITopenExpQuote        -> Expr 
  ITopenPatQuote        -> Expr 
  ITopenDecQuote        -> Expr 
  ITopenTypQuote        -> Expr 
  ITcloseQuote          -> Expr 
  ITidEscape  {}        -> Expr
  ITparenEscape         -> Expr 
  ITtyQuote             -> Expr 
  ITquasiQuote {}       -> Expr 
  ITqQuasiQuote {}      -> Expr 
  ITproc                -> Expr 
  ITrec                 -> Expr 
  IToparenbar           -> Expr 
  ITcparenbar           -> Expr 
  ITlarrowtail          -> Expr 
  ITrarrowtail          -> Expr 
  ITLarrowtail          -> Expr 
  ITRarrowtail          -> Expr 
  ITunknown    {}       -> Expr 
  ITeof                 -> Expr 
  ITdocCommentNext {}   -> Expr 
  ITdocCommentPrev {}   -> Expr 
  ITdocCommentNamed {}  -> Expr 
  ITdocSection {}       -> Expr 
  ITdocOptions {}       -> Expr 
  ITdocOptionsOld {}    -> Expr 
  ITlineComment {}      -> Expr 
  ITblockComment {}     -> Expr 
  -- All constructors are listed above.
  -- A new keyword addition to GHC will trigger a warning here.

-- | Runs the GHC lexer on the code string. Returns an error string or
-- a list of tokens and locations for each token.
runLexer :: DynFlags -> String -> Either String [Located Token]
runLexer flags codeString = toEither (lexTokenStream buffer location flags)
  where
    -- Location displayed as the parsing location.
    filename = "<interactive>"
    initLine = 1
    initCol = 1
    location  = mkRealSrcLoc (mkFastString filename) initLine initCol
    buffer  = stringToStringBuffer codeString

    -- Convert a parse success or failure into an Either type.
    toEither (PFailed span err) = Left $ printErrorBag $ unitBag $ mkPlainErrMsg flags span err
    toEither (POk _ tokens) = Right tokens
    printErrorBag bag = unlines $ map show $ bagToList bag
