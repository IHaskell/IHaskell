{-|
Module      : Jupyter.IHaskell.Lint
Description : IHaskell interface to HLint
Copyright   : (c) Andrew Gibiansky, 2016
License     : MIT
Maintainer  : andrew.gibiansky@gmail.com
Stability   : stable
Portability : POSIX
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Jupyter.IHaskell.Lint (
    -- * Running @hlint@
    lintCodeBlock,
    LintSuggestion(..),
    SuggestionType(..),

    -- * Rendering @hlint@ suggestions
    renderSuggestionPlain,
    renderSuggestionHTML,
    ) where

-- Imports from 'base'
import           Control.Concurrent.MVar (putMVar, newEmptyMVar, isEmptyMVar, readMVar, MVar)
import           Control.Monad (unless)
import           Data.Maybe (mapMaybe, fromJust)
import           System.IO.Unsafe (unsafePerformIO)

-- Imports from 'text'
import           Data.Text (Text)
import qualified Data.Text as T

-- Imports from 'haskell-src-exts'
import           Language.Haskell.Exts.Annotated (parseFileContentsWithMode, parseExpWithMode,
                                                  parseModuleWithMode, parseStmtWithMode, ParseMode,
                                                  ParseResult(..), Decl(..), Module(..), Stmt(..),
                                                  SrcSpanInfo(..), SrcSpan(..))
import           Language.Haskell.Exts.Annotated.Build (doE)

-- Imports from 'hlint'
import           Language.Haskell.HLint2 (ParseFlags(..), Classify, Hint, applyHints,
                                          autoSettings, Severity(..), Idea(..))

-- Imports from 'ihaskell'
import           Jupyter.IHaskell.Parser (Loc(..), CodeBlock(..))

-- | A suggestion output by @hlint@.
data LintSuggestion =
       LintSuggestion
         { lintSuggestionLine :: Int -- ^ Line on which suggestion applies
         , lintSuggestionFound :: Text -- ^ Code before applying suggestion
         , lintSuggestionWhyNot :: Text -- ^ Code after applying suggestion
         , lintSuggestionType :: SuggestionType -- ^ Suggestion severity
         , lintSuggestion :: Text -- ^ Description of the suggestion transformation
         }
  deriving (Eq, Ord, Show)

-- | Suggestion type, indicating how serious an @hlint@ suggestion is (e.g. whether it is just
-- stylistic or semantically important).
data SuggestionType = InfoSuggestion -- ^ Default suggestion type
                    | WarningSuggestion -- ^ More serious suggestion (warning)
                    | ErrorSuggestion -- ^ Suggestion that should be an error
  deriving (Eq, Ord, Show)


-- | Global 'MVar' to store settings for Hlint once it's initialized. Since @hlint@ reads files from
-- disk, we store linter data globally after the first time it's been read.
hlintSettings :: MVar (ParseFlags, [Classify], Hint)
hlintSettings = unsafePerformIO newEmptyMVar
{-# NOINLINE hlintSettings #-}

-- | Identifier used when one is needed for proper context. This allows us to pass fully formed
-- modules even when the input is not a full module (e.g a statement), because we can use this
-- identifier assuming that the code does not have it, and then remove it in postprocessing.
lintIdent :: Text
lintIdent = "lintIdentAEjlkQeh"

-- | Given a parsed code chunk, perform linting and output a list of suggestions for the chunk.
lintCodeBlock :: Loc CodeBlock -> IO [LintSuggestion]
lintCodeBlock block = do
  -- Initialize hlint settings the first time hlint is used.
  initialized <- not <$> isEmptyMVar hlintSettings
  unless initialized $
    autoSettings >>= putMVar hlintSettings

  (flags, classify, hint) <- readMVar hlintSettings
  return $
    case createModule (hseFlags flags) block of
      Nothing   -> []
      Just modl -> mapMaybe ideaToSuggestion $ applyHints classify hint [(modl, [])]

-- | Convert from an @hlint@ 'Idea' to a @ihaskell@ 'LintSuggestion'. Doing this conversion allows
-- this module to export a stable interface, even if the underlying @hlint@ interface changes.
ideaToSuggestion :: Idea -> Maybe LintSuggestion
ideaToSuggestion idea =
  case ideaTo idea of
    Nothing -> Nothing
    Just whyNot ->
      Just
        LintSuggestion
          { lintSuggestionLine = srcSpanStartLine $ ideaSpan idea
          , lintSuggestionFound = preprocessSuggestionText $ T.pack $ ideaFrom idea
          , lintSuggestionWhyNot = preprocessSuggestionText $ T.pack whyNot
          , lintSuggestionType = severityType $ ideaSeverity idea
          , lintSuggestion = T.pack $ ideaHint idea
          }
  where
    severityType sev =
      case sev of
        Error   -> ErrorSuggestion
        Warning -> WarningSuggestion
        _       -> InfoSuggestion

    preprocessSuggestionText :: Text -> Text
    preprocessSuggestionText = T.replace lintIdent "" . dropLeadingDo

    -- Drop leading '  do ', and blank spaces following.
    dropLeadingDo :: Text -> Text
    dropLeadingDo string =
      -- If this is not a statement, we don't need to drop the do statement.
      if lintIdent `T.isInfixOf` string
        then T.unlines . clean . T.lines $ string
        else string

    -- Remove 'do' blocks. If the first line starts with a `do`... Note that hlint always indents by two
    -- spaces in its output.
    clean :: [Text] -> [Text]
    clean stmtLines =
      case stmtLines of
        [] -> []
        l:ls ->
          case T.stripPrefix "  do" l of
            Just l' ->
              case span (T.isPrefixOf "    ") ls of
                (indented, next) ->
                  l' : map (fromJust . T.stripPrefix "    ") indented ++ clean next
            Nothing -> l : clean ls

-- | Create a parsed @haskell-src-exts@ module from a code block. This module can then be processed
-- by @hlint@.
createModule :: ParseMode -> Loc CodeBlock -> Maybe (Module SrcSpanInfo)
createModule mode (Loc line block) =
  case block of
    Expression expr   -> parseResultToMaybe $ exprToModule $ T.unpack expr
    Declarations decl -> parseResultToMaybe $ declToModule $ T.unpack decl
    Statement stmt    -> parseResultToMaybe $ stmtToModule $ T.unpack stmt
    Import impt       -> parseResultToMaybe $ imptToModule $ T.unpack impt
    _                 -> Nothing
  where
    parseResultToMaybe :: ParseResult a -> Maybe a
    parseResultToMaybe (ParseOk a) = Just a
    parseResultToMaybe _ = Nothing

    srcSpan :: String -> SrcSpanInfo
    srcSpan blk =
      SrcSpanInfo
        (SrcSpan
           { srcSpanFilename = "<interactive>"
           , srcSpanStartLine = line
           , srcSpanStartColumn = 0
           , srcSpanEndLine = line + length (lines blk)
           , srcSpanEndColumn = if null blk
                                  then 0
                                  else length (lines blk)
           })
        []

    moduleWithDecls :: String -> Decl SrcSpanInfo -> (Module SrcSpanInfo)
    moduleWithDecls blk decl = Module (srcSpan blk) Nothing [] [] [decl]

    declToModule :: String -> ParseResult (Module SrcSpanInfo)
    declToModule decl = parseModuleWithMode mode decl

    exprToModule :: String -> ParseResult (Module SrcSpanInfo)
    exprToModule expr = moduleWithDecls expr <$>
                        SpliceDecl (srcSpan expr) <$>
                        parseExpWithMode mode expr

    -- Convert a statement to a module by turning it into a template haskell splice, which consists of a
    -- do block with the statement followed by a return statement.
    stmtToModule :: String -> ParseResult (Module SrcSpanInfo)
    stmtToModule stmt =
      case parseStmtWithMode mode stmt of
        ParseOk _ ->
          ParseOk $ moduleWithDecls stmt $
            SpliceDecl (srcSpan stmt) $
              let fromParseOk = fromJust . parseResultToMaybe
              in doE (srcSpan stmt)
                   [ fromParseOk (parseStmtWithMode mode stmt)
                   , fromParseOk
                       (Qualifier (srcSpan stmt) <$> parseExpWithMode mode (T.unpack lintIdent))
                   ]
        ParseFailed a b -> ParseFailed a b

    imptToModule :: String -> ParseResult (Module SrcSpanInfo)
    imptToModule = parseFileContentsWithMode mode

-- | Render a suggestion as plain text.
renderSuggestionPlain :: LintSuggestion -> Text
renderSuggestionPlain LintSuggestion { .. } =
  T.concat
    [ "Line "
    , T.pack $ show lintSuggestionLine
    , ": "
    , lintSuggestion
    , "\nFound:\n"
    , lintSuggestionFound
    , "\nWhy not:\n"
    , lintSuggestionWhyNot
    ]

-- | Render a suggestion as HTML, for display in the IHaskell notebook interface.
renderSuggestionHTML :: LintSuggestion -> Text
renderSuggestionHTML LintSuggestion { .. } =
  T.concat
    [ named lintSuggestion
    , floating "left" $ style severityClassColor "Found:"
    , lintSuggestionFound
    , floating "left" $ style severityClassColor "Why Not:"
    , lintSuggestionWhyNot
    ]
  where
    severityClassColor =
      case lintSuggestionType of
        ErrorSuggestion   -> "red"
        WarningSuggestion -> "rgb(200, 130, 0)"
        InfoSuggestion    -> "blue"

    style :: Text -> Text -> Text
    style color content =
      T.concat ["<div style=\"font-weight: bold; color: ", color, ";\">", content, "</div>"]

    named :: Text -> Text
    named content =
      T.concat ["<div style=\"font-weight: bold;\" style=\"clear:both;\">", content, "</div>"]

    floating :: Text -> Text -> Text
    floating float content =
      T.concat ["<div style=\"float: ", float, ";\">", content, "</div>"]
