{-# LANGUAGE NoImplicitPrelude, FlexibleContexts, ViewPatterns #-}

module IHaskell.Eval.Lint (lint) where

import           IHaskellPrelude
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as CBS

import           Prelude (head, tail, last)
import           Control.Monad
import           Data.List (findIndex)
import           Data.Char
import           Data.Monoid
import           Data.Maybe (mapMaybe)
import           System.IO.Unsafe (unsafePerformIO)

import           Language.Haskell.Exts.Annotated.Syntax hiding (Module)
import qualified Language.Haskell.Exts.Annotated.Syntax as SrcExts
import           Language.Haskell.Exts.Annotated (parseFileContentsWithMode)
import           Language.Haskell.Exts.Annotated.Build (doE)
import           Language.Haskell.Exts.Annotated hiding (Module)
import           Language.Haskell.Exts.SrcLoc

import           Language.Haskell.HLint as HLint
import           Language.Haskell.HLint2

import           IHaskell.Types
import           IHaskell.Display
import           IHaskell.IPython
import           IHaskell.Eval.Parser hiding (line)
import           StringUtils (replace)

type ExtsModule = SrcExts.Module SrcSpanInfo

data LintSuggestion =
       Suggest
         { line :: LineNumber
         , found :: String
         , whyNot :: String
         , severity :: Severity
         , suggestion :: String
         }
  deriving (Eq, Show)

-- Store settings for Hlint once it's initialized.
{-# NOINLINE hlintSettings #-}
hlintSettings :: MVar (ParseFlags, [Classify], Hint)
hlintSettings = unsafePerformIO newEmptyMVar

-- | Identifier used when one is needed for proper context.
lintIdent :: String
lintIdent = "lintIdentAEjlkQeh"

-- | Given parsed code chunks, perform linting and output a displayable report on linting warnings
-- and errors.
lint :: [Located CodeBlock] -> IO Display
lint blocks = do
  -- Initialize hlint settings
  initialized <- not <$> isEmptyMVar hlintSettings
  unless initialized $
    autoSettings >>= putMVar hlintSettings

  -- Get hlint settings
  (flags, classify, hint) <- readMVar hlintSettings
  let mode = hseFlags flags

  -- create 'suggestions'
  let modules = mapMaybe (createModule mode) blocks
      ideas = applyHints classify hint (map (\m -> (m, [])) modules)
      suggestions = mapMaybe showIdea ideas

  return $ Display $
    if null suggestions
      then []
      else [plain $ concatMap plainSuggestion suggestions, html $ htmlSuggestions suggestions]

showIdea :: Idea -> Maybe LintSuggestion
showIdea idea =
  case ideaTo idea of
    Nothing -> Nothing
    Just whyNot ->
      Just
        Suggest
          { line = srcSpanStartLine $ ideaSpan idea
          , found = showSuggestion $ ideaFrom idea
          , whyNot = showSuggestion whyNot
          , severity = ideaSeverity idea
          , suggestion = ideaHint idea
          }

createModule :: ParseMode -> Located CodeBlock -> Maybe ExtsModule
createModule mode (Located line block) =
  case block of
    Expression expr  -> unparse $ exprToModule expr
    Declaration decl -> unparse $ declToModule decl
    Statement stmt   -> unparse $ stmtToModule stmt
    Import impt      -> unparse $ imptToModule impt
    Module mod       -> unparse $ parseModule mod
    _                -> Nothing
  where
    blockStr =
      case block of
        Expression expr  -> expr
        Declaration decl -> decl
        Statement stmt   -> stmt
        Import impt      -> impt
        Module mod       -> mod

    unparse :: ParseResult a -> Maybe a
    unparse (ParseOk a) = Just a
    unparse _ = Nothing

    srcSpan :: SrcSpan
    srcSpan = SrcSpan
      { srcSpanFilename = "<interactive>"
      , srcSpanStartLine = line
      , srcSpanStartColumn = 0
      , srcSpanEndLine = line + length (lines blockStr)
      , srcSpanEndColumn = length $ last $ lines blockStr
      }

    loc :: SrcSpanInfo
    loc = SrcSpanInfo srcSpan []

    moduleWithDecls :: Decl SrcSpanInfo -> ExtsModule
    moduleWithDecls decl = SrcExts.Module loc Nothing [] [] [decl]

    parseModule :: String -> ParseResult ExtsModule
    parseModule = parseFileContentsWithMode mode

    declToModule :: String -> ParseResult ExtsModule
    declToModule decl = moduleWithDecls <$> parseDeclWithMode mode decl

    exprToModule :: String -> ParseResult ExtsModule
    exprToModule exp = moduleWithDecls <$> SpliceDecl loc <$> parseExpWithMode mode exp

    stmtToModule :: String -> ParseResult ExtsModule
    stmtToModule stmtStr =
      case parseStmtWithMode mode stmtStr of
        ParseOk stmt    -> ParseOk mod
        ParseFailed a b -> ParseFailed a b
      where
        mod = moduleWithDecls decl

        decl :: Decl SrcSpanInfo
        decl = SpliceDecl loc expr

        expr :: Exp SrcSpanInfo
        expr = doE loc [stmt, ret]

        stmt :: Stmt SrcSpanInfo
        ParseOk stmt = parseStmtWithMode mode stmtStr

        ret :: Stmt SrcSpanInfo
        ParseOk ret = Qualifier loc <$> parseExp lintIdent

    imptToModule :: String -> ParseResult ExtsModule
    imptToModule = parseFileContentsWithMode mode

plainSuggestion :: LintSuggestion -> String
plainSuggestion suggest =
  printf "Line %d: %s\nFound:\n%s\nWhy not:\n%s" (line suggest) (suggestion suggest) (found suggest)
    (whyNot suggest)

htmlSuggestions :: [LintSuggestion] -> String
htmlSuggestions = concatMap toHtml
  where
    toHtml :: LintSuggestion -> String
    toHtml suggest = concat
                       [ named $ suggestion suggest
                       , floating "left" $ style severityClass "Found:" ++
                                           -- Things that look like this get highlighted.
                                           styleId "highlight-code" "haskell" (found suggest)
                       , floating "left" $ style severityClass "Why Not:" ++
                                           -- Things that look like this get highlighted.
                                           styleId "highlight-code" "haskell" (whyNot suggest)
                       ]
      where
        severityClass =
          case severity suggest of
            Error -> "error"
            Warning -> "warning"

            -- Should not occur
            _ -> "warning"

    style :: String -> String -> String
    style = printf "<div class=\"suggestion-%s\">%s</div>"

    named :: String -> String
    named = printf "<div class=\"suggestion-name\" style=\"clear:both;\">%s</div>"

    styleId :: String -> String -> String -> String
    styleId = printf "<div class=\"%s\" id=\"%s\">%s</div>"

    floating :: String -> String -> String
    floating = printf "<div class=\"suggestion-row\" style=\"float: %s;\">%s</div>"

showSuggestion :: String -> String
showSuggestion = remove lintIdent . dropDo
  where
    remove str = replace str ""

    -- Drop leading '  do ', and blank spaces following.
    dropDo :: String -> String
    dropDo string =
      -- If this is not a statement, we don't need to drop the do statement.
      if lintIdent `isInfixOf` string
        then unlines . clean . lines $ string
        else string

    clean :: [String] -> [String]
    -- If the first line starts with a `do`... Note that hlint always indents by two spaces in its
    -- output.
    clean ((stripPrefix "  do " -> Just a):as) =
      -- Take all indented lines and unindent them.
      let unindented = catMaybes $ takeWhile isJust $ map (stripPrefix "     ") as
          fullDo = a : unindented
          afterDo = drop (length unindented) as
      in fullDo ++ clean afterDo

    -- Ignore other list elements - just proceed onwards.
    clean (x:xs) = x : clean xs
    clean [] = []
