{-# LANGUAGE NoImplicitPrelude, FlexibleContexts, ViewPatterns, CPP #-}

module IHaskell.Eval.Lint (lint) where

import           IHaskellPrelude


import           Data.Maybe (mapMaybe)
import           System.IO.Unsafe (unsafePerformIO)

#if MIN_VERSION_hlint(3,1,1)
import           Language.Haskell.HLint
#elif MIN_VERSION_hlint(3,0,0)
import           Language.Haskell.HLint
import           SrcLoc (SrcSpan(..), srcSpanStartLine)
#else
import           Language.Haskell.Exts hiding (Module)
import           Language.Haskell.HLint as HLint
import           Language.Haskell.HLint3
#endif

import           IHaskell.Types
import           IHaskell.Display
import           IHaskell.Eval.Parser hiding (line)
import           StringUtils (replace)

#if MIN_VERSION_hlint(2,1,18)

#else

import           Prelude (last)
import qualified Language.Haskell.Exts.Syntax as SrcExts
import           Language.Haskell.Exts (parseFileContentsWithMode)

#endif

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

#if MIN_VERSION_hlint(2,1,18)

-- | Given code chunks, perform linting and output a displayable report on linting warnings
-- and errors.
lint :: String -> [Located CodeBlock] -> IO Display
lint code _blocks = do
  -- Initialize hlint settings
  initialized <- not <$> isEmptyMVar hlintSettings
  unless initialized $
    autoSettings' >>= putMVar hlintSettings

  -- Get hlint settings
  (flags, classify, hint) <- readMVar hlintSettings

  parsed <- parseModuleEx flags "-" (Just code)

  -- create 'suggestions'
  let ideas = case parsed of
        Left _ -> []
        Right mods -> applyHints classify hint [mods]
      suggestions = mapMaybe showIdea $ filter (not . ignoredIdea) ideas

  return $ Display $
    if null suggestions
      then []
      else [plain $ concatMap plainSuggestion suggestions, html $ htmlSuggestions suggestions]
  where
    autoSettings' = do
      (fixts, classify, hints) <- autoSettings
      let hidingIgnore = Classify Ignore "Unnecessary hiding" "" ""
      return (fixts, hidingIgnore:classify, hints)
    ignoredIdea idea = ideaSeverity idea == Ignore

#else

type ExtsModule = SrcExts.Module SrcSpanInfo

-- | Given parsed code chunks, perform linting and output a displayable report on linting warnings
-- and errors.
lint :: String -> [Located CodeBlock] -> IO Display
lint _code blocks = do
  -- Initialize hlint settings
  initialized <- not <$> isEmptyMVar hlintSettings
  unless initialized $
    autoSettings' >>= putMVar hlintSettings

  -- Get hlint settings
  (flags, classify, hint) <- readMVar hlintSettings

  -- create 'suggestions'
  let modules = mapMaybe (createModule (hseFlags flags)) blocks
      ideas = applyHints classify hint (map (\m -> (m, [])) modules)
      suggestions = mapMaybe showIdea $ filter (not . ignoredIdea) ideas

  return $ Display $
    if null suggestions
      then []
      else [plain $ concatMap plainSuggestion suggestions, html $ htmlSuggestions suggestions]
  where
    autoSettings' = do
      (fixts, classify, hints) <- autoSettings
      let hidingIgnore = Classify Ignore "Unnecessary hiding" "" ""
      return (fixts, hidingIgnore:classify, hints)
    ignoredIdea idea = ideaSeverity idea == Ignore

createModule :: ParseMode -> Located CodeBlock -> Maybe ExtsModule
createModule md (Located ln block) =
  case block of
    Expression expr  -> unparse $ exprToModule expr
    Declaration decl -> unparse $ declToModule decl
    Statement stmt   -> unparse $ stmtToModule stmt
    Import impt      -> unparse $ imptToModule impt
    Module mdl       -> unparse $ pModule mdl
    _                -> Nothing
  where
    blockStr =
      case block of
        Expression expr  -> expr
        Declaration decl -> decl
        Statement stmt   -> stmt
        Import impt      -> impt
        Module mdl       -> mdl

        -- TODO: Properly handle the other constructors
        _ -> []

    unparse :: ParseResult a -> Maybe a
    unparse (ParseOk a) = Just a
    unparse _ = Nothing

    srcSpan :: SrcSpan
    srcSpan = SrcSpan
      { srcSpanFilename = "<interactive>"
      , srcSpanStartLine = ln
      , srcSpanStartColumn = 0
      , srcSpanEndLine = ln + length (lines blockStr)
      , srcSpanEndColumn = length $ last $ lines blockStr
      }

    lcn :: SrcSpanInfo
    lcn = SrcSpanInfo srcSpan []

    moduleWithDecls :: Decl SrcSpanInfo -> ExtsModule
    moduleWithDecls decl = SrcExts.Module lcn Nothing [] [] [decl]

    pModule :: String -> ParseResult ExtsModule
    pModule = parseFileContentsWithMode md

    declToModule :: String -> ParseResult ExtsModule
    declToModule decl = moduleWithDecls <$> parseDeclWithMode md decl

    exprToModule :: String -> ParseResult ExtsModule
    exprToModule exp = moduleWithDecls <$> SpliceDecl lcn <$> parseExpWithMode md exp

    stmtToModule :: String -> ParseResult ExtsModule
    stmtToModule stmtStr =
      case parseStmtWithMode md stmtStr of
        ParseOk _       -> ParseOk $ moduleWithDecls decl
        ParseFailed a b -> ParseFailed a b
      where
        decl :: Decl SrcSpanInfo
        decl = SpliceDecl lcn expr

        expr :: Exp SrcSpanInfo
        expr = Do lcn [stmt, ret]

        stmt :: Stmt SrcSpanInfo
        ParseOk stmt = parseStmtWithMode md stmtStr

        ret :: Stmt SrcSpanInfo
        ParseOk ret = Qualifier lcn <$> parseExp lintIdent

    imptToModule :: String -> ParseResult ExtsModule
    imptToModule = parseFileContentsWithMode md

#endif

showIdea :: Idea -> Maybe LintSuggestion
showIdea idea =
  case ideaTo idea of
    Nothing -> Nothing
    Just wn ->
      Just
        Suggest
          { line = getSrcSpanStartLine $ ideaSpan idea
          , found = showSuggestion $ ideaFrom idea
          , whyNot = showSuggestion wn
          , severity = ideaSeverity idea
          , suggestion = ideaHint idea
          }
  where
    getSrcSpanStartLine span =
#if MIN_VERSION_hlint(3,1,1)
      case unpackSrcSpan span of
        Just (_, (startLine, _), _) -> startLine
        Nothing -> 1
#elif MIN_VERSION_hlint(3,0,0)
      case span of
        RealSrcSpan realSpan -> srcSpanStartLine realSpan
        UnhelpfulSpan _ -> 1
#else
      srcSpanStartLine span
#endif



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
                       , floating "left" $ styl severityClass "Found:" ++
                                           -- Things that look like this get highlighted.
                                           styleId "highlight-code" "haskell" (escapeDollar $ found suggest)
                       , floating "left" $ styl severityClass "Why Not:" ++
                                           -- Things that look like this get highlighted.
                                           styleId "highlight-code" "haskell" (escapeDollar $ whyNot suggest)
                       ]
      where
        escapeDollar = replace "$" "\\$"
        severityClass =
          case severity suggest of
            Error -> "error"
            Warning -> "warning"

            -- Should not occur
            _ -> "warning"

    styl :: String -> String -> String
    styl = printf "<div class=\"suggestion-%s\">%s</div>"

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
