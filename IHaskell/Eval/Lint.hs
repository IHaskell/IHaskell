{-# LANGUAGE NoImplicitPrelude, QuasiQuotes #-}
module IHaskell.Eval.Lint ( 
  lint
  ) where

import Data.String.Utils (replace, startswith, strip, split)
import Prelude (head, tail)
import Language.Haskell.HLint as HLint
import ClassyPrelude
import Control.Monad
import Data.List (findIndex)
import Text.Printf
import Data.String.Here

import IHaskell.Types
import IHaskell.Display
import IHaskell.IPython
import IHaskell.Eval.Parser

data LintSeverity = LintWarning | LintError deriving (Eq, Show)

data LintSuggestion
  = Suggest {
    line :: LineNumber,
    found :: String,
    whyNot :: String,
    severity :: LintSeverity,
    suggestion :: String
  }
  deriving (Eq, Show)

-- | Identifier used when one is needed for proper context.
lintIdent :: String
lintIdent = "lintIdentAEjlkQeh"

-- | Given parsed code chunks, perform linting and output a displayable
-- report on linting warnings and errors.
lint :: [Located CodeBlock] -> IO [DisplayData]
lint blocks = do
  let validBlocks = map makeValid blocks
      fileContents = joinBlocks 1 validBlocks
  -- Get a temporarly location to store this file.
  ihaskellDir <- getIHaskellDir
  let filename = ihaskellDir ++ "/.hlintFile.hs"

  writeFile (fromString filename) fileContents
  suggestions <- catMaybes <$> map parseSuggestion <$> hlint [filename, "--quiet"]
  return $
    if null suggestions
    then []
    else
      [plain $ concatMap plainSuggestion suggestions, html $ htmlSuggestions suggestions]
  where
    -- Join together multiple valid file blocks into a single file.
    -- However, join them with padding so that the line numbers are
    -- correct.
    joinBlocks :: LineNumber -> [Located String] -> String
    joinBlocks nextLine (Located desiredLine str:strs) =
      -- Place padding to shift the line number appropriately.
      replicate (desiredLine - nextLine) '\n' ++
      str ++ "\n" ++
      joinBlocks (desiredLine + nlines str) strs
    joinBlocks _ [] = ""

    nlines = length . lines

plainSuggestion :: LintSuggestion -> String
plainSuggestion suggest = 
  printf "Line %d: %s\nFound:\n%s\nWhy not:\n%s"
    (line suggest)
    (suggestion suggest)
    (found suggest)
    (whyNot suggest)

htmlSuggestions :: [LintSuggestion] -> String
htmlSuggestions = concatMap toHtml 
  where
    toHtml :: LintSuggestion -> String
    toHtml suggest = concat 
      [
      named $ suggestion suggest,
      floating "left" $ style severityClass "Found:" ++
             -- Things that look like this get highlighted.
             styleId "highlight-code" "haskell" (found suggest),
      floating "left" $ style severityClass "Why Not:" ++
             -- Things that look like this get highlighted.
             styleId "highlight-code" "haskell" (whyNot suggest)
      ]

      where
        severityClass = case severity suggest of
          LintWarning -> "warning"
          LintError -> "error"

    style :: String -> String -> String
    style cls thing = [i| <div class="suggestion-${cls}">${thing}</div> |]

    named :: String -> String
    named thing = [i| <div class="suggestion-name" style="clear:both;">${thing}</div> |]

    styleId :: String -> String -> String -> String
    styleId cls id thing = [i| <div class="${cls}" id="${id}">${thing}</div> |]
    
    floating :: String -> String -> String
    floating dir thing = [i| <div class="suggestion-row" style="float: ${dir};">${thing}</div> |]

-- | Parse a suggestion from Hlint. The suggestions look like this:
--   .ihaskell/.hlintFile.hs:1:19: Warning: Redundant bracket
--   Found:
--     ((3))
--   Why not:
--     (3)
-- We extract all the necessary fields and store them.
-- If parsing fails, return Nothing.
parseSuggestion :: Suggestion -> Maybe LintSuggestion
parseSuggestion suggestion = do
  let str = showSuggestion suggestion
      severity = suggestionSeverity suggestion
  guard (severity /= HLint.Ignore)
  let lintSeverity = case severity of
        Warning -> LintWarning
        Error -> LintError

  let suggestionLines = lines str
  -- Expect a header line, a "Found" line, and a "Why not" line.
  guard (length suggestionLines > 3)

  -- Expect the line after the header to have 'Found' in it.
  let headerLine:foundLine:rest = suggestionLines
  guard ("Found:" `isInfixOf` foundLine)

  -- Expect something like:
  -- ".hlintFile.hs:1:19: Warning: Redundant bracket"
  let headerPieces = split ":" headerLine
  guard (length headerPieces == 5)
  let [file, line, col, severity, name] = headerPieces

  whyIndex <- findIndex ("Why not:" `isInfixOf`) rest
  let (before, _:after) = splitAt whyIndex rest
  lineNum <- readMay line
  return Suggest {
    line = lineNum,
    found = unlines before,
    whyNot = unlines after,
    suggestion = name,
    severity = lintSeverity
  }
  where
    showSuggestion = 
      replace (lintIdent ++ "=") "" .
      replace (lintIdent ++ "$do ") "" .
      replace (replicate (length lintIdent + length " $ do ") ' ' ++ lintIdent) "" .
      replace (" in " ++ lintIdent) "" .
      show


-- | Convert a code chunk into something that could go into a file.
-- The line number on the output is the same as on the input.
makeValid :: Located CodeBlock -> Located String
makeValid (Located line block) = Located line $
  case block of
    -- Expressions need to be bound to some identifier.
    Expression expr -> lintIdent ++ "=" ++ expr

    -- Statements need to go in a 'do' block bound to an identifier.
    -- It must also end with a 'return'.
    Statement stmt -> 
      -- Let's must be handled specially, because we can't have layout
      -- inside non-layout. For instance, this is illegal:
      --   a = do { let x = 3; return 3 }
      --   because it should be
      --   a = do { let {x = 3}; return 3 }
      -- Thus, we rely on template haskell and instead turn it into an
      -- expression via let x = blah 'in blah'.
      if startswith "let" $ strip stmt
      then stmt ++ " in " ++ lintIdent
      else 
        -- We take advantage of the fact that naked expressions at toplevel
        -- are allowed by Template Haskell, and output them to a file.
        let prefix = lintIdent ++ " $ do "
            first:rest = split "\n" stmt
            indent = replicate (length prefix) ' '
            fixedLines = first : map (indent ++) rest
            extraReturnLine = [indent ++ lintIdent]
            code = intercalate "\n" (fixedLines ++ extraReturnLine) in
        prefix ++ code

    -- Modules, declarations, and type signatures are fine as is.
    Module mod -> mod
    Declaration decl -> decl
    TypeSignature sig -> sig
    Import imp -> imp

    -- Output nothing for directives or parse errors.
    Directive {} -> ""
    ParseError {} -> ""
