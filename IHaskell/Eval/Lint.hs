{-# LANGUAGE NoImplicitPrelude, QuasiQuotes, ViewPatterns #-}
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
import Data.Char

import IHaskell.Types
import IHaskell.Display
import IHaskell.IPython
import IHaskell.Eval.Parser

data LintSeverity = LintWarning | LintError deriving (Eq, Show)

data LintSuggestion
  = Suggest {
    line :: LineNumber,
    chunkNumber :: Int,
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
      fileContents = joinBlocks validBlocks
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
    joinBlocks :: [Located String] -> String
    joinBlocks = unlines . zipWith addPragma [1 .. ]

    addPragma :: Int -> Located String -> String
    addPragma i (Located desiredLine str) = linePragma desiredLine i ++ str

    linePragma = printf "{-# LINE %d, \"%d\" -#}"

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

  headerLine:foundLine:rest <- Just (lines str)

  -- Expect the line after the header to have 'Found' in it.
  guard ("Found:" `isInfixOf` foundLine)

  -- Expect something like:
  -- ".hlintFile.hs:1:19: Warning: Redundant bracket"
  -- ==> 
  -- [".hlintFile.hs","1","19"," Warning"," Redundant bracket"]
  [readMay -> Just chunkN,
   readMay -> Just lineNum, _col, severity, name] <- Just (split ":" headerLine)

  (before, _:after) <- Just (break ("Why not:" `isInfixOf`) rest)
  return Suggest {
    line = lineNum,
    chunkNumber = chunkN,
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

    -- Statements go in a 'do' block bound to an identifier.
    --
    -- a cell can contain:
    -- > x <- readFile "foo"
    -- so add a return () to avoid a Parse error: Last statement in
    -- a do-block must be an expression
    --
    -- one place this goes wrong is when the chunk is:
    --
    -- > do
    -- >  {- a comment that has to -} let x = 1
    -- >   {- count as whitespace -}      y = 2
    -- >                              return (x+y) 
    Statement stmt ->
        let expandTabs = replace "\t" "        " 
            nLeading = maybe 1 ((+1) . length . takeWhile isSpace)
                    $ listToMaybe
                    $ filter (not . all isSpace)
                            (lines (expandTabs stmt))
            finalReturn = replicate nLeading ' ' ++ "return ()"
        in intercalate ("\n ") ("do" : lines stmt ++ [finalReturn])

    -- Modules, declarations, and type signatures are fine as is.
    Module mod -> mod
    Declaration decl -> decl
    TypeSignature sig -> sig
    Import imp -> imp

    -- Output nothing for directives or parse errors.
    Directive {} -> ""
    ParseError {} -> ""
