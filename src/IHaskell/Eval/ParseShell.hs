{-# LANGUAGE NoImplicitPrelude #-}

-- | This module splits a shell command line into a list of strings,
--   one for each command / filename
module IHaskell.Eval.ParseShell (parseShell) where

import           IHaskellPrelude

import           Text.ParserCombinators.Parsec

eol :: Parser Char
eol = oneOf "\n\r" <?> "end of line"

quote :: Parser Char
quote = char '\"'

-- | @manyTillEnd p end@ from normal @manyTill@ in that it appends the result of @end@
manyTillEnd :: Parser a -> Parser [a] -> Parser [a]
manyTillEnd p end = scan
  where
    scan = end <|> do
             x <- p
             xs <- scan
             return $ x : xs

manyTillEnd1 :: Parser a -> Parser [a] -> Parser [a]
manyTillEnd1 p end = do
  x <- p
  xs <- manyTillEnd p end
  return $ x : xs

unescapedChar :: Parser Char -> Parser String
unescapedChar p = try $ do
  x <- noneOf "\\"
  _ <- lookAhead p
  return [x]

quotedString :: Parser [Char]
quotedString = do
  _ <- quote <?> "expected starting quote"
  (manyTillEnd anyChar (unescapedChar quote) <* quote) <?> "unexpected in quoted String "

unquotedString :: Parser [Char]
unquotedString = manyTillEnd1 anyChar end
  where
    end = unescapedChar space
          <|> (lookAhead eol >> return [])

word :: Parser [Char]
word = quotedString <|> unquotedString <?> "word"

separator :: Parser String
separator = many1 space <?> "separator"

-- | Input must terminate in a space character (like a \n)
shellWords :: Parser [String]
shellWords = try (eof *> return []) <|> do
               x <- word
               _rest1 <- lookAhead (many anyToken)
               _ss <- separator
               _rest2 <- lookAhead (many anyToken)
               xs <- shellWords
               return $ x : xs

parseShell :: String -> Either ParseError [String]
parseShell str = parse shellWords "shell" (str ++ "\n")
