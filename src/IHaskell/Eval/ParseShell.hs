
-- | This module splits a shell command line into a list of strings,
--   one for each command / filename
module IHaskell.Eval.ParseShell (parseShell) where 

import Prelude hiding (words)
import Text.ParserCombinators.Parsec hiding (manyTill)
import Control.Applicative hiding ((<|>), many, optional)

eol :: Parser Char
eol = oneOf "\n\r" <?> "end of line"

quote :: Parser Char 
quote = char '\"'

-- | @manyTill p end@ from hidden @manyTill@ in that it appends the result of @end@
manyTill :: Parser a -> Parser [a] -> Parser [a]
manyTill p end = scan
  where
    scan = end <|> do
      x <- p
      xs <- scan
      return $ x:xs

manyTill1 p end = do x <- p 
                     xs <- manyTill p end 
                     return $ x : xs

unescapedChar :: Parser Char -> Parser String 
unescapedChar p = try $ do 
  x <- noneOf "\\"
  lookAhead p
  return [x]

quotedString = do
  quote <?> "expected starting quote"
  (manyTill anyChar (unescapedChar quote) <* quote) <?> "unexpected in quoted String "

unquotedString = manyTill1 anyChar end  
  where end = unescapedChar space 
          <|> (lookAhead eol >> return [])

word = quotedString <|> unquotedString <?> "word"

separator :: Parser String
separator = many1 space <?> "separator"

-- | Input must terminate in a space character (like a \n)
words :: Parser [String]
words = try (eof *> return []) <|> do
  x <-  word 
  rest1 <- lookAhead (many anyToken)
  ss <-  separator 
  rest2 <-  lookAhead (many anyToken)
  xs <-  words 
  return $ x : xs 

parseShell :: String -> Either ParseError [String]
parseShell string = parse words "shell" (string ++ "\n")
