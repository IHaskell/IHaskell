
-- | This module splits a shell command line into a list of strings,
--   one for each command / filename
module IHaskell.Eval.ParseShell (parseShell) where 

import Prelude hiding (words)
import Text.ParserCombinators.Parsec hiding (manyTill)
import Control.Applicative hiding ((<|>), many, optional)

import Debug.Trace

import Test.Hspec
import Test.Hspec.HUnit
import Test.HUnit (assertBool, assertFailure)

debug = False

trace' x a = if debug then trace x a else a

eol :: Parser Char
eol = do x <- oneOf "\n\r"
         return x
    <?> "end of line"
 

quote :: Parser Char 
quote    = char '\"'

manyTill :: Parser a -> Parser [a] -> Parser [a]
-- | @manyTill p end@ from hidden @manyTill@ in that it appends the result of @end@
manyTill p end      = do scan
                    where
                      scan  = do{ x <- end; return x }
                            <|>
                              do{ x <- p; xs <- scan; return $ x:xs }

manyTill1 p end = do x <- p 
                     xs <- manyTill p end 
                     return $ x : xs

unescapedChar :: Parser Char -> Parser String 
unescapedChar p = try $ do x <- noneOf ['\\'] 
                           lookAhead p
                           return $ x : []

quotedString = (trace' "in quotedString") 
               (do quote <?> "expected starting quote"
                   manyTill anyChar end <* quote)
            <?> "unexpected in quoted String "
        where end = unescapedChar quote

unquotedString = (trace' "in unquotedString") 
                 manyTill1 anyChar end  
        where end = unescapedChar space 
                <|> do x <- lookAhead eol 
                       return []

word = quotedString <|> unquotedString <?> "word"

separator :: Parser String
separator = many1 space <?> "separator"

words :: Parser [String ]
-- | Input must terminate in a space character (like a \n)
words = try (eof *> return []) <|>
        do x <-  word 
           rest1 <- trace' ("word: " ++ show x) lookAhead (many anyToken)
           ss <- trace' ("rest1: " ++ show rest1) separator 
           rest2 <- trace' ("spaces: " ++ show ss) lookAhead (many anyToken)
           xs <- trace' ("rest2: " ++ show rest2) words 
           return $ x : xs 

parseShell :: String -> Either ParseError [String]
parseShell string = parse words "shell" (string ++ "\n")


