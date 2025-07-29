{-# LANGUAGE NoImplicitPrelude #-}
module StringUtils (
    strip,
    lstrip,
    rstrip,
    replace,
    split,
    splitFirst,
    ) where

import           IHaskellPrelude
import           Data.Char
import qualified Data.List as L
import qualified Data.Text as T
import           Data.List.Split (splitOn)
import qualified Data.List.Split as Split

lstrip :: String -> String
lstrip = dropWhile isSpace

rstrip :: String -> String
rstrip = L.dropWhileEnd isSpace

strip :: String -> String
strip = rstrip . lstrip

replace :: String -> String -> String -> String
replace needle replacement haystack =
  T.unpack $ T.replace (T.pack needle) (T.pack replacement) (T.pack haystack)

split :: String -> String -> [String]
split = splitOn

splitFirst :: String -> String -> [String]
splitFirst delim str = let
  (head:_:tail) = Split.split (Split.onSublist delim) str
  in [head, concat tail]
