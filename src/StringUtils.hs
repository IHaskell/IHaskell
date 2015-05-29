module StringUtils (
    strip,
    lstrip,
    rstrip,
    replace,
    split,
    ) where

import           IHaskellPrelude
import qualified Data.Text as T

lstrip :: String -> String
lstrip = dropWhile (`elem` (" \t\r\n" :: String))

rstrip :: String -> String
rstrip = reverse . lstrip . reverse

strip :: String -> String
strip = rstrip . lstrip

replace :: String -> String -> String -> String
replace needle replacement haystack =
  T.unpack $ T.replace (T.pack needle) (T.pack replacement) (T.pack haystack)

split :: String -> String -> [String]
split delim = map T.unpack . T.splitOn (T.pack delim) . T.pack
