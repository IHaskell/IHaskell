module IHaskell.Test.Util (lstrip, rstrip, strip, replace, ghc, shouldBeAmong) where

import           Prelude
import qualified Data.Text as T

import           Test.HUnit (assertBool)

import           GHC
import qualified GHC.Paths

-- | Drop whitespace from the left of a string.
lstrip :: String -> String
lstrip = dropWhile (`elem` (" \t\r\n" :: String))

-- | Drop whitespace from the right of a string.
rstrip :: String -> String
rstrip = reverse . lstrip . reverse

-- | Drop whitespace from both sides of a string.
strip :: String -> String
strip = rstrip . lstrip

-- | Replace all occurrences of a string with another string.
replace :: String -> String -> String -> String
replace needle replacement haystack =
  T.unpack $ T.replace (T.pack needle) (T.pack replacement) (T.pack haystack)

ghc ::  Ghc a -> IO a
ghc = runGhc (Just GHC.Paths.libdir)
--
-- | @sublist \`shouldbeAmong\` list@ sets the expectation that @sublist@ elements are 
-- among those in @list@.
shouldBeAmong :: (Show a, Eq a) => [a] -> [a] -> IO ()
sublist `shouldBeAmong` list = assertBool errorMsg $ and [x `elem` list | x <- sublist]
  where
    errorMsg = show list ++ " doesn't contain " ++ show sublist
