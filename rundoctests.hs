{-# LANGUAGE OverloadedStrings #-}
import System.Process
import Test.DocTest
import System.Environment
import Data.String.Utils

-- | tests that all the >>> comments are followed by correct output. Easiest is to
--
-- > cabal test
--
-- or
--
-- > runghc examples/rundoctests.hs
--
-- or
--
-- > runghc examples/rundoctests.hs Data/HList/File1.hs Data/HList/File2.hs
--
-- you need Cabal >= 1.18 since that's around when cabal repl got added.
main :: IO ()
main = do
    -- Get files to run on.
    args <- getArgs

    -- Get flags via cabal repl.
    let cabalCmds = unlines [":show packages", ":show language"]
        cabalOpts = ["repl","--ghc-options","-v0 -w"]
    options <- readProcess "cabal" cabalOpts cabalCmds
    let extraFlags = ["-fobject-code", "-XNoImplicitPrelude"]
        flags = words (unlines $ filter (startswith "-" . strip) $ lines options) ++ extraFlags

    let files = case args of
            [] -> ["Main.hs"]
            _ -> args
    putStrLn "Testing:\n--------"
    mapM_ putStrLn files
    putStr "\n"

    doctest $ "-i.": "-idist/build/autogen":
             "-optP-include":
             "-optPdist/build/autogen/cabal_macros.h" :
             "-Idist/build/autogen" : "-w":
             files ++ flags
