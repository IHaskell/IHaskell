{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}
module IHaskell.BrokenPackages (getBrokenPackages) where

import ClassyPrelude hiding ((<|>))

import Text.Parsec
import Text.Parsec.String
import Control.Applicative hiding ((<|>), many)

import Data.String.Utils (startswith)

import Shelly

data BrokenPackage = BrokenPackage {
        packageID :: String,
        brokenDeps :: [String]
      }

instance Show BrokenPackage where
  show = packageID

-- | Get a list of broken packages.
-- This function internally shells out to `ghc-pkg`, and parses the output
-- in order to determine what packages are broken.
getBrokenPackages :: IO [String]
getBrokenPackages = shelly $ do
  silently $ errExit False $ run "ghc-pkg" ["check"]
  checkOut <- lastStderr
  
  -- Get rid of extraneous things
  let rightStart str = startswith "There are problems" str || 
                       startswith "  dependency" str 
      ghcPkgOutput = unlines . filter rightStart . lines $ unpack checkOut

  return $ case parse (many check) "ghc-pkg output" ghcPkgOutput of
    Left err -> []
    Right pkgs -> map show pkgs

check :: Parser BrokenPackage
check = string "There are problems in package "
        >> BrokenPackage <$> ident <* string ":\n" <*> many1 dependency

ident :: Parser String
ident = many (alphaNum <|> oneOf "-.")

dependency :: Parser String
dependency = string "  dependency \"" *> ident <* string "\" doesn't exist\n"
