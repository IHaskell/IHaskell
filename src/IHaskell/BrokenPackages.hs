{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}
module IHaskell.BrokenPackages (getBrokenPackages) where

import ClassyPrelude hiding ((<|>))

import Text.Parsec
import Text.Parsec.String
import Control.Applicative hiding ((<|>), many)

import Shelly

data BrokenPackage = BrokenPackage {
        packageID :: String,
        brokenDeps :: [String]
      }

instance Show BrokenPackage where
  show = packageID

getBrokenPackages :: IO [String]
getBrokenPackages = shellyNoDir $ do
  silently $ errExit False $ run "ghc-pkg" ["check"]
  checkOut <- lastStderr
  
  return $ case parse (many check) "ghc-pkg output" $ unpack checkOut of
    Left err -> []
    Right pkgs -> map show pkgs

check :: Parser BrokenPackage
check = string "There are problems in package "
        >> BrokenPackage <$> ident <* string ":\n" <*> many1 dependency

ident :: Parser String
ident = many (alphaNum <|> oneOf "-.")

dependency :: Parser String
dependency = string "  dependency \"" *> ident <* string "\" doesn't exist\n"
