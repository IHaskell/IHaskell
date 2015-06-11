{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, FlexibleContexts #-}

module IHaskell.BrokenPackages (getBrokenPackages) where

import           IHaskellPrelude
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as CBS

import           Text.Parsec
import           Text.Parsec.String
import           Control.Applicative hiding ((<|>), many)

import           Shelly

data BrokenPackage = BrokenPackage { packageID :: String, brokenDeps :: [String] }

instance Show BrokenPackage where
  show = packageID

-- | Get a list of broken packages. This function internally shells out to `ghc-pkg`, and parses the
-- output in order to determine what packages are broken.
getBrokenPackages :: IO [String]
getBrokenPackages = shelly $ do
  silently $ errExit False $ run "ghc-pkg" ["check"]
  checkOut <- lastStderr

  -- Get rid of extraneous things
  let rightStart str = "There are problems" `isPrefixOf` str ||
                       "  dependency" `isPrefixOf` str
      ghcPkgOutput = unlines . filter rightStart . lines $ T.unpack checkOut

  return $
    case parse (many check) "ghc-pkg output" ghcPkgOutput of
      Left err   -> []
      Right pkgs -> map show pkgs

check :: Parser BrokenPackage
check = string "There are problems in package "
        >> BrokenPackage <$> ident <* string ":\n" <*> many1 dependency

ident :: Parser String
ident = many (alphaNum <|> oneOf "-.")

dependency :: Parser String
dependency = string "  dependency \"" *> ident <* string "\" doesn't exist\n"
