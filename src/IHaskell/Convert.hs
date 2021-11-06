{-# LANGUAGE NoImplicitPrelude #-}

-- | Description : mostly reversible conversion between ipynb and lhs
module IHaskell.Convert (convert) where

import           IHaskellPrelude

import           Control.Monad (unless, when)
import           Data.Functor.Identity (Identity(Identity))
import           IHaskell.Convert.Args (ConvertSpec(..), fromJustConvertSpec, toConvertSpec)
import           IHaskell.Convert.IpynbToLhs (ipynbToLhs)
import           IHaskell.Convert.LhsToIpynb (lhsToIpynb)
import           IHaskell.Flags (Argument)
import           System.Directory (doesFileExist)
import           Text.Printf (printf)

-- | used by @IHaskell convert@
convert :: [Argument] -> IO ()
convert args =
  case fromJustConvertSpec (toConvertSpec args) of
    ConvertSpec
      { convertToIpynb = Identity toIpynb
      , convertInput = Identity inputFile
      , convertOutput = Identity outputFile
      , convertLhsStyle = Identity lhsStyle
      , convertOverwriteFiles = force
      }
      | toIpynb -> do
          unless force (failIfExists outputFile)
          lhsToIpynb lhsStyle inputFile outputFile
      | otherwise -> do
          unless force (failIfExists outputFile)
          ipynbToLhs lhsStyle inputFile outputFile

-- | Call fail when the named file already exists.
failIfExists :: FilePath -> IO ()
failIfExists file = do
  exists <- doesFileExist file
  when exists $ fail $
    printf "File %s already exists. To force supply --force." file
