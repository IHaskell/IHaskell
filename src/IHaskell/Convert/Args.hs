{-# LANGUAGE NoImplicitPrelude #-}

-- | Description: interpret flags parsed by "IHaskell.Flags"
module IHaskell.Convert.Args (ConvertSpec(..), fromJustConvertSpec, toConvertSpec) where

import           IHaskellPrelude
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as CBS

import           Control.Applicative ((<$>))
import           Control.Monad.Identity (Identity(Identity))
import           Data.Char (toLower)
import           Data.List (partition)
import           Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy as T (pack, Text)
import           IHaskell.Flags (Argument(..), LhsStyle, lhsStyleBird, NotebookFormat(..))
import           System.FilePath ((<.>), dropExtension, takeExtension)
import           Text.Printf (printf)

-- | ConvertSpec is the accumulator for command line arguments
data ConvertSpec f =
       ConvertSpec
         { convertToIpynb :: f Bool
         , convertInput :: f FilePath
         , convertOutput :: f FilePath
         , convertLhsStyle :: f (LhsStyle LT.Text)
         , convertOverwriteFiles :: Bool
         }

-- | Convert a possibly-incomplete specification for what to convert into one which can be executed.
-- Calls error when data is missing.
fromJustConvertSpec :: ConvertSpec Maybe -> ConvertSpec Identity
fromJustConvertSpec convertSpec = convertSpec
  { convertToIpynb = Identity toIpynb
  , convertInput = Identity inputFile
  , convertOutput = Identity outputFile
  , convertLhsStyle = Identity $ fromMaybe (LT.pack <$> lhsStyleBird) (convertLhsStyle convertSpec)
  }
  where
    toIpynb = fromMaybe (error "Error: direction for conversion unknown")
                (convertToIpynb convertSpec)
    (inputFile, outputFile) =
      case (convertInput convertSpec, convertOutput convertSpec) of
        (Nothing, Nothing) -> error "Error: no files specified for conversion"
        (Just i, Nothing)
          | toIpynb -> (i, dropExtension i <.> "ipynb")
          | otherwise -> (i, dropExtension i <.> "lhs")
        (Nothing, Just o)
          | toIpynb -> (dropExtension o <.> "lhs", o)
          | otherwise -> (dropExtension o <.> "ipynb", o)
        (Just i, Just o) -> (i, o)

-- | Does this @Argument@ explicitly request a file format?
isFormatSpec :: Argument -> Bool
isFormatSpec (ConvertToFormat _) = True
isFormatSpec (ConvertFromFormat _) = True
isFormatSpec _ = False

toConvertSpec :: [Argument] -> ConvertSpec Maybe
toConvertSpec args = mergeArgs otherArgs (mergeArgs formatSpecArgs initialConvertSpec)
  where
    (formatSpecArgs, otherArgs) = partition isFormatSpec args
    initialConvertSpec = ConvertSpec Nothing Nothing Nothing Nothing False

mergeArgs :: [Argument] -> ConvertSpec Maybe -> ConvertSpec Maybe
mergeArgs args initialConvertSpec = foldr mergeArg initialConvertSpec args

mergeArg :: Argument -> ConvertSpec Maybe -> ConvertSpec Maybe
mergeArg OverwriteFiles convertSpec = convertSpec { convertOverwriteFiles = True }
mergeArg (ConvertLhsStyle lhsStyle) convertSpec
  | Just previousLhsStyle <- convertLhsStyle convertSpec,
    previousLhsStyle /= fmap LT.pack lhsStyle
  = error $ printf "Conflicting lhs styles requested: <%s> and <%s>" (show lhsStyle)
              (show previousLhsStyle)
  | otherwise = convertSpec { convertLhsStyle = Just (LT.pack <$> lhsStyle) }
mergeArg (ConvertFrom inputFile) convertSpec
  | Just previousInputFile <- convertInput convertSpec,
    previousInputFile /= inputFile
  = error $ printf "Multiple input files specified: <%s> and <%s>" inputFile previousInputFile
  | otherwise = convertSpec
      { convertInput = Just inputFile
      , convertToIpynb = case (convertToIpynb convertSpec, fromExt inputFile) of
        (prev, Nothing)    -> prev
        (prev@(Just _), _) -> prev
        (Nothing, format)  -> fmap (== LhsMarkdown) format
      }
mergeArg (ConvertTo outputFile) convertSpec
  | Just previousOutputFile <- convertOutput convertSpec,
    previousOutputFile /= outputFile
  = error $ printf "Multiple output files specified: <%s> and <%s>" outputFile previousOutputFile
  | otherwise = convertSpec
      { convertOutput = Just outputFile
      , convertToIpynb = case (convertToIpynb convertSpec, fromExt outputFile) of
        (prev, Nothing)    -> prev
        (prev@(Just _), _) -> prev
        (Nothing, format)  -> fmap (== IpynbFile) format
      }
mergeArg unexpectedArg _ = error $ "IHaskell.Convert.mergeArg: impossible argument: "
                                   ++ show unexpectedArg

-- | Guess the format based on the file extension.
fromExt :: FilePath -> Maybe NotebookFormat
fromExt s =
  case map toLower (takeExtension s) of
    ".lhs"   -> Just LhsMarkdown
    ".ipynb" -> Just IpynbFile
    _        -> Nothing
