{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module IHaskell.Convert.IpynbToLhs (ipynbToLhs) where

import           IHaskellPrelude
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as CBS

import           Data.Aeson (decode, Object, Value(Array, Object, String))
import           Data.Monoid ((<>), Monoid(mempty))
import           Data.Vector (Vector)
import           Data.HashMap.Strict (lookup)

import qualified Data.Text.Lazy.IO as LTIO
import qualified Data.Vector as V (map, mapM, toList)

import           IHaskell.Flags (LhsStyle(..))

ipynbToLhs :: LhsStyle LText
           -> FilePath -- ^ the filename of an ipython notebook
           -> FilePath -- ^ the filename of the literate haskell to write
           -> IO ()
ipynbToLhs sty from to = do
  Just (js :: Object) <- decode <$> LBS.readFile from
  case lookup "cells" js of
    Just (Array cells) ->
      LTIO.writeFile to $ LT.unlines $ V.toList $ V.map (\(Object y) -> convCell sty y) cells
    _ -> error "IHaskell.Convert.ipynbTolhs: json does not follow expected schema"

concatWithPrefix :: LT.Text -- ^ the prefix to add to every line
                 -> Vector Value          -- ^ a json array of text lines
                 -> Maybe LT.Text
concatWithPrefix p arr = LT.concat . map (p <>) . V.toList <$> V.mapM toStr arr

toStr :: Value -> Maybe LT.Text
toStr (String x) = Just (LT.fromStrict x)
toStr _ = Nothing

-- | @convCell sty cell@ converts a single cell in JSON into text suitable for the type of lhs file
-- described by the @sty@
convCell :: LhsStyle LT.Text -> Object -> LT.Text
convCell _sty object
  | Just (String "markdown") <- lookup "cell_type" object,
    Just (Array xs) <- lookup "source" object,
    ~(Just s) <- concatWithPrefix "" xs
  = s
convCell sty object
  | Just (String "code") <- lookup "cell_type" object,
    Just (Array i) <- lookup "source" object,
    Just (Array o) <- lookup "outputs" object,
    ~(Just i) <- concatWithPrefix (lhsCodePrefix sty) i,
    o <- fromMaybe mempty (convOutputs sty o)
  = "\n" <>
    lhsBeginCode sty <> i <> lhsEndCode sty <> "\n" <> o <> "\n"
convCell _ _ = "IHaskell.Convert.convCell: unknown cell"

convOutputs :: LhsStyle LT.Text
            -> Vector Value -- ^ JSON array of output lines containing text or markup
            -> Maybe LT.Text
convOutputs sty array = do
  outputLines <- V.mapM (getTexts (lhsOutputPrefix sty)) array
  return $ lhsBeginOutput sty <> LT.concat (V.toList outputLines) <> lhsEndOutput sty

getTexts :: LT.Text -> Value -> Maybe LT.Text
getTexts p (Object object)
  | Just (Array text) <- lookup "text" object = concatWithPrefix p text
getTexts _ _ = Nothing
