{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module IHaskell.Convert.IpynbToLhs (ipynbToLhs) where

import Control.Applicative ((<$>))
import Data.Aeson (decode, Object, Value(Array, Object, String))
import qualified Data.ByteString.Lazy as L (readFile)
import qualified Data.HashMap.Strict as M (lookup)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>), Monoid(mempty))
import qualified Data.Text.Lazy as T (concat, fromStrict, Text, unlines)
import qualified Data.Text.Lazy.IO as T (writeFile)
import Data.Vector (Vector)
import qualified Data.Vector as V (map, mapM, toList)
import IHaskell.Flags (LhsStyle(lhsBeginCode, lhsBeginOutput, lhsCodePrefix, lhsEndCode, lhsEndOutput, lhsOutputPrefix))

ipynbToLhs :: LhsStyle T.Text
  -> FilePath -- ^ the filename of an ipython notebook
  -> FilePath -- ^ the filename of the literate haskell to write
  -> IO ()
ipynbToLhs sty from to = do
  Just (js :: Object) <- decode <$> L.readFile from
  case M.lookup "worksheets" js of
    Just (Array worksheets)
      | [ Object worksheet ] <- V.toList worksheets,
        Just (Array cells) <- M.lookup "cells" worksheet ->
            T.writeFile to $ T.unlines $ V.toList
              $ V.map (\(Object y) -> convCell sty y) cells
    _ -> error "IHaskell.Convert.ipynbTolhs: json does not follow expected schema"  

concatWithPrefix :: T.Text -- ^ the prefix to add to every line
  -> Vector Value          -- ^ a json array of text lines
  -> Maybe T.Text
concatWithPrefix p arr = T.concat . map (p <>) . V.toList <$> V.mapM toStr arr

toStr :: Value -> Maybe T.Text
toStr (String x) = Just (T.fromStrict x)
toStr _ = Nothing

-- | @convCell sty cell@ converts a single cell in JSON into text suitable
-- for the type of lhs file described by the @sty@
convCell :: LhsStyle T.Text -> Object -> T.Text
convCell _sty object
  | Just (String "markdown") <- M.lookup "cell_type" object,
    Just (Array xs)          <- M.lookup "source" object,
    ~ (Just s) <- concatWithPrefix "" xs = s
convCell sty object
    | Just (String "code") <- M.lookup "cell_type" object,
      Just (Array i)       <- M.lookup "input" object,
      Just (Array o)       <- M.lookup "outputs" object,
     ~ (Just i) <- concatWithPrefix (lhsCodePrefix sty) i,
      o <- fromMaybe mempty (convOutputs sty o) = "\n" <>
              lhsBeginCode sty <> i <> lhsEndCode sty <> "\n" <> o <> "\n"
convCell _ _ = "IHaskell.Convert.convCell: unknown cell"

convOutputs ::  LhsStyle T.Text
  -> Vector Value -- ^ JSON array of output lines containing text or markup
  -> Maybe T.Text
convOutputs sty array = do
  outputLines <- V.mapM (getTexts (lhsOutputPrefix sty)) array
  return $ lhsBeginOutput sty <> T.concat (V.toList outputLines) <> lhsEndOutput sty

getTexts ::  T.Text -> Value -> Maybe T.Text
getTexts p (Object object)
  | Just (Array text) <- M.lookup "text" object = concatWithPrefix p text
getTexts _ _ = Nothing
