{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module IHaskell.Convert.LhsToIpynb (lhsToIpynb) where

import Control.Applicative ((<$>))
import Data.Aeson ((.=), encode, object, Value(Array, Bool, Number, String, Null))
import qualified Data.ByteString.Lazy as L (writeFile)
import Data.Char (isSpace)
import Data.Monoid (Monoid(mempty))
import qualified Data.Text as TS (Text)
import qualified Data.Text.Lazy as T (dropWhile, lines, stripPrefix, Text, toStrict, snoc)
import qualified Data.Text.Lazy.IO as T (readFile)
import qualified Data.Vector as V (fromList, singleton)
import IHaskell.Flags (LhsStyle(LhsStyle))

lhsToIpynb :: LhsStyle T.Text -> FilePath -> FilePath -> IO ()
lhsToIpynb sty from to = do
  classed <-  classifyLines sty . T.lines <$> T.readFile from
  L.writeFile to . encode . encodeCells $ groupClassified classed

data CellLine a = CodeLine a | OutputLine a | MarkdownLine a
                deriving Show

isCode ::  CellLine t -> Bool
isCode (CodeLine _) = True
isCode _ = False

isOutput ::  CellLine t -> Bool
isOutput (OutputLine _) = True
isOutput _ = False

isMD ::  CellLine t -> Bool
isMD (MarkdownLine _) = True
isMD _ = False

isEmptyMD ::  (Eq a, Monoid a) => CellLine a -> Bool
isEmptyMD (MarkdownLine a) = a == mempty
isEmptyMD _ = False


untag ::  CellLine t -> t
untag (CodeLine a) = a
untag (OutputLine a) = a
untag (MarkdownLine a) = a

data Cell a = Code a a | Markdown a
            deriving (Show)

encodeCells :: [Cell [T.Text]] -> Value
encodeCells xs = object $
   [ "cells" .= Array (V.fromList (map cellToVal xs)) ]
   ++ boilerplate

cellToVal :: Cell [T.Text] -> Value
cellToVal (Code i o) = object $
  [ "cell_type" .= String "code",
    "execution_count" .= Null,
    "metadata" .= object [ "collapsed" .= Bool False ],
    "source" .= arrayFromTxt i,
    "outputs" .= Array
        (V.fromList (
           [ object ["text" .= arrayFromTxt o,
             "metadata" .= object [],
             "output_type" .= String "display_data" ]
          | _ <- take 1 o])) ]

cellToVal (Markdown txt) = object $
  [ "cell_type" .= String "markdown",
    "metadata" .= object [ "hidden" .= Bool False ],
    "source" .= arrayFromTxt txt ]

-- | arrayFromTxt makes a JSON array of string s
arrayFromTxt ::  [T.Text] -> Value
arrayFromTxt i = Array (V.fromList $ map stringify i)
    where
        stringify = String . T.toStrict . flip T.snoc '\n'

-- | ihaskell needs this boilerplate at the upper level to interpret the
-- json describing cells and output correctly.
boilerplate :: [(TS.Text, Value)]
boilerplate =
  [ "metadata" .= object [ kernelspec, lang ]
  , "nbformat" .= Number 4
  , "nbformat_minor" .= Number 0
  ]
  where
    kernelspec = "kernelspec" .= object [
        "display_name" .= String "Haskell"
      , "language" .= String "haskell"
      , "name" .= String "haskell"
      ]
    lang = "language_info" .= object [
        "name" .= String "haskell"
      , "version" .= String VERSION_ghc
      ]

groupClassified :: [CellLine T.Text] -> [Cell [T.Text]]
groupClassified (CodeLine a : x)
    | (c,x) <- span isCode x,
      (_,x) <- span isEmptyMD x,
      (o,x) <- span isOutput x = Code (a : map untag c) (map untag o) : groupClassified x
groupClassified (MarkdownLine a : x)
    | (m,x) <- span isMD x = Markdown (a: map untag m) : groupClassified x
groupClassified (OutputLine a : x ) = Markdown [a] : groupClassified x
groupClassified [] = []

classifyLines :: LhsStyle T.Text -> [T.Text] -> [CellLine T.Text]
classifyLines sty@(LhsStyle c o _ _ _ _) (l:ls) = case (sp c, sp o) of
    (Just a, Nothing) -> CodeLine a : classifyLines sty ls
    (Nothing, Just a) -> OutputLine a : classifyLines sty ls
    (Nothing,Nothing) -> MarkdownLine l : classifyLines sty ls
    _ -> error "IHaskell.Convert.classifyLines"
  where sp c = T.stripPrefix (T.dropWhile isSpace c) (T.dropWhile isSpace l)
classifyLines _ [] = []

