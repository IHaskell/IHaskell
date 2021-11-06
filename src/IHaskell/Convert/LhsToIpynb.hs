{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module IHaskell.Convert.LhsToIpynb (lhsToIpynb) where

import           IHaskellPrelude
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString.Lazy as LBS

import           Data.Aeson ((.=), encode, object, Value(Array, Bool, Number, String, Null))
import           Data.Char (isSpace)
import qualified Data.Vector as V
import qualified Data.List as List

import           IHaskell.Flags (LhsStyle(LhsStyle))

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Key as Key
#else
#endif

lhsToIpynb :: LhsStyle LText -> FilePath -> FilePath -> IO ()
lhsToIpynb sty from to = do
  classed <- classifyLines sty . LT.lines . LT.pack <$> readFile from
  LBS.writeFile to . encode . encodeCells $ groupClassified classed

data CellLine a = CodeLine a
                | OutputLine a
                | MarkdownLine a
  deriving Show

isCode :: CellLine t -> Bool
isCode (CodeLine _) = True
isCode _ = False

isOutput :: CellLine t -> Bool
isOutput (OutputLine _) = True
isOutput _ = False

isMD :: CellLine t -> Bool
isMD (MarkdownLine _) = True
isMD _ = False

isEmptyMD :: (Eq a, Monoid a) => CellLine a -> Bool
isEmptyMD (MarkdownLine a) = a == mempty
isEmptyMD _ = False

untag :: CellLine t -> t
untag (CodeLine a) = a
untag (OutputLine a) = a
untag (MarkdownLine a) = a

data Cell a = Code a a
            | Markdown a
  deriving Show

encodeCells :: [Cell [LText]] -> Value
encodeCells xs = object $
  "cells" .= Array (V.fromList (map cellToVal xs)) : boilerplate

cellToVal :: Cell [LText] -> Value
cellToVal (Code i o) = object
                         [ "cell_type" .= String "code"
                         , "execution_count" .= Null
                         , "metadata" .= object ["collapsed" .= Bool False]
                         , "source" .= arrayFromTxt i
                         , "outputs" .= Array
                                          (V.fromList
                                             [object
                                                [ "text" .= arrayFromTxt o
                                                , "metadata" .= object []
                                                , "output_type" .= String "display_data"
                                                ] | _ <- take 1 o])
                         ]
cellToVal (Markdown txt) = object
                             [ "cell_type" .= String "markdown"
                             , "metadata" .= object ["hidden" .= Bool False]
                             , "source" .= arrayFromTxt txt
                             ]

-- | arrayFromTxt makes a JSON array of string s
arrayFromTxt :: [LText] -> Value
arrayFromTxt i = Array (V.fromList $ map stringify i)
  where
    stringify = String . LT.toStrict . flip LT.snoc '\n'

-- | ihaskell needs this boilerplate at the upper level to interpret the json describing cells and
-- output correctly.
#if MIN_VERSION_aeson(2,0,0)
boilerplate :: [(Key.Key, Value)]
#else
boilerplate :: [(T.Text, Value)]
#endif
boilerplate =
  ["metadata" .= object [kernelspec, lang], "nbformat" .= Number 4, "nbformat_minor" .= Number 0]
  where
    kernelspec = "kernelspec" .= object
                                   [ "display_name" .= String "Haskell"
                                   , "language" .= String "haskell"
                                   , "name" .= String "haskell"
                                   ]
    lang = "language_info" .= object ["name" .= String "haskell", "version" .= String VERSION_ghc]

groupClassified :: [CellLine LText] -> [Cell [LText]]
groupClassified (CodeLine a:x)
  | (c, x1) <- List.span isCode x,
    (_, x2) <- List.span isEmptyMD x1,
    (o, x3) <- List.span isOutput x2
  = Code (a : map untag c) (map untag o) : groupClassified x3
groupClassified (MarkdownLine a:x)
  | (m, x1) <- List.span isMD x = Markdown (a : map untag m) : groupClassified x1
groupClassified (OutputLine a:x) = Markdown [a] : groupClassified x
groupClassified [] = []

classifyLines :: LhsStyle LText -> [LText] -> [CellLine LText]
classifyLines sty@(LhsStyle c o _ _ _ _) (l:ls) =
  case (sp c, sp o) of
    (Just a, Nothing)  -> CodeLine a : classifyLines sty ls
    (Nothing, Just a)  -> OutputLine a : classifyLines sty ls
    (Nothing, Nothing) -> MarkdownLine l : classifyLines sty ls
    _                  -> error "IHaskell.Convert.classifyLines"
  where
    sp x = LT.stripPrefix (dropSpace x) (dropSpace l) `mplus` blankCodeLine x
    blankCodeLine x = if LT.strip x == LT.strip l
                        then Just ""
                        else Nothing
    dropSpace = LT.dropWhile isSpace
classifyLines _ [] = []
