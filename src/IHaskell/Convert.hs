{-# LANGUAGE OverloadedStrings,
   PatternGuards,
   QuasiQuotes,
   ScopedTypeVariables,
   ViewPatterns #-}
-- | Description : mostly reversible conversion between ipynb and lhs 
module IHaskell.Convert (convert) where
import Control.Applicative
import Data.Aeson
import Data.Char
import Data.List
import Control.Monad.Identity
import Data.Maybe
import Data.Monoid
import Data.Vector (Vector)
import IHaskell.Convert.HMQQ (q)
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as TS
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Vector as V
import System.FilePath
import System.Directory
import Text.Printf

import IHaskell.Flags

fromExt ::  FilePath -> Maybe NotebookFormat
fromExt s = case map toLower (takeExtension s) of
  ".lhs" -> Just LhsMarkdown
  ".ipynb" -> Just IPYNB
  _ -> Nothing


data ConvertSpec f = ConvertSpec
  { convertToIpynb :: f Bool,
    convertInput :: f FilePath,
    convertOutput :: f FilePath,
    convertLhsStyle :: f (LhsStyle T.Text),
    convertOverwriteFiles :: Bool
  }

-- | used by @IHaskell convert@
convert :: [Argument] -> IO ()
convert args = case fromJustConvertSpec (toConvertSpec args) of
  ConvertSpec { convertToIpynb = Identity toIpynb,
                convertInput = Identity inputFile,
                convertOutput = Identity outputFile,
                convertLhsStyle = Identity lhsStyle,
                convertOverwriteFiles = force }
      | toIpynb -> do
        unless force (failIfExists outputFile)
        lhsToIpynb lhsStyle inputFile outputFile
      | otherwise -> do
        unless force (failIfExists outputFile)
        ipynbTolhs lhsStyle inputFile outputFile

failIfExists :: FilePath -> IO ()
failIfExists file = do
  exists <- doesFileExist file
  when exists $ fail $
    printf "File %s already exists. To force supply --force." file

fromJustConvertSpec ::  ConvertSpec Maybe -> ConvertSpec Identity
fromJustConvertSpec convertSpec = convertSpec {
        convertToIpynb = Identity toIpynb,
        convertInput = Identity inputFile,
        convertOutput = Identity outputFile,
        convertLhsStyle = Identity $ fromMaybe
                                        (T.pack <$> lhsStyleBird)
                                        (convertLhsStyle convertSpec)
      }
  where
    toIpynb = fromMaybe (error "Error: direction for conversion unknown")
                            (convertToIpynb convertSpec)
    (inputFile, outputFile) = case (convertInput convertSpec, convertOutput convertSpec) of
        (Nothing, Nothing) -> error "Error: no files specified for conversion"
        (Just i, Nothing) | toIpynb -> (i, dropExtension i <.> "ipynb")
                          | otherwise -> (i, dropExtension i <.> "lhs")
        (Nothing, Just o) | toIpynb -> (dropExtension o <.> "lhs", o)
                          | otherwise -> (dropExtension o <.> "ipynb", o)
        (Just i, Just o) -> (i, o)

isFormatSpec ::  Argument -> Bool
isFormatSpec (ConvertToFormat _) = True
isFormatSpec (ConvertFromFormat _) = True
isFormatSpec _ = False

toConvertSpec :: [Argument] -> ConvertSpec Maybe
toConvertSpec args = mergeArgs otherArgs
                                (mergeArgs formatSpecArgs initialConvertSpec)
  where
    (formatSpecArgs, otherArgs) = partition isFormatSpec args
    initialConvertSpec = ConvertSpec Nothing Nothing Nothing Nothing False

mergeArgs ::  [Argument] -> ConvertSpec Maybe -> ConvertSpec Maybe
mergeArgs args initialConvertSpec = foldr mergeArg initialConvertSpec args

mergeArg ::  Argument -> ConvertSpec Maybe -> ConvertSpec Maybe
mergeArg OverwriteFiles convertSpec = convertSpec { convertOverwriteFiles = True }
mergeArg (ConvertLhsStyle lhsStyle) convertSpec
  | Just previousLhsStyle <- convertLhsStyle convertSpec,
    previousLhsStyle /= fmap T.pack lhsStyle = error $ printf
                                              "Conflicting lhs styles requested: <%s> and <%s>"
                                              (show lhsStyle) (show previousLhsStyle)
  | otherwise = convertSpec { convertLhsStyle = Just (T.pack <$> lhsStyle) }
mergeArg (ConvertFrom inputFile) convertSpec
  | Just previousInputFile <- convertInput convertSpec,
    previousInputFile /= inputFile = error $ printf "Multiple input files specified: <%s> and <%s>"
                                              inputFile previousInputFile
  | otherwise = convertSpec {
          convertInput = Just inputFile,
          convertToIpynb = case (convertToIpynb convertSpec, fromExt inputFile) of
                              (prev, Nothing) -> prev
                              (prev @ (Just _), _) -> prev
                              (Nothing, format) -> fmap (== LhsMarkdown) format
        }

mergeArg (ConvertTo outputFile) convertSpec
  | Just previousOutputFile <- convertOutput convertSpec,
    previousOutputFile /= outputFile = error $ printf "Multiple output files specified: <%s> and <%s>"
                                              outputFile previousOutputFile
  | otherwise = convertSpec {
          convertOutput = Just outputFile,
          convertToIpynb = case (convertToIpynb convertSpec, fromExt outputFile) of
                              (prev, Nothing)         -> prev
                              (prev @ (Just _), _) -> prev
                              (Nothing, format) -> fmap (== IPYNB) format
        }

mergeArg unexpectedArg _ = error $ "IHaskell.Convert.mergeArg: impossible argument: "
                                      ++ show unexpectedArg

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
  [ "worksheets" .= Array (V.singleton (object
    [ "cells" .= Array (V.fromList (map cellToVal xs)) ] ))
  ] ++ boilerplate

cellToVal :: Cell [T.Text] -> Value
cellToVal (Code i o) = object $
  [ "cell_type" .= String "code",
    "collapsed" .= Bool False,
    "language" .= String "python", -- is what it IPython gives us
    "metadata" .= object [],
     "input" .= arrayFromTxt i,
     "outputs" .= Array 
        (V.fromList (
           [ object ["text" .= arrayFromTxt o,
             "metadata" .= object [],
             "output_type" .= String "display_data" ]
          | _ <- take 1 o])) ]

cellToVal (Markdown txt) = object $
  [ "cell_type" .= String "markdown",
    "metadata" .= object [],
    "source" .= arrayFromTxt txt ]

arrayFromTxt ::  [T.Text] -> Value
arrayFromTxt i = Array (V.fromList (map (String . T.toStrict) i))

boilerplate :: [(TS.Text, Value)]
boilerplate =
  [ "metadata" .= object [ "language" .= String "haskell", "name" .= String ""],
    "nbformat" .= Number 3,
    "nbformat_minor" .= Number 0 ]

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


ipynbTolhs :: LhsStyle T.Text -> FilePath -> FilePath -> IO ()
ipynbTolhs sty from to = do
  Just (js :: Object) <- decode <$> L.readFile from
  case js of
    [q| worksheets : Array x |]
      | [ Object [q| cells : Array x |] ] <- V.toList x ->
            T.writeFile to $ T.unlines $ V.toList
              $ V.map (\(Object y) -> convCell sty y) x
    _ -> error "IHaskell.Convert.ipynbTolhs: json does not follow expected schema"  

concatWithPrefix :: T.Text -> Vector Value -> Maybe T.Text
concatWithPrefix p arr = T.concat . map (p <>) . V.toList <$> V.mapM toStr arr

toStr :: Value -> Maybe T.Text
toStr (String x) = Just (T.fromStrict x)
toStr _ = Nothing

convOutputs ::  LhsStyle T.Text -> Vector Value -> Maybe T.Text
convOutputs sty arr = do
  outputLines <- V.mapM (getTexts (lhsOutputPrefix sty)) arr
  return $ lhsBeginOutput sty <> T.concat (V.toList outputLines) <> lhsEndOutput sty

getTexts ::  T.Text -> Value -> Maybe T.Text
getTexts p (Object [q| text : Array x |]) = concatWithPrefix p x
getTexts _ _ = Nothing

convCell :: LhsStyle T.Text -> Object -> T.Text
convCell _sty
         [q| cell_type : String "markdown",
             source : Array xs |]
    | ~ (Just s) <- concatWithPrefix "" xs = s
convCell sty
         [q| cell_type : String "code",
             input : Array i,
             outputs : Array o
            |]
    | ~ (Just i) <- concatWithPrefix (lhsCodePrefix sty) i,
      o <- fromMaybe mempty (convOutputs sty o) = "\n" <>
              lhsBeginCode sty <> i <> lhsEndCode sty <> "\n" <> o <> "\n"
convCell _ _ = "IHaskell.Convert.convCell: unknown cell"
