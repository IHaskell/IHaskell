{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Jupyter.IHaskell.Hoogle (
    hoogleSearch,
    hoogleSearchExact,
    HoogleResult(..),
    renderHoogleResultHTML,
    renderHoogleResultPlain,
    ) where

-- Imports from 'base'
import           Control.Exception (catch, SomeException)
import           Control.Monad (guard)
import           Data.Bifunctor (second)
import           Data.Char (isAscii, isAlphaNum)
import           Data.Function ((&))
import           Data.List (groupBy, elemIndex)
import           Data.Monoid ((<>))

-- Imports from 'aeson'
import           Data.Aeson (FromJSON(..), (.:), eitherDecode, Value(..))

-- Imports from 'text'
import           Data.Text (Text)
import qualified Data.Text as T

-- Imports from 'bytestring'
import           Data.ByteString.Lazy (ByteString)

-- Imports from 'http-client'
import           Network.HTTP.Client (withManager, parseUrl, httpLbs, responseBody, parseRequest)

-- Imports from 'http-client-tls'
import           Network.HTTP.Client.TLS (tlsManagerSettings)

data HoogleResult =
       HoogleResult
         { hoogleResultLocation :: Text
         , hoogleResultSelf :: Text
         , hoogleResultDocs :: Text
         }
  deriving (Eq, Ord, Show)

instance FromJSON HoogleResult where
  parseJSON (Object obj) =
    HoogleResult <$> obj .: "location" <*> obj .: "self" <*> obj .: "docs"
  parseJSON _ = fail "Expected object with fields: location, self, docs"

newtype HoogleResults = HoogleResults { unHoogleResults :: [HoogleResult] }
  deriving (Eq, Ord, Show)

instance FromJSON HoogleResults where
  parseJSON (Object obj) = do
    results <- obj .: "results"
    HoogleResults <$> mapM parseJSON results
  parseJSON _ = fail "Expected object with 'results' field."

-- | Look up an identifier on Hoogle. Return documentation for that identifier. If there are many
-- identifiers, include documentation for all of them.
hoogleSearchExact :: Text -> IO (Either String [HoogleResult])
hoogleSearchExact string = second filterHoogleMatches <$> hoogleSearch string
  where
    filterHoogleMatches :: [HoogleResult] -> [HoogleResult]
    filterHoogleMatches = filter (isExactMatch string)

    isExactMatch :: Text -> HoogleResult -> Bool
    isExactMatch name HoogleResult { .. } = T.strip hoogleResultSelf == T.strip name

-- | Query Hoogle for the given string. This searches Hoogle using the internet. It returns either
-- an error message or the successful JSON result.
hoogleSearch :: Text -> IO (Either String [HoogleResult])
hoogleSearch str = do
  let url = T.concat ["https://www.haskell.org/hoogle/?hoogle=", T.pack $ urlEncode $ T.unpack str, "&mode=json"]
  flip catch handleException $ do
    request <- parseRequest (T.unpack url)
    parseResponse <$> withManager tlsManagerSettings (httpLbs request)

  where
    handleException :: SomeException -> IO (Either String a)
    handleException exception = return . Left . show $ exception

    parseResponse = second unHoogleResults . eitherDecode . responseBody

-- | Encode a string for use within a URL query parameter.
--
-- Copied directly from the HTTP package.
urlEncode :: String -> String
urlEncode [] = []
urlEncode (ch:t)
  | (isAscii ch && isAlphaNum ch) || ch `elem` ("-_.~" :: String) = ch : urlEncode t
  | not (isAscii ch) = foldr escape (urlEncode t) (eightBs [] (fromEnum ch))
  | otherwise = escape (fromEnum ch) (urlEncode t)
  where
    escape :: Int -> String -> String
    escape b rs = '%' : showH (b `div` 16) (showH (b `mod` 16) rs)

    showH :: Int -> String -> String
    showH x xs
      | x <= 9 = toEnum (o_0 + x) : xs
      | otherwise = toEnum (o_A + (x - 10)) : xs
      where
        o_0 = fromEnum '0'
        o_A = fromEnum 'A'

    eightBs :: [Int] -> Int -> [Int]
    eightBs acc x
      | x <= 255 = x : acc
      | otherwise = eightBs ((x `mod` 256) : acc) (x `div` 256)

-- | Render a Hoogle result to plain text.
renderHoogleResultPlain :: HoogleResult -> Text
renderHoogleResultPlain HoogleResult { .. } =
  T.concat [hoogleResultSelf, "\nURL: ", hoogleResultLocation, "\n", hoogleResultDocs]

-- | Render a Hoogle result to HTML.
renderHoogleResultHTML :: HoogleResult -> Text
renderHoogleResultHTML HoogleResult { .. } =
  renderSelf hoogleResultSelf hoogleResultLocation <> renderDocs hoogleResultDocs

renderSelf :: Text -> Text -> Text
renderSelf string loc
  | "package" `T.isPrefixOf` string =
      T.concat [pkg, " ", mkSpan "hoogle-package" (mkLink loc $ extractPackage string)]

  | "module" `T.isPrefixOf` string =
      T.concat
        [ mod
        , " "
        , mkSpan "hoogle-module" (mkLink loc $ extractModule string)
        , packageSub (extractPackageName loc)
        ]

  | "class" `T.isPrefixOf` string =
      T.concat
        [ cls
        , " "
        , mkSpan "hoogle-class" (mkLink loc $ extractClass string)
        , packageSub (extractPackageName loc)
        ]

  | "data" `T.isPrefixOf` string =
      T.concat
        [ dat
        , " "
        , mkSpan "hoogle-class" (mkLink loc $ extractData string)
        , packageSub (extractPackageName loc)
        ]

  | otherwise =
      case T.splitOn "::" string of
        [name, args] ->
          mkSpan "hoogle-name"
            (unicodeReplace $ mkLink loc (T.strip name) <> " :: " <> T.strip args)
          <> packageAndModuleSub (extractPackageName loc) (extractModuleName loc)
  where
    extractPackage = T.strip . T.replace "package" ""
    extractModule = T.strip . T.replace "module" ""
    extractClass = T.strip . T.replace "class" ""
    extractData = T.strip . T.replace "data" ""
    pkg = mkSpan "hoogle-head" "package"
    mod = mkSpan "hoogle-head" "module"
    cls = mkSpan "hoogle-head" "class"
    dat = mkSpan "hoogle-head" "data"

    unicodeReplace :: Text -> Text
    unicodeReplace =
      T.replace "forall" "&#x2200;" .
      T.replace "=>" "&#x21D2;" .
      T.replace "->" "&#x2192;" .
      T.replace "::" "&#x2237;"

    packageSub Nothing = ""
    packageSub (Just package) =
      mkSpan "hoogle-sub" $
        T.concat ["(", pkg, " ", mkSpan "hoogle-package" package, ")"]

    packageAndModuleSub Nothing _ = ""
    packageAndModuleSub (Just package) Nothing = packageSub (Just package)
    packageAndModuleSub (Just package) (Just modname) =
      mkSpan "hoogle-sub" $
        T.concat
          [ "("
          , pkg
          , " "
          , mkSpan "hoogle-package" package
          , ", "
          , mod
          , " "
          , mkSpan "hoogle-module" modname
          , ")"
          ]

renderDocs :: Text -> Text
renderDocs docs =
  docs &
  T.lines &
  groupBy bothAreCode &
  map makeBlock &
  T.unlines &
  mkDiv "hoogle-doc"
  where
    bothAreCode x y = isCode [x] && isCode [y]
    isCode ls =
      case ls of
        []  -> False
        s:_ -> T.isPrefixOf ">" $ T.strip s
    makeBlock ls =
      mkDiv
        (if isCode ls
           then "hoogle-code"
           else "hoogle-text")
        (T.unlines $ filter (not . T.null . T.strip) ls)

extractPackageName :: Text -> Maybe Text
extractPackageName link = do
  let pieces = T.splitOn "/" link
  archiveLoc <- elemIndex "archive" pieces
  latestLoc <- elemIndex "latest" pieces
  guard $ latestLoc - archiveLoc == 2
  return $ pieces !! (latestLoc - 1)

extractModuleName :: Text -> Maybe Text
extractModuleName link = do
  case T.splitOn "/" link of
    []     -> Nothing
    pieces -> Just $ T.replace "-" "." $ T.takeWhile (/= '.') $ last pieces

mkDiv :: Text -> Text -> Text
mkDiv cls content = T.concat ["<div class='", cls, "'>", content, "</div>"]

mkSpan :: Text -> Text -> Text
mkSpan cls content = T.concat ["<span class='", cls, "'>", content, "</span>"]

mkLink :: Text -> Text -> Text
mkLink href content = T.concat  ["<a target='_blank' href='", href, "'>", content, "</a>"]
