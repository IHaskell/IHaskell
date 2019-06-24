{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module IHaskell.Eval.Hoogle (
    search,
    document,
    render,
    OutputFormat(..),
    HoogleResult(..),
    HoogleResponse(..),
    parseResponse,
    ) where

import qualified Data.ByteString.Char8   as CBS
import qualified Data.ByteString.Lazy    as LBS
import           Data.Either             (either)
import           IHaskellPrelude

import           Data.Aeson
import           Data.Char               (isAlphaNum, isAscii)
import qualified Data.List               as List
import qualified Data.Text               as T
import           Data.Vector             (toList)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS

import           StringUtils             (replace, split, strip)

-- | Types of formats to render output to.
data OutputFormat = Plain      -- ^ Render to plain text.
                  | HTML       -- ^ Render to HTML.

data HoogleResponse = HoogleResponse { location :: String, self :: String, docs :: String }
  deriving (Eq, Show)

data HoogleResult = SearchResult HoogleResponse
                  | DocResult HoogleResponse
                  | NoResult String
  deriving Show

data HoogleResponseList = HoogleResponseList [HoogleResponse]

instance FromJSON HoogleResponseList where
  parseJSON (Array arr) =
    HoogleResponseList <$> mapM parseJSON (toList arr)

  parseJSON _ = fail "Expected array."

instance FromJSON HoogleResponse where
  parseJSON (Object obj) =
    HoogleResponse
      <$>  obj .: "url"
      <*>  (removeMarkup <$> obj .: "item")
      <*>  obj .: "docs"

  parseJSON _ = fail "Expected object with fields: url, item, docs"

-- | Query Hoogle for the given string. This searches Hoogle using the internet. It returns either
-- an error message or the successful JSON result.
query :: String -> IO (Either String String)
query str = do
  request <- parseUrlThrow $ queryUrl $ urlEncode str
  mgr <- newManager tlsManagerSettings
  catch
    (Right . CBS.unpack . LBS.toStrict . responseBody <$> httpLbs request mgr)
    (\e -> return $ Left $ show (e :: SomeException))

  where
    queryUrl :: String -> String
    queryUrl = printf "http://hoogle.haskell.org/?hoogle=%s&mode=json"

-- | Copied from the HTTP package.
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

-- | Search for a query on Hoogle. Return all search results.
search :: String -> IO [HoogleResult]
search string = either ((:[]) . NoResult) parseResponse <$> query string

parseResponse :: String -> [HoogleResult]
parseResponse jsn =
  case eitherDecode $ LBS.fromStrict $ CBS.pack jsn of
    Left err -> [NoResult err]
    Right results ->
      case map SearchResult $ (\(HoogleResponseList l) -> l) results of
        []  -> [NoResult "no matching identifiers found."]
        res -> res

-- | Look up an identifier on Hoogle. Return documentation for that identifier. If there are many
-- identifiers, include documentation for all of them.
document :: String -> IO [HoogleResult]
document string = do
  matchingResults <- filter matches <$> search string
  return $
    case mapMaybe toDocResult matchingResults of
      []  -> [NoResult "no matching identifiers found."]
      res -> res

  where
    matches (SearchResult resp) =
      ("<s0>" ++ strip string ++ "</s0>") `elem` (split " " $ self resp)
    matches _ = False

    toDocResult (SearchResult resp) = Just $ DocResult resp
    toDocResult (DocResult _)       = Nothing
    toDocResult (NoResult _)        = Nothing

-- | Render a Hoogle search result into an output format.
render :: OutputFormat -> HoogleResult -> String
render Plain = renderPlain
render HTML  = renderHtml

-- | Render a Hoogle result to plain text.
renderPlain :: HoogleResult -> String
renderPlain (NoResult res) =
  "No response available: " ++ res

renderPlain (SearchResult resp) =
  printf "%s\nURL: %s\n%s" (self resp) (location resp) (docs resp)

renderPlain (DocResult resp) =
  printf "%s\nURL: %s\n%s" (self resp) (location resp) (docs resp)

-- | Render a Hoogle result to HTML.
renderHtml :: HoogleResult -> String
renderHtml (NoResult resp) =
  printf "<span class='err-msg'>No result: %s</span>" resp

renderHtml (DocResult resp) =
  renderSelf (self resp) (location resp)
  ++
  renderDocs (docs resp)

renderHtml (SearchResult resp) =
  renderSelf (self resp) (location resp)
  ++
  renderDocs (docs resp)

renderSelf :: String -> String -> String
renderSelf string loc
  | "package" `isPrefixOf` string =
      pkg ++ " " ++ span "hoogle-package" (link loc $ extractPackage string)

  | "module" `isPrefixOf` string =
      let package = extractPackageName loc
      in mdl ++ " " ++
                span "hoogle-module" (link loc $ extractModule string) ++
                packageSub package

  | "class" `isPrefixOf` string =
      let package = extractPackageName loc
      in cls ++ " " ++
                span "hoogle-class" (link loc $ extractClass string) ++
                packageSub package

  | "data" `isPrefixOf` string =
      let package = extractPackageName loc
      in dat ++ " " ++
                span "hoogle-class" (link loc $ extractData string) ++
                packageSub package

  | "newtype" `isPrefixOf` string =
      let package = extractPackageName loc
      in nwt ++ " " ++
                span "hoogle-class" (link loc $ extractNewtype string) ++
                packageSub package

  | "type" `isPrefixOf` string =
      let package = extractPackageName loc
      in nwt ++ " " ++
                span "hoogle-class" (link loc $ extractType string) ++
                packageSub package

  | otherwise =
      let [name, args] = split "::" string
          package = extractPackageName loc
          modname = extractModuleName loc
      in span "hoogle-name"
           (unicodeReplace $
              link loc (strip name) ++
              " :: " ++
              strip args)
         ++ packageAndModuleSub package modname
  where
    extractPackage = strip . replace "package" ""
    extractModule = strip . replace "module" ""
    extractClass = strip . replace "class" ""
    extractData = strip . replace "data" ""
    extractNewtype = strip . replace "newtype" ""
    extractType = strip . replace "newtype" ""
    pkg = span "hoogle-head" "package"
    mdl = span "hoogle-head" "module"
    cls = span "hoogle-head" "class"
    dat = span "hoogle-head" "data"
    nwt = span "hoogle-head" "newtype"

    unicodeReplace :: String -> String
    unicodeReplace =
      replace "forall" "&#x2200;" .
      replace "=>" "&#x21D2;" .
      replace "->" "&#x2192;" .
      replace "::" "&#x2237;"

    packageSub Nothing = ""
    packageSub (Just package) =
      span "hoogle-sub" $
        "(" ++ pkg ++ " " ++ span "hoogle-package" package ++ ")"

    packageAndModuleSub Nothing _ = ""
    packageAndModuleSub (Just package) Nothing = packageSub (Just package)
    packageAndModuleSub (Just package) (Just modname) =
      span "hoogle-sub" $
        "(" ++ pkg ++ " " ++ span "hoogle-package" package ++
                             ", " ++ mdl ++ " " ++ span "hoogle-module" modname ++ ")"

renderDocs :: String -> String
renderDocs doc = div' "hoogle-doc" doc

extractPackageName :: String -> Maybe String
extractPackageName lnk = do
  let pieces = split "/" lnk
  archiveLoc <- List.elemIndex "archive" pieces
  latestLoc <- List.elemIndex "latest" pieces
  guard $ latestLoc - archiveLoc == 2
  return $ pieces List.!! (latestLoc - 1)

extractModuleName :: String -> Maybe String
extractModuleName lnk =
  replace "-" "." . takeWhile (/= '.') <$> lastMay (split "/" lnk)

div' :: String -> String -> String
div' = printf "<div class='%s'>%s</div>"

span :: String -> String -> String
span = printf "<span class='%s'>%s</span>"

link :: String -> String -> String
link = printf "<a target='_blank' href='%s'>%s</a>"

-- | very explicit cleaning of the type signature in the hoogle 5 response,
-- to remove html markup and escaped characters.
removeMarkup :: String -> String
removeMarkup s = T.unpack $ List.foldl (flip ($)) (T.pack s) replaceAll
  where replacements :: [ (T.Text, T.Text) ]
        replacements = [ ( "<span class=name>", "" )
                       , ( "</span>", "" )
                       , ( "<0>", "" )
                       , ( "</0>", "" )
                       , ( "&gt;", ">" )
                       , ( "&lt;", "<" )
                       , ( "<b>", "")
                       , ( "</b>", "")
                       ]
        replaceAll = uncurry T.replace <$> replacements
