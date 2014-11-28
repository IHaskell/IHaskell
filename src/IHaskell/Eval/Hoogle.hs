{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, OverloadedStrings #-}
module IHaskell.Eval.Hoogle (
  search,
  document,
  render,
  OutputFormat(..),
  HoogleResult
  ) where

import ClassyPrelude hiding (last, span, div)
import Text.Printf
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Data.Aeson
import Data.String.Utils
import Data.List (elemIndex, (!!), last)
import qualified Data.ByteString.Lazy.Char8 as Char


import IHaskell.IPython

-- | Types of formats to render output to.
data OutputFormat
     = Plain      -- ^ Render to plain text.
     | HTML       -- ^ Render to HTML.

data HoogleResponse = HoogleResponse {
    location :: String,
    self :: String,
    docs :: String
  }
  deriving (Eq, Show)

data HoogleResult
     = SearchResult HoogleResponse
     | DocResult HoogleResponse
     | NoResult String
     deriving Show

instance FromJSON [HoogleResponse] where
  parseJSON (Object obj) = do
    results <- obj .: "results"
    mapM parseJSON results

  parseJSON _ = fail "Expected object with 'results' field."

instance FromJSON HoogleResponse where
  parseJSON (Object obj) =
    HoogleResponse      <$>
    obj .: "location" <*>
    obj .: "self"     <*>
    obj .: "docs"

  parseJSON _ = fail "Expected object with fields: location, self, docs"

-- | Query Hoogle for the given string.
-- This searches Hoogle using the internet. It returns either an error
-- message or the successful JSON result.
query :: String -> IO (Either String String)
query str = do
  request <- parseUrl $ queryUrl str
  response <- try $ withManager tlsManagerSettings $ httpLbs request
  return $ case response of
    Left err -> Left $ show (err :: SomeException)
    Right resp -> Right $ Char.unpack $ responseBody resp
  where
    queryUrl :: String -> String
    queryUrl = printf "https://www.haskell.org/hoogle/?hoogle=%s&mode=json"

-- | Search for a query on Hoogle.
-- Return all search results.
search :: String -> IO [HoogleResult]
search string = do
  response <- query string
  return $ case response of
    Left err -> [NoResult err]
    Right json ->
      case eitherDecode $ Char.pack json of
        Left err -> [NoResult err]
        Right results ->
          case map SearchResult results of
            [] -> [NoResult "no matching identifiers found."]
            res -> res

-- | Look up an identifier on Hoogle.
-- Return documentation for that identifier. If there are many
-- identifiers, include documentation for all of them.
document :: String -> IO [HoogleResult]
document string = do
  matchingResults <- filter matches <$> search string
  let results = map toDocResult matchingResults
  return $ case results of
    [] -> [NoResult "no matching identifiers found."]
    res -> res
  where
    matches (SearchResult resp) =
      case split " " $ self resp of
        name:_ -> strip string == strip name
        _ -> False
    matches _ = False

    toDocResult (SearchResult resp) = DocResult resp

-- | Render a Hoogle search result into an output format.
render :: OutputFormat -> HoogleResult -> String
render Plain = renderPlain
render HTML  = renderHtml

-- | Render a Hoogle result to plain text.
renderPlain :: HoogleResult -> String

renderPlain (NoResult res) =
  "No response available: " ++ res

renderPlain (SearchResult resp) =
  printf "%s\nURL: %s\n%s"
  (self resp)
  (location resp)
  (docs resp)

renderPlain (DocResult resp) =
  printf "%s\nURL: %s\n%s"
  (self resp)
  (location resp)
  (docs resp)

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
  | startswith "package" string
    = pkg ++ " " ++ span "hoogle-package" (link loc $ extractPackage string)

  | startswith "module" string
    = let package = extractPackageName loc in
        mod ++ " " ++
        span "hoogle-module" (link loc $ extractModule string) ++
        packageSub package

  | startswith "class" string
    = let package = extractPackageName loc in
        cls ++ " " ++
        span "hoogle-class" (link loc $ extractClass string) ++
        packageSub package

  | startswith "data" string
    = let package = extractPackageName loc in
        dat ++ " " ++
        span "hoogle-class" (link loc $ extractData string) ++
        packageSub package

  | otherwise
    = let [name, args] = split "::" string
          package = extractPackageName loc
          modname = extractModuleName loc in
        span "hoogle-name" (unicodeReplace $
          link loc (strip name) ++
          " :: " ++
          strip args)
        ++ packageAndModuleSub package modname

  where
    extractPackage = strip . replace "package" ""
    extractModule = strip . replace "module" ""
    extractClass = strip . replace "class" ""
    extractData = strip . replace "data" ""
    pkg = span "hoogle-head" "package"
    mod = span "hoogle-head" "module"
    cls = span "hoogle-head" "class"
    dat = span "hoogle-head" "data"

    unicodeReplace :: String -> String
    unicodeReplace =
     replace "forall" "&#x2200;" .
     replace "=>"     "&#x21D2;" .
     replace "->"     "&#x2192;" .
     replace "::"     "&#x2237;"

    packageSub Nothing = ""
    packageSub (Just package) =
      span "hoogle-sub" $
        "(" ++ pkg ++ " " ++ span "hoogle-package" package ++ ")"

    packageAndModuleSub Nothing _ = ""
    packageAndModuleSub (Just package) Nothing = packageSub (Just package)
    packageAndModuleSub (Just package) (Just modname) =
        span "hoogle-sub" $
          "(" ++ pkg ++ " " ++ span "hoogle-package" package ++
           ", " ++ mod ++ " " ++ span "hoogle-module" modname ++ ")"

renderDocs :: String -> String
renderDocs doc =
  let groups = groupBy bothAreCode $ lines doc
      nonull = filter (not . null . strip)
      bothAreCode s1 s2 =
        startswith ">" (strip s1) &&
        startswith ">" (strip s2)
      isCode (s:_) = startswith ">" $ strip s
      makeBlock lines =
        if isCode lines
        then div "hoogle-code" $ unlines $ nonull lines
        else div "hoogle-text" $ unlines $ nonull lines
      in
    div "hoogle-doc" $ unlines $ map makeBlock groups

extractPackageName :: String ->  Maybe String
extractPackageName link = do
  let pieces = split "/" link
  archiveLoc <- elemIndex "archive" pieces
  latestLoc <- elemIndex "latest" pieces
  guard $ latestLoc - archiveLoc == 2
  return $ pieces !! (latestLoc - 1)

extractModuleName :: String ->  Maybe String
extractModuleName link = do
  let pieces = split "/" link
  guard $ not $ null pieces
  let html = last pieces
      mod = replace "-" "." $ takeWhile (/= '.') html
  return mod

div :: String -> String -> String
div = printf "<div class='%s'>%s</div>"

span :: String -> String -> String
span = printf "<span class='%s'>%s</span>"

link :: String -> String -> String
link = printf "<a target='_blank' href='%s'>%s</a>"
