{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, OverloadedStrings #-}
module IHaskell.Eval.Hoogle ( 
  search,
  document,
  render,
  OutputFormat(..)
  ) where

import ClassyPrelude
import Text.Printf
import Network.HTTP
import Data.Aeson
import Data.String.Utils
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
  let request = getRequest $ queryUrl str
  response <- simpleHTTP request
  return $ case response of
    Left err -> Left $ show err
    Right resp -> Right $ rspBody resp
  where
    queryUrl :: String -> String
    queryUrl = printf "http://www.haskell.org/hoogle/?hoogle=%s&mode=json" . urlEncode

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
        Right results -> map SearchResult results

-- | Look up an identifier on Hoogle.
-- Return documentation for that identifier. If there are many
-- identifiers, include documentation for all of them.
document :: String -> IO [HoogleResult]
document string = do
  matchingResults <- filter matches <$> search string
  return $ map toDocResult matchingResults
  where
    matches (SearchResult resp) = startswith "string" $ self resp
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
  printf "%s<br/><a href='%s'>...more...</a><br/><div class='hoogle-doc'>%s</div>"
  (renderSelf $ self resp)
  (location resp)
  (renderDocs $ docs resp)

renderHtml (SearchResult resp) = 
  printf "%s<br/><a href='%s'>...more...</a><br/><div class='hoogle-doc'>%s</div>"
  (renderSelf $ self resp)
  (location resp)
  (renderDocs $ docs resp)

renderSelf :: String -> String
renderSelf string
  | startswith "package" string
    = printf "%s <span class='hoogle-package-name'>%s</span>" pkg $ replace "package" "" string
  | otherwise
    = printf "<span class='hoogle-name'>%s</span>" $ strip string
  where 
    pkg = "<span class='hoogle-package'>package</span>" :: String

renderDocs :: String -> String
renderDocs doc = 
  let groups = groupBy bothAreCode $ lines doc
      bothAreCode s1 s2 = 
        startswith ">" (strip s1) &&
        startswith ">" (strip s2)
      isCode (s:_) = startswith ">" $ strip s
      makeBlock lines =
        if isCode lines
        then printf "<div class='hoogle-code'>%s<div>" $ unlines lines
        else printf "<div class='hoogle-text'>%s<div>" $ unlines lines
      in
    unlines $ map makeBlock groups


