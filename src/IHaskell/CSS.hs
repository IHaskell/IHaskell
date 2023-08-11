{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module IHaskell.CSS (ihaskellCSS) where

import           Data.Text as T

ihaskellCSS :: Text
ihaskellCSS =
  T.unlines
    [
    hoogleCSS
    , basicCSS
    , highlightCSS
    , hlintCSS
    ]

hoogleCSS :: Text
hoogleCSS =
  T.unlines
    [
    -- Custom IHaskell CSS
    "/* Styles used for the Hoogle display in the pager */"
    , ".hoogle-doc {"
    , "display: block;"
    , "padding-bottom: 1.3em;"
    , "padding-left: 0.4em;"
    , "}"
    , ".hoogle-code {"
    , "display: block;"
    , "font-family: monospace;"
    , "white-space: pre;"
    , "}"
    , ".hoogle-text {"
    , "display: block;"
    , "}"
    , ".hoogle-name {"
    , "color: green;"
    , "font-weight: bold;"
    , "}"
    , ".hoogle-head {"
    , "font-weight: bold;"
    , "}"
    , ".hoogle-sub {"
    , "display: block;"
    , "margin-left: 0.4em;"
    , "}"
    , ".hoogle-package {"
    , "font-weight: bold;"
    , "font-style: italic;"
    , "}"
    , ".hoogle-module {"
    , "font-weight: bold;"
    , "}"
    , ".hoogle-class {"
    , "font-weight: bold;"
    , "}"
    ]


basicCSS :: Text
basicCSS =
  T.unlines
    [
    -- Styles used for basic displays
    ".get-type {"
    , "color: green;"
    , "font-weight: bold;"
    , "font-family: monospace;"
    , "display: block;"
    , "white-space: pre-wrap;"
    , "}"
    , ".show-type {"
    , "color: green;"
    , "font-weight: bold;"
    , "font-family: monospace;"
    , "margin-left: 1em;"
    , "}"
    , ".mono {"
    , "font-family: monospace;"
    , "display: block;"
    , "}"
    , ".err-msg {"
    , "color: red;"
    , "font-style: italic;"
    , "font-family: monospace;"
    , "white-space: pre;"
    , "display: block;"
    , "}"
    , "#unshowable {"
    , "color: red;"
    , "font-weight: bold;"
    , "}"
    , ".err-msg.in.collapse {"
    , "padding-top: 0.7em;"
    , "}"
    ]

highlightCSS :: Text
highlightCSS =
  T.unlines
    [
    -- Code that will get highlighted before it is highlighted
    ".highlight-code {"
    , "white-space: pre;"
    , "font-family: monospace;"
    , "}"
    ]

hlintCSS :: Text
hlintCSS =
  T.unlines
    [
    ".suggestion-warning { "
    , "font-weight: bold;"
    , "color: rgb(200, 130, 0);"
    , "}"
    , ".suggestion-error { "
    , "font-weight: bold;"
    , "color: red;"
    , "}"
    , ".suggestion-name {"
    , "font-weight: bold;"
    , "}"
    ]
