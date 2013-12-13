{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module IHaskell.Display (
  IHaskellDisplay(..),
  plain,
  html
  ) where

import ClassyPrelude
import Data.String.Utils (rstrip) 

import IHaskell.Types

-- | A class for displayable Haskell types.
class IHaskellDisplay a where
  display :: a -> [DisplayData]

-- | Generate a plain text display.
plain :: String -> DisplayData
plain = Display PlainText . rstrip

-- | Generate an HTML display.
html :: String -> DisplayData
html = Display MimeHtml
