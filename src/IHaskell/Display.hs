{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module IHaskell.Display (
  IHaskellDisplay(..),
  plain, html, png, jpg, svg, latex,
  serializeDisplay
  ) where

import ClassyPrelude
import Data.Serialize as Serialize
import Data.ByteString
import Data.String.Utils (rstrip) 

import IHaskell.Types

-- | A class for displayable Haskell types.
--
-- IHaskell's displaying of results behaves as if these two
-- overlapping/undecidable instances also existed:
-- 
-- > instance (Show a) => IHaskellDisplay a
-- > instance Show a where shows _ = id
class IHaskellDisplay a where
  display :: a -> [DisplayData]

-- | Generate a plain text display.
plain :: String -> DisplayData
plain = Display PlainText . rstrip

-- | Generate an HTML display.
html :: String -> DisplayData
html = Display MimeHtml

png :: String -> DisplayData
png = Display MimePng 

jpg :: String -> DisplayData
jpg = Display MimeJpg

svg :: String -> DisplayData
svg = Display MimeSvg

latex :: String -> DisplayData
latex = Display MimeLatex

serializeDisplay :: [DisplayData] -> ByteString
serializeDisplay = Serialize.encode
